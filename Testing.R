# remotes::install_github("besibo/DeepCCR")
library(tidyverse) ; library(DeepCCR)

source("R/create_ZHL16_C.R")

# 1. Dive parameters
max_depth <- 35
bottom_time <- 47

speed_desc <- 30
speed_asc <- 10
gradient_low <- 0.20
gradient_high <- 0.75

ppO2_low <- 0.7
ppO2_high <- 1.3
ppO2_switch_depth <- 15
ppO2_min <- 0.18
ppO2_max <- 1.61

diluent <- c(21, 35)
penalty <- 3
quot	<- c(0.627,0.567,0.493)  # Do not change these values!
p_H2O	<- quot[penalty]

steps <- 0.5


# Create a table where each row depth, from the sruface to the max depth, one meter at a time
time_per_meter <- 1 / speed_desc

desc_tbl <- tibble(phase = "descent",
                   depth_start = 0:(max_depth-1),
                   depth_end   = 1:(max_depth)) |> 
  mutate(time_start = depth_start * (1 / speed_desc),
         time_end   = depth_end   * (1 / speed_desc))

bottom_tbl <- tibble(phase = "bottom",
                     depth_start = max_depth,
                     depth_end   = max_depth,
                     time_start = last(desc_tbl$time_end),
                     time_end   = bottom_time)

asc_tbl <- tibble(phase = "ascent",
                  depth_start = max_depth:1,
                  depth_end   = (max_depth-1): 0,
                  time_start = last(bottom_tbl$time_end) + (max_depth - depth_start) * (1 / speed_asc),
                  time_end   = last(bottom_tbl$time_end) + (max_depth - depth_end) * (1 / speed_asc))

dive_tbl <- bind_rows(desc_tbl, bottom_tbl, asc_tbl) |>
  mutate(duration = time_end - time_start,
         ppO2_mix_start = if_else(depth_start < ppO2_switch_depth, ppO2_low, ppO2_high),
         ppO2_mix_end   = if_else(depth_end < ppO2_switch_depth, ppO2_low, ppO2_high),
         ppO2_mix_start = if_else(phase == "ascent" & depth_start <= 3, ppO2_low, ppO2_high),
         ppO2_mix_end   = if_else(phase == "ascent" & depth_end <= 3, ppO2_low, ppO2_high),
         ambiant_pressure_start = depth_start / 10 + 1,
         ambiant_pressure_end   = depth_end / 10 + 1,
         mix_start  = map2(ppO2_mix_start, depth_start, ~ loop_mix(diluent, .x, .y)),
         mix_end    = map2(ppO2_mix_end, depth_end,   ~ loop_mix(diluent, .x, .y))) |> 
  unnest_wider(c(mix_start, mix_end), names_sep = "_") |>
  rename(O2_start = mix_start_1,
         N2_start = mix_start_2,
         He_start = mix_start_3,
         O2_end   = mix_end_1,
         N2_end   = mix_end_2,
         He_end   = mix_end_3) |> 
  mutate(ppN2_mix_start = N2_start * ambiant_pressure_start,
         ppHe_mix_start = He_start * ambiant_pressure_start,
         ppN2_mix_end   = N2_end * ambiant_pressure_end,
         ppHe_mix_end   = He_end * ambiant_pressure_end,
         EAD_start = EAD(N2_start, depth_start),
         EAD_end = EAD(N2_end, depth_end),
         N2_load_start = list(rep(0,16)),
         N2_load_end = list(rep(0,16)),
         He_load_start = list(rep(0,16)),
         He_load_end = list(rep(0,16)),
         M_val = list(rep(0,16)),
         tension = list(rep(0,16)),
         percent_gradient = list(rep(0,16)))


# 8. Compute tissue loadings for both N2 and He
## Manually compute the first segment
dive_tbl$N2_load_start[[1]] <- rep((10 - p_H2O) * 0.795, 16)

seg1 <- dive_tbl[1,] 
tmp <- tissue_loadings(N2 = seg1$N2_start, 
                       He = seg1$He_start, 
                       depth_start = seg1$depth_start, 
                       depth_end = seg1$depth_end, 
                       duration = seg1$duration,
                       N2_load_start = seg1$N2_load_start, 
                       He_load_start = seg1$He_load_start, 
                       penalty = penalty)

dive_tbl$N2_load_end[[1]] <- tmp[[1]]
dive_tbl$He_load_end[[1]] <- tmp[[2]]

## Compute loadings for all other segments
for (i in 2:nrow(dive_tbl)) {
  dive_tbl$N2_load_start[[i]] <- dive_tbl$N2_load_end[[i-1]]
  dive_tbl$He_load_start[[i]] <- dive_tbl$He_load_end[[i-1]]
  tmp <- tissue_loadings(N2 = dive_tbl$N2_start[i], 
                         He = dive_tbl$He_start[i], 
                         depth_start = dive_tbl$depth_start[i], 
                         depth_end   = dive_tbl$depth_end[i], 
                         N2_load_start = dive_tbl$N2_load_start[i], 
                         He_load_start = dive_tbl$He_load_start[i], 
                         duration = dive_tbl$duration[i],
                         penalty = penalty)
  dive_tbl$N2_load_end[[i]] <- tmp[[1]]
  dive_tbl$He_load_end[[i]] <- tmp[[2]]
}


# 9. Compute M-values, tensions and percent_gradient for all segments
for (i in 1:nrow(dive_tbl)) {
  dive_tbl$M_val[[i]] <- M_value(dive_tbl$N2_load_end[i], 
                                 dive_tbl$He_load_end[i], 
                                 dive_tbl$depth_end[i])
  dive_tbl$tension[[i]] <- dive_tbl$N2_load_end[[i]] + dive_tbl$He_load_end[[i]]
  dive_tbl$percent_gradient[[i]] <- percent_gradient(dive_tbl$tension[[i]], 
                                                     dive_tbl$depth_end[i], 
                                                     dive_tbl$M_val[[i]])
}


dive_tbl |> 
  mutate(leading_compartments = map_int(tension, ~ which(.x == max(.x))),
         leading_tension = map_dbl(tension, max),
         deco_zone = ambiant_pressure_end * 10 - leading_tension) |> 
  print(n = Inf)






# Various sanity checks


dive_tbl |> 
  last() |> 
  select(contains("load")) |> 
  unnest(cols = everything())


# GF_stops(a = ZHL16_C$a, 
#          b = ZHL16_C$b, 
#          ppart = cbind(dive_tbl$ppN2_mix_end, dive_tbl$ppHe_mix_end), 
#          GF = gradient_high, 
#          prof.max = max_depth) |> 
#   print()


dive_tbl |> 
  select(depth_end, time_end, He_load_end) |> 
  unnest_longer(col = He_load_end) |> 
  mutate(tissue = rep(1:16, 71)) |>
  ggplot(aes(x = time_end, y = He_load_end, color = factor(tissue))) +
  geom_line() +
  geom_point(size = 0.5)


dive_tbl |> 
  select(depth_start, time_start, M_val) |> 
  unnest_longer(col = M_val) |> 
  mutate(tissue = rep(1:16, 71)) |>
  ggplot(aes(x = time_start, y = M_val, color = factor(tissue))) +
  geom_line() +
  geom_point(size = 0.5)

dive_tbl |>
  filter(phase == "bottom") |> 
  unnest()
  