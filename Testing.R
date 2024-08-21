# 0. Installing and loading packages and datasets ------------------------------

remotes::install_github("besibo/DeepCCR")
library(DeepCCR)
library(tidyverse)


# 1. Dive parameters -----------------------------------------------------------

# Set for each dive
max_depth <- 50
bottom_time <- 40
diluent <- c(21, 00)  # Percent O2 and He in the diluent

# Check for each dive
gradient_low <- 0.85
gradient_high <- 0.90

ppO2_low <- 0.7
ppO2_high <- 1.3
ppO2_switch_depth <- 15
last_stop <- 4

# Almost never change
speed_desc <- 20
speed_asc <- 10

ppO2_min <- 0.18
ppO2_max <- 1.61

# Never ever change, unless you have a very good reason !
penalty <- 3
quot	<- c(0.627,0.567,0.493)  # Do not change these values!
p_H2O	<- quot[penalty]
steps <- 0.25


# 2. Create the dive table -----------------------------------------------------

# When diving with a CCR, the composition of the breathing gas in the loop  
# changes continually with depth. Here, we approximate this continuous change
# by dividing the dive into small segments. Each segment is defined by a start
# and end depth, and a start and end time. The composition of the breathing gas
# is assumed to be constant within each segment.

# Create a segments table and add columns to track the composition of the 
# breathing gas and to compute tissue loadings, tensions, M-values and 
# percent gradients.
dive_tbl <- create_dive_segments(max_depth, bottom_time, 
                                 speed_desc, speed_asc, last_stop) |>
  mutate(duration = time_end - time_start,   # Duration of each segment
         # Set appropriate ppO2 for each segment
         ppO2_mix_start = if_else(phase == "descent" & depth_start < ppO2_switch_depth |
                                  phase == "ascent" & depth_start <= 3,
                                  ppO2_low, 
                                  ppO2_high),
         ppO2_mix_end = if_else(phase == "descent" & depth_end < ppO2_switch_depth |
                                phase == "ascent" & depth_end <= 3,
                                ppO2_low, 
                                ppO2_high),         # Compute ambiant pressures at the start and end of each segment
         ambiant_pressure_start = depth_start / 10 + 1,
         ambiant_pressure_end   = depth_end / 10 + 1,
         # Composition of the breathing gas at the start and end of each segment
         mix_start  = map2(ppO2_mix_start, 
                           depth_start,
                           ~ loop_mix(diluent, .x, .y)),
         mix_end    = map2(ppO2_mix_end, 
                           depth_end,   
                           ~ loop_mix(diluent, .x, .y))) |> 
  unnest_wider(c(mix_start, mix_end), names_sep = "_") |>
  # Rename columns for the compistion of the breathing gas
  rename(O2_start = mix_start_1,
         N2_start = mix_start_2,
         He_start = mix_start_3,
         O2_end   = mix_end_1,
         N2_end   = mix_end_2,
         He_end   = mix_end_3) |> 
  # Partial pressures for inert gases at the start and end of each segment
  mutate(ppN2_mix_start = N2_start * ambiant_pressure_start,
         ppHe_mix_start = He_start * ambiant_pressure_start,
         ppN2_mix_end   = N2_end * ambiant_pressure_end,
         ppHe_mix_end   = He_end * ambiant_pressure_end,
         # Compute equivalent aire depth (for nitrogen narcosis)
         EAD_start = EAD(N2_start, depth_start),
         EAD_end = EAD(N2_end, depth_end),
         # Initialize columns for tissue loadings
         N2_load_start = list(rep(0,16)),
         N2_load_end = list(rep(0,16)),
         He_load_start = list(rep(0,16)),
         He_load_end = list(rep(0,16)),
         # Initialize columns for tensions, M-values and percent gradients
         M_val = list(rep(0,16)),
         tension = list(rep(0,16)),
         percent_gradient = list(rep(0,16)))


# 3. Initialize tissue loadings for both N2 and He (first segment) -------------

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

rm(seg1, tmp)


# 4. Compute tissue loadings for all other segments ----------------------------

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


# 5. Compute M-values, tensions and percent_gradient for all segments ----------

for (i in 1:nrow(dive_tbl)) {
  dive_tbl$M_val[[i]] <- M_value(dive_tbl$N2_load_end[i], 
                                 dive_tbl$He_load_end[i], 
                                 dive_tbl$depth_end[i])
  dive_tbl$tension[[i]] <- dive_tbl$N2_load_end[[i]] + dive_tbl$He_load_end[[i]]
  dive_tbl$percent_gradient[[i]] <- percent_gradient(dive_tbl$tension[[i]], 
                                                     dive_tbl$depth_end[i], 
                                                     dive_tbl$M_val[[i]])
}

# 6. Which compartment is leading? ---------------------------------------------

dive_tbl <- dive_tbl |> 
  mutate(leading_compartment = map_int(tension, ~ which(.x == max(.x))),
         leading_tension = map_dbl(tension, max),
         max_percent_gradient = map_dbl(percent_gradient, ~max(.x)))


# 7. Where are the deco zone and first stop? -----------------------------------

start_deco_zone <- dive_tbl |> 
  mutate(deco_zone = ambiant_pressure_end * 10 - leading_tension) |> 
  filter(deco_zone < 0) |>
  first() |>
  pull(depth_start)

first_stop <- dive_tbl |> 
  filter(max_percent_gradient >= gradient_low * 100) |>
  first() |>
  pull(depth_start)


# 7. Add deco stops -----------------------------------------------------------

# Compute the gradient factors for each possible stop
tmp2 <- seq(gradient_low * 100, gradient_high * 100, 
            length.out = length(first_stop:1))
# remove the last 3 values of tmp2
tmp2 <- c(tmp2[-((length(tmp2)-(last_stop - 1)):length(tmp2))], 
          gradient_high * 100)

stops_table <- dive_tbl |> 
  filter(phase == "ascent", depth_start <= first_stop) |> 
  mutate(target_GF = tmp2,
         phase = "deco") |> 
  relocate(target_GF, max_percent_gradient, .after = phase)

first_deco <- stops_table |> 
  first()

for (i in 1:nrow(stops_table)) {
  # For each segment in the deco zone
  # as long as the true gradient is higher than the target GF,
  # We add time to the segment and recompute everything for that segment
  while (stops_table$max_percent_gradient[i] >= stops_table$target_GF[i]) {
    
    stops_table[i, ]$duration <- stops_table[i, ]$duration + steps
    
    # New loadings
    tmp <- tissue_loadings(
      N2 = stops_table$N2_start[i],
      He = stops_table$He_start[i],
      depth_start = stops_table$depth_start[i],
      depth_end   = stops_table$depth_end[i],
      N2_load_start = stops_table$N2_load_start[i],
      He_load_start = stops_table$He_load_start[i],
      duration = stops_table$duration[i],
      penalty = penalty
    )
    stops_table$N2_load_end[[i]] <- tmp[[1]]
    stops_table$He_load_end[[i]] <- tmp[[2]]
    
    # M-values
    stops_table$M_val[[i]] <- M_value(stops_table$N2_load_end[i],
                                      stops_table$He_load_end[i],
                                      stops_table$depth_end[i])
    
    # Tensions
    stops_table$tension[[i]] <- stops_table$N2_load_end[[i]] + stops_table$He_load_end[[i]]
    
    # Percent gradient
    stops_table$percent_gradient[[i]] <- percent_gradient(stops_table$tension[[i]],
                                                          stops_table$depth_end[i],
                                                          stops_table$M_val[[i]])
    
    stops_table$leading_compartment[i] <- which.max(stops_table$tension[[i]])
    stops_table$leading_tension[i] <- max(stops_table$tension[[i]])
    stops_table$max_percent_gradient[i] <- max(stops_table$percent_gradient[[i]])
  }
  
  stops_table$time_end[i] <- stops_table$time_start[i] + stops_table$duration[i]
  
  if (i < nrow(stops_table)) {
    stops_table$time_start[i+1] <- stops_table$time_end[i]
    stops_table$N2_load_start[[i+1]] <- stops_table$N2_load_end[[i]]
    stops_table$He_load_start[[i+1]] <- stops_table$He_load_end[[i]]
  }
  
    
}

dive_tbl <- dive_tbl |> 
  filter(! (phase == "ascent" & depth_start <= first_stop)) |> 
  bind_rows(stops_table)

# Adjust the length of the last stop so that the ascent from the last stop to 
# the surface is done at a normal speed, without exceeding the gradient_high.

last_tbl <- stops_table |> 
  tail(2)

# Fix the duration of the last ascent segment
last_tbl[2,]$duration <- (last_tbl[2,]$depth_start - last_tbl[2,]$depth_end) / speed_asc

# Recompute the loadings of the last ascent segment
last_tbl <- last_ascent(last_tbl, penalty)

while (last_tbl$max_percent_gradient[2] > last_tbl$target_GF[2]) {
  
last_tbl <- increase_stop_duration(last_tbl, steps, penalty)

last_tbl$time_start[2] <- last_tbl$time_end[1]
last_tbl$time_end[2] <- last_tbl$time_start[2] + last_tbl$duration[2]
last_tbl$N2_load_start[[2]] <- last_tbl$N2_load_end[[1]]
last_tbl$He_load_start[[2]] <- last_tbl$He_load_end[[1]]

last_tbl <- last_ascent(last_tbl, penalty)

}

last_tbl

# Replace last 2 rows of dive_tbl by last_tbl
dive_tbl <- dive_tbl |> 
  filter(!(phase == "deco" & depth_start <= (last_stop+1))) |> 
  bind_rows(last_tbl)

dive_tbl |> print(n = Inf)

dive_tbl |> 
  filter(phase == "deco") |>
  summarise(total_deco_time = sum(duration))






# 8. Plotting ------------------------------------------------------------------
library(scales)

# Dive profile
dive_tbl |> 
  ggplot(aes(x = time_end, y = depth_end, color = O2_end)) +
  geom_line() +
  geom_point() +
  scale_y_reverse() +
  scale_color_gradient(low = "blue", high = "red", label = percent_format()) +
  labs(title = "Dive profile",
       x = "Time (min)",
       y = "Depth (m)",
       color = "O2 (%)\nin the loop ") +
  theme_bw()

# Tissue loadings
dive_tbl |> 
  select(depth_end, time_end, N2_load_end, He_load_end) |> 
  unnest_longer(col = c(N2_load_end, He_load_end)) |> 
  mutate(tissue = rep(1:16, nrow(dive_tbl))) |>
  pivot_longer(cols = c(N2_load_end, He_load_end), names_to = "inert_gas", values_to = "load") |>
  mutate(inert_gas = factor(inert_gas, levels = c("N2_load_end", "He_load_end"), labels = c("N2", "He"))) |>
  ggplot(aes(x = time_end, y = load, color = factor(tissue))) +
  geom_line() +
  facet_wrap(~inert_gas) +
  scale_color_viridis_d(option = "A") +
  labs(title = "Compartment loadings",
       x = "Time (min)",
       y = "Loadings",
       color = "Tissue") +
  theme_bw() +
  theme(legend.position = "none")

# M-values
dive_tbl |> 
  select(depth_end, time_end, M_val) |> 
  unnest_longer(col = M_val) |> 
  mutate(tissue = rep(1:16, nrow(dive_tbl))) |>
  ggplot(aes(x = time_end, y = M_val, color = factor(tissue))) +
  geom_line() +
  geom_point(size = 0.5) +
  scale_color_viridis_d(option = "A") +
  labs(title = "M-values",
       x = "Time (min)",
       y = "M-values",
       color = "Tissue") +
  theme_bw()

# Percent gradient
dive_tbl |> 
  select(phase, depth_end, time_end, percent_gradient, target_GF) |> 
  unnest_longer(col = percent_gradient) |> 
  mutate(tissue = rep(1:16, nrow(dive_tbl))) |>
  filter(phase %in% c("ascent", "deco"), 
         percent_gradient > 0) |>
  ggplot(aes(x = depth_end, y = percent_gradient, color = factor(tissue))) +
  geom_line() +
  geom_line(aes(y = target_GF), color = "grey20", linetype = 3) +
  geom_point() +
  geom_vline(xintercept = first_stop, linetype = "dashed") +
  scale_color_viridis_d(option = "A") +
  scale_x_reverse() +
  labs(title = "Percent gradient",
       x = "Depth (m)",
       y = "Percent gradient",
       color = "Tissue") +
  theme_bw()


# 9. Extract usefull information -----------------------------------------------

# descent
# 
# ppO2_switch_descent <- dive_tbl |> filter(phase == "descent", ppO2_mix_start != ppO2_mix_end)
# 
#   
#     
#   select(phase, depth_start, depth_end, time_start, time_end, 
#          ppO2_mix_start, ppO2_mix_end, 
#          O2_start, N2_start, He_start, 
#          O2_end, N2_end, He_end, 
#          EAD_start, EAD_end, 
#          leading_compartment, leading_tension, max_percent_gradient) |>
#   filter(row_number()==1 | row_number()==n(), .by = phase) |> 
#   filter()
# 

