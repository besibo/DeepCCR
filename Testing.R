# 0. Installing and loading packages and datasets ------------------------------

remotes::install_github("besibo/DeepCCR")
library(DeepCCR)
library(tidyverse)


# 1. Dive parameters -----------------------------------------------------------

# Set for each dive
max_depth <- 45
bottom_time <- 50
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
steps <- 0.25


# 2. Create the dive table -----------------------------------------------------

# When diving with a CCR, the composition of the breathing gas in the loop  
# changes continually with depth. Here, we approximate this continuous change
# by dividing the dive into small segments. Each segment is defined by a start
# and end depth, and a start and end time. The composition of the breathing gas
# is assumed to be constant within each segment.

# The code below does the following:
# a. Creates a segments table 
# b. Adds columns to track the composition of the breathing gas and empty
#    columns for later computations
# c. Initializes compartment loadings for both N2 and He for the first segment
# d. Computes the compartment loadings for all other segments based on the 1st
# e. Compute M-values, tensions and percent_gradient for all segments
# f. Get the leading compartment, its tension and % gradient for all segments
dive_tbl <- max_depth |> 
  create_dive_segments(bottom_time, speed_desc, speed_asc, last_stop) |>
  compute_mix(ppO2_low, ppO2_high, ppO2_switch_depth, diluent) |> 
  initialize_tissue_loadings(penalty) |> 
  compute_tissue_loadings(penalty) |> 
  deco_data() |> 
  leading_tissue()

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
  
last_tbl <- adjust_last_stop_duration(last_tbl, steps, penalty)

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

