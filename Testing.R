# 0. Installing and loading packages and datasets ------------------------------

install_github("besibo/DeepCCR")
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
last_stop <- 3

# Almost never change
speed_desc <- 20
speed_asc <- 10

# ppO2_min <- 0.18
# ppO2_max <- 1.61

# Never ever change, unless you have a very good reason !
penalty <- 3
steps <- 0.25


# 2. Create the dive table -----------------------------------------------------
dive_tbl <- dive_profile(max_depth, bottom_time, speed_desc, speed_asc, 
                         last_stop, ppO2_low, ppO2_high, ppO2_switch_depth, 
                         diluent, penalty, gradient_low, gradient_high, steps)


# 3. Print the dive segments and other usefull information ---------------------
# Dive segments 
dive_tbl |> print(n = Inf)

# Depth at which compartments start off-gasing
off_gasing_limit <- deco_zone(dive_tbl)
off_gasing_limit

# Depth of the first mandatory stop
first_stop <- first_deco_stop(dive_tbl, gradient_low)
first_stop

# Total time of the deco phase
dive_tbl |> 
  filter(phase == "deco") |>
  summarise(total_deco_time = sum(duration))


# 4. Plotting ------------------------------------------------------------------
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