remotes::install_github("besibo/Deep_CCR")

library(tidyverse) ; library(DeepDiveR)
source("R/Create_ZHL16_C.R")

# Dive parameters
max_depth <- 98
bottom_time <- 17
speed_desc <- 30
speed_asc <- 10
gradient_low <- 0.20
gradient_high <- 0.75

ppO2_min <- 0.18
ppO2_max <- 1.61
ppO2_low <- 0.7
ppO2_high <- 1.3
ppO2_switch_depth <- 20

diluent <- c(10, 55)
penalty <- 3
steps <- 0.5


# Composition of the loop mix at the maximum depth
# loop_mix(diluent, ppO2_high, max_depth)

# Create a table where each row is a segment of the dive






# # ------------------------------------------------------------------------------------
# 
# # 1. Create a full list of gases
# colnames(Gas_list) <- c("O2", "He")
# Gas_list <- as.tibble(Gas_list) %>%
#   mutate(N2 = 100 - O2 - He) %>%
#   relocate(O2, N2, He) %>%
#   arrange(O2)
# 
# # 2. Compute Minimum and Maximum Operating Depths for each gaz
# Gas_list <- Gas_list %>%
#   mutate(Min_OD = Operating_depth(O2, PO2_min, "top"),
#          Max_OD = Operating_depth(O2, PO2_max, "bottom"))
# 
# # 3. Create a tibble containing gases to use during the descent
# Desc_gas <- Create_desc_gas_tbl(Gas_list = Gas_list, Max_depth = Max_depth) %>%
#   mutate(Type = "Descent")
# 
# # 4. Create a tibble containing the gas to use as the bottom of the dive
# Bottom_gas <- Create_bottom_gas_tbl(Desc_gas)  %>%
#   mutate(Type = "Bottom")
# 
# # 5. Create a tibble containing the gas to use during the ascent
# Ascent_gas <- Create_ascent_gas_tbl(Gas_list = Gas_list, Max_depth = Max_depth) %>%
#   mutate(Type = "Ascent")
# 
# # 6. Concatenate all 3 tables, compute PO2s and EADs, and add empty list columns for loadings
# All_segments <- bind_rows(Desc_gas, Bottom_gas, Ascent_gas) %>%
#   mutate(EAD_min = EAD(N2, Depth = Start_depth),
#          EAD_max = EAD(N2, Depth = End_depth),
#          PO2_start = (O2 / 100) * (Start_depth/10 + 1),
#          PO2_end   = (O2 / 100) * (End_depth/10 + 1)) %>%
#   mutate(Time_start = NA,
#          Time_end = NA) %>%
#   mutate(N2_Load_Start = list(rep(0,16)),
#          N2_Load_End = list(rep(0,16)),
#          He_Load_Start = list(rep(0,16)),
#          He_Load_End = list(rep(0,16)),
#          M_val = list(rep(0,16)),
#          Tension = list(rep(0,16)),
#          PC_Gradient = list(rep(0,16)))
# 
# # 7. Compute times for the descent and bottom segments
# Desc_segments <- All_segments %>%
#   filter(Type %in% c("Descent", "Bottom"))
# 
# ## Computations for the first segment
# Desc_segments$Time_start[1] <- 0
# 
# Depth_drop <- Desc_segments %>%
#   transmute(Drop = End_depth - Start_depth) %>%
#   mutate(Duration = Drop / Speed_desc)
# 
# Desc_segments$Time_end[1] <- Desc_segments$Time_start[1] + Depth_drop$Duration[1]
# 
# ## Computations for all other segments
# for (i in 2:nrow(Desc_segments)) {
#   Desc_segments$Time_start[i] <- Desc_segments$Time_end[i-1]
#   Desc_segments$Time_end[i] <- Desc_segments$Time_start[i] + Depth_drop$Duration[i]
# }
# 
# ## Adjusting the last value (bottom time)
# Desc_segments$Time_end[nrow(Desc_segments)] <- Bottom_time
# 
# ## Add a "Duration" variable
# Desc_segments <- Desc_segments %>%
#   mutate(Duration = Time_end - Time_start)
# 
# 
# # 8. Compute tissue loadings for both N2 and He
# ## Manually compute the first segment
# Quot	<- c(0.627,0.567,0.493)
# P_H2O	<- Quot[Penalty]
# Desc_segments$N2_Load_Start[[1]] <- rep((10 - P_H2O)*0.795, 16)
# tmp <- Tissue_loadings(Desc_segments[1,], Penalty = Penalty)
# Desc_segments$N2_Load_End[[1]] <- tmp[[1]]
# Desc_segments$He_Load_End[[1]] <- tmp[[2]]
# 
# ## Compute loadings for all other segments
# for (i in 2:nrow(Desc_segments)) {
#   Desc_segments$N2_Load_Start[[i]] <- Desc_segments$N2_Load_End[[i-1]]
#   Desc_segments$He_Load_Start[[i]] <- Desc_segments$He_Load_End[[i-1]]
#   tmp <- Tissue_loadings(Desc_segments[i,], Penalty = Penalty)
#   Desc_segments$N2_Load_End[[i]] <- tmp[[1]]
#   Desc_segments$He_Load_End[[i]] <- tmp[[2]]
# }
# 
# 
# # 9. Determine where the maximum depth for the decompression zone
# for (i in 1:nrow(Desc_segments)) {
#   
#   Desc_segments$M_val[[i]] <- M_value(Desc_segments[i,]$N2_Load_End, Desc_segments[i,]$He_Load_End, Desc_segments[i,]$End_depth)
#   
# }
# 
