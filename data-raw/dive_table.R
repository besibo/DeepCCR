## code to prepare `dive_table` dataset goes here

max_depth <- 45
bottom_time <- 50

dive_table <- max_depth |> 
  create_dive_segments(bottom_time) |>
  compute_mix() |> 
  initialize_tissue_loadings() |> 
  compute_tissue_loadings() |> 
  deco_data() |> 
  leading_tissue()

usethis::use_data(dive_table, overwrite = TRUE)
