compute_ac_exposure <- function(model) {
  
  surf_tbl <- model$to_table(class = "BuildingSurface:Detailed")
  surf_wide <- surf_tbl |>
    select(name, field, value) |>
    pivot_wider(names_from = field, values_from = value)
  
  geo <- model$geometry()
  area <- geo$area() |> 
    select(name, area)
  
  surf_full <- surf_wide |> 
    left_join(area,
              by = "name")
  surf_full <- surf_full |> 
    relocate(area, .after = name)
  
  is_ac_zone <- grepl("_AC", surf_wide$`Zone Name`, ignore.case = TRUE)
  
  roof_ac <- surf_full |>
    filter(`Surface Type` == "Roof",
           `Outside Boundary Condition` == "Outdoors",
           is_ac_zone) |>
    summarise(area = sum(as.numeric(area))) |> pull(area)
  
  roof_total <- surf_full |>
    filter(`Surface Type` == "Roof",
           `Outside Boundary Condition` == "Outdoors") |>
    summarise(area = sum(as.numeric(area))) |> pull(area)
  
  wall_ac <- surf_full |>
    filter(`Surface Type` == "Wall",
           `Outside Boundary Condition` == "Outdoors",
           is_ac_zone) |>
    summarise(area = sum(as.numeric(area))) |> pull(area)
  
  floor_ac <- surf_full |>
    filter(`Surface Type` == "Floor",
           is_ac_zone) |>
    summarise(area = sum(as.numeric(area))) |> pull(area)
  
  env_ac <- roof_ac + wall_ac
  
  tibble(
    roof_ac_area = roof_ac,
    roof_total_area = roof_total,
    wall_ac_area = wall_ac,
    floor_ac_area = floor_ac,
    env_ac_area = env_ac,
    ratio_env_to_floor = env_ac / floor_ac,
    ratio_roof_ac = roof_ac / roof_total
  )
}

