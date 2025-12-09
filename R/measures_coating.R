set_coating <- function(model, 
                        coating = NA) {
  if (is.na(coating)) return(model)
  
  coat_tbl <- model$to_table(class = "Material") |> 
    filter(name == "Cool Concrete") |> 
    mutate(value = case_when(
        field == "Solar Absorptance" ~ sprintf("%.2f", coating),
        TRUE ~ value)
    )
  
  model$update(coat_tbl)
  model
}

# Create a facade specific coating material
create_coating_material <- function(model,
                                    new_mat = "Cool Concrete",
                                    absorptance = 0.2) {
  
  mat <- list(
    "Material" = list(
      name = new_mat,
      roughness = "MediumRough",
      thickness = 0.15,
      conductivity = 1.95,
      density = 2240,
      "Specific Heat" = 900,
      "Thermal Absorptance" = 0.9,
      "Solar Absorptance" = absorptance,
      "Visible Absorptance" = 0.7
  ))
  
  model$add(mat)
  model
}

create_roof_material <- function(model,
                                    new_mat = "Cool Concrete",
                                    absorptance = 0.2) {
  
  mat <- list(
    "Material" = list(
      name = new_mat,
      roughness = "MediumRough",
      thickness = 0.03,
      conductivity = 0.836,
      density = 1280,
      "Specific Heat" = 900,
      "Thermal Absorptance" = 0.9,
      "Solar Absorptance" = absorptance,
      "Visible Absorptance" = 0.7
    ))
  
  model$add(mat)
  model
}

# Create a facade specific construction
create_facade_construction <- function(model,
                                       new_constr = "Cool Facade",
                                       new_mat = "Cool Concrete") {
  constr <- list(
    "Construction" = list(
      Name = new_constr,
      "Outside Layer" = new_mat,
      "Layer 2" = "Polyethylene",
      "Layer 3" = "Cement plaster"
    )
  )
  
  model$add(constr)
  model
}

create_roof_construction <- function(model,
                                       new_constr = "Cool Facade",
                                       new_mat = "Cool Concrete") {
  constr <- list(
    "Construction" = list(
      Name = new_constr,
      "Outside Layer" = new_mat,
      "Layer 2" = "F05 Ceiling air space resistance",
      "Layer 3" = "Roof concrete",
      "Layer 4" = "Cement plaster"
    )
  )
  
  model$add(constr)
  model
}

# Apply construction only to selected surfaces
apply_coating_to_surfaces <- function(model,
                                      surface_names,
                                      new_constr = "Cool Facade") {
  surf_tbl <- model$to_table(class = "BuildingSurface:Detailed")
  
  surf_tbl <- surf_tbl |> 
    mutate(
      value = ifelse(
        field == "Construction Name" & name %in% surface_names,
        new_constr,
        value
      )
    )
  
  model$update(surf_tbl)
  model
}

# Combined helper for facade specific coating
apply_facade_coating <- function(model,
                                 facade_walls,
                                 absorptance = 0.2,
                                 new_mat = "Cool Concrete",
                                 new_constr = "Cool Wall") {
  model <- create_coating_material(model,
                                   new_mat = new_mat,
                                   absorptance = absorptance)
  model <- create_facade_construction(model,
                                      new_constr = new_constr,
                                      new_mat = new_mat)
  model <- apply_coating_to_surfaces(model,
                                     surface_names = facade_walls,
                                     new_constr = new_constr)
  model
}

apply_roof_coating <- function(model,
                                 facade_walls,
                                 absorptance = 0.2,
                                 new_mat = "Cool Concrete",
                                 new_constr = "Cool Facade") {
  model <- create_roof_material(model,
                                   new_mat = new_mat,
                                   absorptance = absorptance)
  model <- create_roof_construction(model,
                                      new_constr = new_constr,
                                      new_mat = new_mat)
  model <- apply_coating_to_surfaces(model,
                                     surface_names = facade_walls,
                                     new_constr = new_constr)
  model
}

get_facade_exterior_walls <- function(model, orientation) {
  
  # Extract geometry and azimuths
  geo <- model$geometry()
  az  <- geo$azimuth() |> 
    filter(type == "Wall")
  
  # Convert numeric azimuth to orientation label
  az <- az |> 
    mutate(
      orientation_class = case_when(
        (azimuth < 45 | azimuth >= 315) ~ "North",
        (azimuth >= 45  & azimuth < 135) ~ "East",
        (azimuth >= 135 & azimuth < 225) ~ "South",
        (azimuth >= 225 & azimuth < 315) ~ "West",
        TRUE ~ "Unknown"
      )
    )
  
  # Get exterior walls from IDF
  surf_tbl <- model$to_table(class = "BuildingSurface:Detailed")
  
  exterior_walls <- surf_tbl |>
    filter(field == "Outside Boundary Condition", 
           value == "Outdoors") |>
    pull(name)
  
  # Filter by orientation + exterior
  target <- az |>
    filter(orientation_class == orientation, 
           name %in% exterior_walls)
  
  return(target$name)
}

get_facade_exterior_roof <- function(model) {
  
  # Extract geometry and azimuths
  geo <- model$geometry()
  exterior_roof  <- geo$azimuth() |> 
    filter(type == "Roof")
  
  return(exterior_roof)
}



