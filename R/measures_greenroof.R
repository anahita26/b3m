create_roof_vegetation <- function(model,
                                   mat_name = "green_roof",
                                   constr_name = "Green Roof",
                                   lai = 1.0,
                                   height = 0.1) {
  
  # 1. Add Material:RoofVegetation
  veg_mat <- list(
    "Material:RoofVegetation" = list(
      Name = mat_name,
      "Height of Plants" = height,
      "Leaf Area Index" = lai,
      "Leaf Reflectivity" = 0.2,
      "Leaf Emissivity" = 0.95,
      "Minimum Stomatal Resistance" = 180,
      "Soil Layer Name" = "",
      "Roughness" = "MediumRough",
      "Thickness" = 0.1,
      "Conductivity of Dry Soil" = 0.35,
      "Density of Dry Soil" = 1100,
      "Specific Heat of Dry Soil" = 1200,
      "Thermal Absorptance" = 0.9,
      "Solar Absorptance" = 0.7,
      "Visible Absorptance" = 0.75,
      "Saturation Volumetric Moisture Content of the Soil Layer" = 0.3,
      "Residual Volumetric Moisture Content of the Soil Layer" = 0.01,
      "Initial Volumetric Moisture Content of the Soil Layer" = 0.1,
      "Moisture Diffusion Calculation Method" = "Simple"
    )
  )
  
  model$add(veg_mat)
  
  # 2. Add Construction object
  constr <- list(
    "Construction" = list(
      Name = constr_name,
      "Outside Layer" = mat_name,
      "Layer 2" = "Ferrocement",
      "Layer 3" = "F05 Ceiling air space resistance",
      "Layer 4" = "Roof concrete",
      "Layer 5" = "Cement plaster"
    )
  )
  model$add(constr)
  
  # 3. Find all roof surfaces
  roof_tbl <- model$to_table(class = "BuildingSurface:Detailed")
  roof_tbl <- roof_tbl |> 
    mutate(value = case_when(
      value == "Exterior Roof" ~ "Green Roof",
      TRUE ~ value
    ))
  
  model$update(roof_tbl)
  model
}