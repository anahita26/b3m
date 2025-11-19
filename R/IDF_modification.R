## The following R script makes all necessary modifications to the original IDF file to prepare it for analysis
# Clear env: rm(list = ls())
# Load relevant libraries
library(eplusr)
library(here)
library(dplyr)

# Load IDF
use_eplus("C:/EnergyPlusV9-3-0")
path_idf <- here("data", "idf", "Blk7_V930.idf")
model <- read_idf(path_idf)

# Remove all output variables and meters
model$Output_Variable <- NULL
model$Output_Meter <- NULL

# Add desired output variables
output_list <- list(
  Output_Variable = list(
    key_value = "*",
    Variable_Name = "Zone Ideal Loads Supply Air Sensible Cooling Energy",
    Reporting_Frequency = "Monthly"
  ),
  Output_Variable = list(
    key_value = "*",
    Variable_Name = "Zone Ideal Loads Supply Air Total Cooling Energy",
    Reporting_Frequency = "Monthly"
  ),
  Output_Variable = list(
    key_value = "*",
    Variable_Name = "Surface Outside Face Temperature",
    Reporting_Frequency = "Monthly"
  ),
  Output_Variable = list(
    key_value = "*",
    Variable_Name = "Surface Inside Face Temperature",
    Reporting_Frequency = "Monthly"
  ),
  Output_Variable = list(
    key_value = "*",
    Variable_Name = "Surface Outside Face Conduction Heat Gain Rate",
    Reporting_Frequency = "Monthly"
  ),
  Output_Variable = list(
    key_value = "*",
    Variable_Name = "Surface Outside Face Incident Solar Radiation Rate per Area",
    Reporting_Frequency = "Monthly"
  ),
  Output_Variable = list(
    key_value = "*",
    Variable_Name = "Surface Outside Face Absorbed Shortwave Radiation Rate",
    Reporting_Frequency = "Monthly"
  ),
  Output_Meter = list(
    key_name = "Cooling:Electricity",
    Reporting_Frequency = "Monthly"
  ),
  Output_Meter = list(
    key_name = "InteriorLights:Electricity",
    Reporting_Frequency = "Monthly"
  ),
  Output_Meter = list(
    key_name = "InteriorEquipment:Electricity",
    Reporting_Frequency = "Monthly"
  )
)
output_list <- list(
  Output_Meter = list(
    key_name = "Cooling:EnergyTransfer",
    Reporting_Frequency = "Monthly"
  ),
  Output_Meter = list(
    key_name = "InteriorLights:Electricity",
    Reporting_Frequency = "Monthly"
  ),
  Output_Meter = list(
    key_name = "InteriorEquipment:Electricity",
    Reporting_Frequency = "Monthly"
  )
)
model$add(output_list)

# Update the surface details to include the roof vegetation
model$'Material:RoofVegetation'
roof_tbl <- model$to_table(class = "BuildingSurface:Detailed")
roof_tbl <- roof_tbl |> 
  mutate(value = case_when(
    value == "Exterior Roof" ~ "Green Roof",
    TRUE ~ value
  ))
model$update(roof_tbl)

# Add overhang shading to each window 
windows <- model$to_table(class = "FenestrationSurface:Detailed")
windows <- windows |> 
  filter(value == "Window", field =="Surface Type") |> 
  group_by(name) 

#|> 
  #summarise(window_name = first(name))

create_overhangs <- function(model, windows, depth = 0) {
  for (i in seq_len(nrow(windows))) {
    win_name <- windows$name[i]
    new_mat <- list(
      'Shading:Overhang:Projection' = list(
        Name = paste0("Overhang_", win_name),
        'Window or Door Name' = win_name,
        'Height above Window or Door' = 0.1,
        'Tilt Angle from Window/Door' = 0,
        'Left extension from Window/Door Width' = 0,
        'Right extension from Window/Door Width' = 0,
        'Depth as Fraction of Window/Door Height' = depth
      ))
    model$add(new_mat)
  }
  return(model)
}

model <- create_overhangs(model, windows, depth = 0)
model$"Shading:Overhang:Projection"

# Change simulation period to one year
#model$RunPeriod$June2015$`Begin Month` <- 6
#model$RunPeriod$June2015$`End Month` <- 6
#model$RunPeriod$June2015$`End Day of Month` <- 30

# Save updated model
model$save(here("data", "idf", "model_preprocessed.idf"), overwrite = TRUE)

# Check updated were made
model$`Output:Variable`
