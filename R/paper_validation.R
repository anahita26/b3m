library(eplusr)
library(here)
library(tidyverse)
library(lubridate)
library(tidyr)
library(dplyr)

# Clear env: rm(list = ls())

#General flow
# 1. Import model and edit output meters, save
# 2. Get azimuth angles and assign directions to facades
# 3. Create overhangs on only one facade, save
# 4. Run simulation for different overhang depths

use_eplus("C:/EnergyPlusV9-3-0")

#Assign idf and weather file
path_idf <- here("data", "idf", "Blk7.idf")
model <- read_idf(path_idf)
path_epw <- here("data", "epw", "SGP_Developed_Site(Blk7).epw")
epw <- read_epw(path_epw)

#job <- model$run(epw, dir = tempdir())

#Retrieving variables and meters after simulation
#mdd <- job$read_mdd()
#rdd <- job$read_rdd() #this shows me all possible output variables I could get from E+

#Remove all output variables and meters
model$Output_Variable <- NULL
model$Output_Meter <- NULL

#Create output list of interest
output_list <- list(
  Output_Meter = list(
    key_name = "Cooling:EnergyTransfer",
    Reporting_Frequency = "Daily"
  ),
  Output_Meter = list(
    key_name = "InteriorLights:Electricity",
    Reporting_Frequency = "Daily"
  ),
  Output_Meter = list(
    key_name = "InteriorEquipment:Electricity",
    Reporting_Frequency = "Daily"
  )
)
model$add(output_list)

# Save preprocessed model
model$save(here("data", "idf", "model_preprocessed.idf"), overwrite = TRUE)

# 2nd run through of sim
#job <- model$run(epw, dir = tempdir())

# Filter results for only weekdays
#report <- job$report_data()

# get azimuth angles 
geo <- model$geometry()
az <- geo$azimuth() |> 
  filter(type == "Window")
north <- az |> 
  filter(azimuth == 0)
south <- az |> 
  filter(azimuth == 180)

# Function to create standin for overhang depth
create_overhangs <- function(model, north, depth = 0) {
  for (i in seq_len(nrow(north))) {
    win_name <- north$name[i]
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

# Apply shading to only north facing facade
model <- create_overhangs(model, north, depth = 0.5)
model$"Shading:Overhang:Projection" #check if added correctly

# Save model
model$save(here("data", "idf", "model_preprocessed.idf"), overwrite = TRUE)
# Call parametric tool
param <- param_job(model, epw)

# Function to change overhang depth value
set_shading <- function(model, shading = NA) {
  # keep original if applicable
  if (is.na(shading)) return(model)
  
  # extract all overhang objects properly
  shade_tbl <- model$to_table(class = "Shading:Overhang:Projection")
  
  # update fraction of overhang shading
  shade_tbl <- shade_tbl |> 
    mutate(value = ifelse(field == "Depth as Fraction of Window/Door Height",
                          sprintf("%0.2f", shading),
                          value))
  
  # update model with new overhang fraction
  model$update(shade_tbl)
  
  # return modified model
  model
}

# Overhang depths to be tested
#shading <- c(0.1, 0.5, 1)
# Names for each case
names <- c("1/4 Shade", "Half Shade", "3/4 Shade", "Full Shade")
# Run simulations for varying shading depths in parallel
ecm <- function(model, shading) {
  model <- set_shading(model, shading)
}
param$apply_measure(ecm,
                    shading = c(0.25, 0.5, 0.75, 1),
                    .names = names)
param$cases() #check
param$run()

# Extract results
report <-  job$report_data()
# Extract only weekdays
report_clean <- report |> 
  mutate(
    datetime = ymd(datetime)
  )
report_weekday <- report_clean |> 
  filter(!(wday(datetime) %in% c(1,7)))

# Calculate energy consumption per meter
energy_by_meter <- report_weekday |> 
  group_by(name, case) |> 
  summarise(e_J = sum(value), .groups = "drop") |> 
  mutate(e_kWh = e_J / 3.6e6)
# Assign energy consumption by lighting
e_light <- energy_by_meter |> 
  filter(name == "InteriorLights:Electricity") |> 
  pull(e_kWh)
# Assign energy consumption by plug loads / interior equipment
e_plugload <- energy_by_meter |> 
  filter(name == "InteriorEquipment:Electricity") |> 
  pull(e_kWh)
# Assign energy consumption by AC
cop <- 3 #COP defined in paper
e_ac <- energy_by_meter |> 
  filter(name == "Cooling:EnergyTransfer") |> 
  pull(e_kWh) / cop

# Define all variables in equ
# AC_ownership % per building
ac_own <- c(1, 0.83, 0.88, 0.97, 0.97, 0.86, 0.91)
# Number of occupied flats
n_occ <- c(95, 94, 96, 94, 94, 95, 120)
# Number of total flats 
n_flats <- c(99, 99, 99, 99, 99, 99, 120)

# Apply equation
e_total = (e_light + (e_ac * ac_own[7]) + e_plugload) * (n_occ[7] / n_flats[7])

# Record results
all_totals <- c(48421.92, 43324.31, 45918.52, 47128.58, 48047.19, 46439.92, 52361.64)
all_totals_adjusted <- c(23388.22, 22014.88,  22895.67, 23398.05, 23182.92, 22910.57, 27000.77)

# Change coating on outside walls
set_coating <- function(model, coating = NA) {
  # keep original if applicable 
  if (is.na(coating)) return(model)
  
  # extract all Material objects properly
  coat_tbl <- model$to_table(class = "Material")
  
  # filter for the target material
  coat_tbl <- coat_tbl |>
    filter(name == "M15 150mm heavyweight concrete")
  
  # modify the solar absorptance field -- not sure what this code is doing exactly
  coat_tbl <- coat_tbl |> 
    mutate(value = case_when(
      field == "Solar Absorptance" ~ sprintf("%.2f", coating),
      TRUE ~ value))
  
  # update model with new absorptance
  model$update(coat_tbl)
  
  # return modified model
  model
}

names <- c("Dark", "Med-Dark", "Medium", "Med-Cool", "Cool")
# Run simulations for varying shading depths in parallel
ecm <- function(model, coating) {
  model <- set_coating(model, coating)
}
param$apply_measure(ecm,
                    coating = c(0.7, 0.5, 0.4, 0.3, 0.2),
                    .names = names)
param$cases() #check
param$run()

# Add roof vegetation 
# Function that creates green roof class
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
  
  return(model)
}

#Change shading to 0.5 (above)
#Change coating to 0.2
model$Material$`M15 150mm heavyweight concrete`$`Solar Absorptance` <- 0.2
#Chang green roof to grass
model <- create_roof_vegetation(model,
                                lai = 3,
                                height = .15)
#change green roof to sparse trees
model$`Material:RoofVegetation`$green_roof$`Height of Plants` <- 1
model$`Material:RoofVegetation`$green_roof$`Leaf Area Index` <- 1.5

#Run it
job <- model$run(epw, dir = tempdir())
# reporting above

#Checking
model$`Material:RoofVegetation`
model$Construction
surf <- model$to_table(class = "BuildingSurface:Detailed")


