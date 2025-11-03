# This doc is a first try at running a parametric analysis in R
# Clear env: rm(list = ls())
# Clear console: Crtl L

library(eplusr)
library(here)
library(tidyverse)
library(dplyr)
library(data.table)                         

# Assign idf and weather file
use_eplus("C:/EnergyPlusV9-3-0")
path_idf <- here("data", "idf", "model_preprocessed.idf")
model <- read_idf(path_idf)
path_epw <- here("data", "epw", "SGP_Developed_Site(Blk7).epw")
epw <- read_epw(path_epw)
#job <- model$run(epw, dir = tempdir())

# Call parametric tool
param <- param_job(model, epw)

# Assign scenario variables 
shading <- c(0.1, 0.5, 1)
coating <- c(0.7, 0.4, 0.2)
lai <- c(1, 2.5, 5)

## Create measure for modifying shading
# Check value
depth <- model$'Shading:Overhang:Projection'$Overhang_9B19D6$`Depth as Fraction of Window/Door Height`

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

## Create measure to modify coating
# Verify the correct value 
material <- model$'Material'$'M15 150mm heavyweight concrete'$`Solar Absorptance`

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

## Create measure to modify leaf area index
# Check value
model$'Material:RoofVegetation'$'green_roof'$`Leaf Area Index`

set_lai <- function(model, lai = NA) {
  # keep original if applicable
  if (is.na(lai)) return(model) #What exactly does this mean/ what does it do / when is it applicable?
  
  # extract all Material objects properly
  lai_tbl <- model$to_table(class = "Material:RoofVegetation")
  
  # modify the leaf area index field
  lai_tbl <- lai_tbl |> 
    mutate(value = case_when(
      field == "Leaf Area Index" ~ sprintf("%.2f", lai), 
      TRUE ~ value))
  
  # update model with new leaf area index
  model$update(lai_tbl)
  
  #return modified model
  model
  
}

# Test it out
model_update <- set_coating(model, coating[2])
model_update_leaf <- set_lai(model, lai[2])
model_update$Material$`M15 150mm heavyweight concrete`$`Solar Absorptance`
model_update_leaf$'Material:RoofVegetation'$'green_roof'$`Leaf Area Index`
model_update_shade <- set_shading(model, shading[2])
model_update_shade$'Shading:Overhang:Projection'$Overhang_9B19D6$`Depth as Fraction of Window/Door Height`

## Apply all measures to model
# name the simulation cases
names <- expand.grid(
  Coating = c("Dark", "Medium", "Cool"),
  LAI = c("Sparse", "Moderate", "Lush"),
  Shading = c("No Shade", "Half Shade", "Full Shade")
) |> 
  apply(1, paste, collapse = "_")

# create simulation grid
param_grid <- expand.grid(
  coating = coating,
  lai = lai,
  shading = shading
)

# use a wrapper function
ecm <- function(model, coating, lai, shading) {
  model <- set_coating(model, coating)
  model <- set_lai(model, lai)
  model <- set_shading(model, lai)
}

param$apply_measure(ecm,
                    coating = param_grid$coating,
                    lai = param_grid$lai,
                    shading = param_grid$shading,
                    .names = names)


# Apply each measure separately for testing
#param$apply_measure(set_coating,
                    #coating,
                    #.names = c("Dark", "Medium", "Cool"))
#param$apply_measure(set_lai,
                    #lai,
                    #.names = c("Sparse", "Moderate", "Lush"))
#param$apply_measure(set_shading,
                    #shading,
                    #.names = c("No Shade", "Half Shade", "Full Shade"))

# Retrieve summary of parameter values and model names
param$cases()

## Run parametric simulations in parallel
param$run()
param$status()

## Collect results
report <-  param$report_data()

# 1) Cooling demand via Zone Ideal Loads Supply Air Total Cooling Energy
report_cooling <- report |> 
  filter(name == "Zone Ideal Loads Supply Air Total Cooling Energy")

cooling_all_cases <- numeric(length(names))
# loop through each case
for (i in seq_along(names)) {
  case_name <- names[i]
  
  # filter for particular case and sum the "value" column 
  total_value <- report_cooling |> 
    filter(case == case_name) |> 
    summarise(sum_value = sum(value, na.rm = TRUE)) |> 
    pull(sum_value)
  
  # store total value in vector
  cooling_all_cases[i] <- total_value
}

cooling_all_cases_kWh <- cooling_all_cases / 3.6e6




# Need to look deeper into results that are actually affected by the green roof to show success
param_energy <- param$tabular_data(table_name = "Site and Source Energy", wide = TRUE)[[1L]] |> 
  select(case,
         row_name,
         total_energy = `Total Energy [kWh]`,
         eui =  `Energy Per Total Building Area [kWh/m2]`,
         eui_AC = `Energy Per Conditioned Building Area [kWh/m2]`) |>
  arrange(row_name, case) |> 
  filter(!grepl("Net", row_name))

# Compute savings...to be continued
# want to create nice table that shows you your savings
base_energy <- param_energy |> 
  filter(case == "Dark") |> 
  select(row_name,
         base_total = `total_energy`,
         base_eui = `eui`)


