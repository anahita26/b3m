#This doc is a first try at running a parametric analysis in R
#Clear env: rm(list = ls())
#Clear console: Crtl L

library(eplusr)
library(here)
library(tidyverse)
library(dplyr)
library(data.table)

#Assign idf and weather file
use_eplus("C:/EnergyPlusV9-3-0")
path_idf <- here("data", "idf", "Blk7_V930.idf")
model <- read_idf(path_idf)
path_epw <- here("data", "epw", "SGP_Developed_Site(Blk7).epw")
epw <- read_epw(path_epw)
job <- idf$run(epw, dir = tempdir())

#Call parametric tool
param <- param_job(idf, epw)

#Assign scenario variables 
shading <- c(0, 0.8, 1.5)
coating <- c(0.7, 0.4, 0.2)
lai <- c(0, 2.5, 5)

# Create measure for modifying shading
# List parent walls
surf_geo <- model$geometry()
ext_walls <- surf_geo$azimuth() |> 
  filter(class == "BuildingSurface:Detailed" & 
           type == "Wall")
# Filter for only outdoor walls
# Not sure how to do this yet

set_shading <- function(model, shading = NA) {
  if (is.na(shading)) return(mdoel)
  shade_tbl <- model$to_table("Shading:Overhang:Projection")
  shade_tbl <- shade_tbl |> 
    mutate(value = ifelse(field == "Height above Window or Door",
                        sprintf("%0.2f", shading),
                        value))
  model$update(shade_tbl)
  model
}

# Create measure to modify coating
# Extract the correct value
material <- model$'Material'$'M15 150mm heavyweight concrete'$`Solar Absorptance`

set_coating <- function(model, coating = NA) {
  # keep original if applicable 
  #if (is.na(coating)) return(model)
  
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
  
  # update absorptance using table
  model$update(coat_tbl)
  
  #return modified model
  model
}

# Test it out
model_update <- set_coating(model, coating[2])
model_update$Material$`M15 150mm heavyweight concrete`$`Solar Absorptance`+

