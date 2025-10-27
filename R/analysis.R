#This doc is a first try at running a parametric analysis in R

library(eplusr)
library(here)
library(tidyverse)
library(dplyr)

#Assign idf and weather file
use_eplus("C:/EnergyPlusV9-3-0")
path_idf <- here("data", "idf", "Blk7_V930.idf")
idf <- read_idf(path_idf)
path_epw <- here("data", "epw", "SGP_Developed_Site(Blk7).epw")
epw <- read_epw(path_epw)

#Call parametric tool
param <- param_job(idf, epw)

#Assign scenario variables 
shading <- c(0, 0.8, 1.5)
absorptance <- c(0.7, 0.4, 0.2)
lai <- c(0, 2.5, 5)

#Create measure for modifying shading
set_shading <- function(idf, shading = NA) {
  if (is.na(shading)) return(idf)
  shade_tbl <- idf$to_table("Shading:Overhang:Projection")
  shade_tbl <- shade_tbl |> 
    mutate(value = ifelse(field == "Height above Window or Door",
                        sprintf("%0.2f", shading),
                        value))
  idf$update(shade_tbl)
  idf
}