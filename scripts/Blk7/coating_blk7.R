rm(list = ls())
library(eplusr)
library(here)
source("R/setup_model.R")
source("R/measures_overhang.R")
source("R/measures_coating.R")
source("R/postprocess_energy.R")

idf_path <- here("data", "idf", "Blk7.idf")
epw_path <- here("data", "epw", "SGP_Developed_Site(Blk7).epw")

model <- load_model(idf_path) |> set_ouput_meters()
epw <- load_weather(epw_path)

geo <- model$geometry()
az <- geo$azimuth() |> 
  filter(type == "Window")
north_windows <- az |> 
  filter(azimuth == 0)
south_windows <- az |> 
  filter(azimuth == 180)

# (Optional) add overhangs on facade(s)
model <- create_overhangs(model, az, depth = 0.5) #change facade here

north_exterior <- get_facade_exterior_walls(model, azimuth_value = 0)
south_exterior <- get_facade_exterior_walls(model, azimuth_value = 180)
both_exterior <- c(north_exterior, south_exterior)

# Apply facade specific coating once to have baseline absorptance
model <- apply_facade_coating(model,
                              facade_walls = both_exterior, #change facade here
                              absorptance = 0.7) 

model$save(here("data", "idf", "model_preprocessed.idf"), overwrite = TRUE)
param <- param_job(model, epw)

# Parametric variation of solar absorptance on cool concrete
coating_vals <- c(0.7, 0.5, 0.4, 0.3, 0.2)
coating_names <- c("Dark", "Med-Dark", "Medium", "Med-Cool", "Cool")

ecm_coating <- function(model, coating) set_coating(model,coating)

param$apply_measure(ecm_coating,
                    coating = coating_vals,
                    .names = coating_names)
param$run()

report <- param$report_data()
report_weekday <- filter_weekdays(report)
energies <- summarise_meters(report_weekday, cop = 3)

write.csv(
  tibble(E_AC_blk7 = energies$e_ac),
  here("data", "results", "shading0.5_coating_both_blk7") # update csv name
)
#check <- read.csv(
  #here("data", "results", "coating_cooling_blk7")
#)
