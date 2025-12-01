rm(list = ls())
library(eplusr)
library(here)
library(dplyr)
source("R/setup_model.R")
source("R/measures_overhang.R")
source("R/measures_coating.R")
source("R/measures_greenroof.R")
source("R/postprocess_energy.R")

idf_path <- here("data", "idf", "Blk7.idf")
epw_path <- here("data", "epw", "SGP_Developed_Site(Blk7).epw")

model <- load_model(idf_path) |> set_ouput_meters()
epw <- load_weather(epw_path)

geo <- model$geometry()
az <- geo$azimuth() |> 
  filter(type == "Window")

# Create overhangs on facade(s) with depth 0.5
#model <- create_overhangs(model, az, depth = 0.5) 

# Change coating to 0.2
model$Material$`M15 150mm heavyweight concrete`$`Solar Absorptance` <- 0.2

# Add green roof with first set of metrics
model <- create_roof_vegetation(model,
                                lai = 3,
                                height = .15)

# Save and run model
model$save(here("data", "idf", "model_preprocessed.idf"), overwrite = TRUE)
job <- model$run(epw, dir = tempdir())

report <- job$report_data()
report_weekday <- filter_weekdays(report)
energies <- summarise_meters(report_weekday, cop = 3)
grass <- energies$e_ac |> 
  mutate(case = "grass")

# Change green roof to sparse trees
model$`Material:RoofVegetation`$green_roof$`Height of Plants` <- 1
model$`Material:RoofVegetation`$green_roof$`Leaf Area Index` <- 1.5

# Run simulation again
model$save(here("data", "idf", "model_preprocessed.idf"), overwrite = TRUE)
job <- model$run(epw, dir = tempdir())

report <- job$report_data()
report_weekday <- filter_weekdays(report)
energies <- summarise_meters(report_weekday, cop = 3)
shrubs <- energies$e_ac |> 
  mutate(case = "shrubs")

results <- bind_rows(grass, shrubs)

write.csv(
  results,
  here("data", "results", "greenroof_0.2coating_blk7")
)
