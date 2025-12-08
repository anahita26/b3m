rm(list = ls())
library(eplusr)
library(here)
library(tidyverse)
source("R/setup_model.R")
source("R/postprocess_energy.R")

idf_path <- here("data", "idf", "Blk7.idf")
epw_path <- here("data", "epw", "SGP_Developed_Site(Blk7).epw")

model <- load_model(idf_path) |> set_ouput_meters()
epw <- load_weather(epw_path)

model$save(here("data", "idf", "model_preprocessed.idf"), overwrite = TRUE)
job <- model$run(epw, dir = tempdir())

report <- job$report_data()
report_weekday <- filter_weekdays(report)
energies <- summarise_meters(report_weekday, cop = 3)

baseline_blk7 <- energies$e_ac

write_csv(baseline_blk7,
          here("data", "results", "blk7_baseline"))

# Get roof solar exposure
roof_vec <- model$to_table(class = "BuildingSurface:Detailed") |> 
  filter(value == "Roof") |> 
  pull(name)

roof_exposure <- report |> 
  filter(name == "Surface Outside Face Incident Solar Radiation Rate per Area",
         key_value %in% roof_vec) |> 
  summarise(mean_solar = mean(value))

write_csv(roof_exposure,
          here("data", "results", "roof_solar_blk7"))
