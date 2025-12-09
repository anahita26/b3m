rm(list = ls())
library(eplusr)
library(here)
library(tidyverse)
source("R/setup_model.R")
source("R/postprocess_energy.R")

idf_path <- here("data", "idf", "Blk2.idf")
epw_path <- here("data", "epw", "SGP_Developed_Site(Blk7).epw")

model <- load_model(idf_path) |> set_ouput_meters()
epw <- load_weather(epw_path)

model$save(here("data", "idf", "model_preprocessed.idf"), overwrite = TRUE)
job <- model$run(epw, dir = tempdir())

report <- job$report_data()
report_weekday <- filter_weekdays(report)
energies <- summarise_meters(report_weekday, cop = 3)

ac_own  <- c(1, 0.83, 0.88, 0.97, 0.97, 0.86, 0.91)
n_occ   <- c(95, 94, 96, 94, 94, 95, 120)
n_flats <- c(99, 99, 99, 99, 99, 99, 120)
total_consumption_blk2 <- compute_E_total_blk(energies$e_light[2], 
                                              energies$e_ac[2], 
                                              energies$e_plug[2],
                                              ac_own, n_occ, n_flats, blk = 2)
baseline_blk2 <- energies$e_ac

write_csv(baseline_blk2,
          here("data", "results", "blk2_baseline"))

# Get roof solar exposure
roof_vec <- model$to_table(class = "BuildingSurface:Detailed") |> 
  filter(value == "Roof") |> 
  pull(name)

roof_exposure <- report |> 
  filter(name == "Surface Outside Face Incident Solar Radiation Rate per Area",
         key_value %in% roof_vec) |> 
  summarise(mean_solar = mean(value))

write_csv(roof_exposure,
          here("data", "results", "roof_solar_blk2"))
