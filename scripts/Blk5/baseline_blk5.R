rm(list = ls())
library(eplusr)
library(here)
source("R/setup_model.R")
source("R/postprocess_energy.R")

idf_path <- here("data", "idf", "Blk5.idf")
epw_path <- here("data", "epw", "SGP_Developed_Site(SurBlks).epw")

model <- load_model(idf_path) |> set_ouput_meters()
epw <- load_weather(epw_path)

model$save(here("data", "idf", "model_preprocessed.idf"), overwrite = TRUE)
job <- model$run(epw, dir = tempdir())

report <- job$report_data()
report_weekday <- filter_weekdays(report)
energies <- summarise_meters(report_weekday, cop = 3)

baseline_blk5 <- energies$e_ac

write_csv(baseline_blk5,
          here("data", "results", "blk5_baseline"))
