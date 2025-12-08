rm(list = ls())
library(eplusr)
library(here)
source("R/setup_model.R")
source("R/measures_overhang.R")
source("R/postprocess_energy.R")

idf_path <- here("data", "idf", "Blk2.idf")
epw_path <- here("data", "epw", "SGP_Developed_Site(Blk7).epw")

model <- load_model(idf_path) |> set_ouput_meters()
epw <- load_weather(epw_path)

geo <- model$geometry()
az <- geo$azimuth() |> 
  filter(type == "Window")
north_windows <- az |> 
  filter(azimuth < 45 | azimuth >= 315)
south_windows <- az |> 
  filter(azimuth >= 135 & azimuth < 225)

# Create placeholder overhangs on facade(s) of choice 
model <- create_overhangs(model, az, depth = 0) #change facade here
model$save(here("data", "idf", "model_preprocessed.idf"), overwrite = TRUE)
param <- param_job(model, epw)

# Shading depths and names
shading_vals <- c(0.25, 0.5, 0.75, 1)
shading_names <- c("1/4 Shade", "Half Shade", "3/4 Shade", "Full Shade")

ecm_shade <- function(model, shading) set_shading(model, shading)

param$apply_measure(ecm_shade,
                    shading = shading_vals,
                    .names  = shading_names)

param$run()

report <- param$report_data()
report_weekday <- filter_weekdays(report)
energies <- summarise_meters(report_weekday, cop = 3)

#ac_own  <- c(1, 0.83, 0.88, 0.97, 0.97, 0.86, 0.91)
#n_occ   <- c(95, 94, 96, 94, 94, 95, 120)
#n_flats <- c(99, 99, 99, 99, 99, 99, 120)

#E_total_blk7 <- compute_E_total_blk(
#energies$e_light[2], energies$e_ac[2], energies$e_plug[2],
#ac_own, n_occ, n_flats, blk = 7
#)

#write.csv(
#tibble(E_total_blk7 = E_total_blk7),
#here("data", "results", "shading_north_total_blk7")
#)

write.csv(
  tibble(E_AC_blk2 = energies$e_ac),
  here("data", "results", "shading_both_blk2")
)
#check <- read.csv(
#here("data", "results", "shading_cooling_blk7")
#)
