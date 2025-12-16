rm(list = ls())
library(eplusr)
library(here)
source("R/setup_model.R")
source("R/measures_overhang.R")
source("R/measures_coating.R")
source("R/postprocess_energy.R")

idf_path <- here("data", "idf", "Blk5.idf")
epw_path <- here("data", "epw", "SGP_Developed_Site(Blk7).epw")

model <- load_model(idf_path) |> set_ouput_meters()
epw <- load_weather(epw_path)

# (Optional) add overhangs on facade(s)
#model <- create_overhangs(model, az, depth = 0.5) #change facade here

north_exterior <- get_facade_exterior_walls(model, "North")
south_exterior <- get_facade_exterior_walls(model, "South")
east_exterior <- get_facade_exterior_walls(model, "East")
west_exterior <- get_facade_exterior_walls(model, "West")

all_exterior <- c(north_exterior, south_exterior, east_exterior, west_exterior)
roof <- get_facade_exterior_roof(model)
roof <- roof$name

# Apply facade specific coating once to have baseline absorptance
model <- apply_facade_coating(model,
                              facade_walls = all_exterior, #change facade here
                              absorptance = 0.2) 
# Change roof coating
model <- apply_roof_coating(model,
                            facade_walls = roof,
                            absorptance = 0.2)

model$save(here("data", "idf", "model_preprocessed.idf"), overwrite = TRUE)
#param <- param_job(model, epw)

# Parametric variation of solar absorptance on cool concrete
#coating_vals <- c(0.7, 0.5, 0.4, 0.3, 0.2)
#coating_names <- c("Dark", "Med-Dark", "Medium", "Med-Cool", "Cool")

#ecm_coating <- function(model, coating) set_coating(model,coating)

#param$apply_measure(ecm_coating,
#                    coating = coating_vals,
#                    .names = coating_names)
#param$run()

#report <- param$report_data()
#report_weekday <- filter_weekdays(report)
#energies <- summarise_meters(report_weekday, cop = 3)

# Running entire building coated
job <- model$run(epw, dir = tempdir())
report <- job$report_data()
report_weekday <- filter_weekdays(report)
energies <- summarise_meters(report_weekday, cop = 3)

write.csv(
  tibble(E_AC_blk5 = energies$e_ac),
  here("data", "results", "complete_coating_blk5") # update csv name
)
