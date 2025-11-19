library(eplusr)
library(here)
library(tidyverse)
library(lubridate)
library(tidyr)
library(dplyr)

#General flow: Import idf and epw, run sim, preprocess the idf, save it, run sim again

use_eplus("C:/EnergyPlusV9-3-0")

#Assign idf and weather file
path_idf <- here("data", "idf", "Blk7.idf")
model <- read_idf(path_idf)
path_epw <- here("data", "epw", "SGP_Developed_Site(Blk7).epw")
epw <- read_epw(path_epw)

#Verify run period is June 2015 (same as paper)
model$'RunPeriod'

job <- model$run(epw, dir = tempdir())

#Retrieving variables and meters after simulation
mdd <- job$read_mdd()
rdd <- job$read_rdd() #this shows me all possible output variables I could get from E+

#Remove all output variables and meters
model$Output_Variable <- NULL
model$Output_Meter <- NULL

#Create output list of interest
output_list <- list(
  Output_Meter = list(
    key_name = "Cooling:EnergyTransfer",
    Reporting_Frequency = "Daily"
  ),
  Output_Meter = list(
    key_name = "InteriorLights:Electricity",
    Reporting_Frequency = "Daily"
  ),
  Output_Meter = list(
    key_name = "InteriorEquipment:Electricity",
    Reporting_Frequency = "Daily"
  )
)
model$add(output_list)

# Save preprocessed model
model$save(here("data", "idf", "model_preprocessed.idf"), overwrite = TRUE)

# 2nd run through of sim
job <- model$run(epw, dir = tempdir())

# Filter results for only weekdays
report <- job$report_data()
report_clean <- report |> 
  mutate(
    datetime = ymd(datetime)
  )
report_weekday <- report_clean |> 
  filter(!(wday(datetime) %in% c(1,7)))

# Calculate energy consumption
energy_by_meter <- report_weekday |> 
  group_by(name) |> 
  summarise(e_J = sum(value), .groups = "drop") |> 
  mutate(e_kWh = e_J / 3.6e6)

# Energy consumption by lighting
e_light <- energy_by_meter |> 
  filter(name == "InteriorLights:Electricity") |> 
  pull(e_kWh)
# Energy consumption by plug loads / interior equipment
e_plugload <- energy_by_meter |> 
  filter(name == "InteriorEquipment:Electricity") |> 
  pull(e_kWh)
# Energy consumption by AC
cop <- 3 #COP defined in paper
e_ac <- energy_by_meter |> 
  filter(name == "Cooling:EnergyTransfer") |> 
  pull(e_kWh) / cop

# Define all variables in equ
# AC_ownership % per building
ac_own <- c(1, 0.83, 0.88, 0.97, 0.97, 0.86, 0.91)
# Number of occupied flats
n_occ <- c(95, 94, 96, 94, 94, 95, 120)
# Number of total flats 
n_flats <- c(99, 99, 99, 99, 99, 99, 120)


# Apply equation
e_total = (e_light + (e_ac * ac_own[7]) + e_plugload) * (n_occ[7] / n_flats[7])

all_totals <- c(48421.92, 43324.31, 45918.52, 47128.58, 48047.19, 46439.92, 52361.64)
all_totals_adjusted <- c(23388.22, 22014.88,  22895.67, 23398.05, 23182.92, 22910.57, 27000.77)

windows <- model$to_table(class = "FenestrationSurface:Detailed")
surf_table <- model$to_table(class = "BuildingSurface:Detailed")
surf_info <- surf_table |> 
  select(name, field, value) |> 
  pivot_wider(names_from = field,
              values_from = value)
surf_info_clean <- surf_info |> 
  filter(`Outside Boundary Condition` == "Outdoors",
         `Surface Type` %in% c("Wall", "Roof"))
