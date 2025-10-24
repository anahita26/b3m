library(eplusr)
library(here)
library(tidyverse)

#General flow: Import idf and epw, run sim, preprocess the idf, save it, run sim again

use_eplus("C:/EnergyPlusV9-3-0")

#Assign idf and weather file
path_idf <- here("data", "idf", "Blk7_V930.idf")
model <- read_idf(path_idf)
path_epw <- here("data", "epw", "SGP_Developed_Site(Blk7).epw")
epw <- read_epw(path_epw)

#Verify run period is June 2015 (same as paper)
model$'RunPeriod'

#1st run through of simulation without weather data (less time consuming)
job <- model$run(epw, dir = tempdir())

#Retrieving variables and meters after simulation
mdd <- job$read_mdd()
rdd <- job$read_rdd() #this shows me all possible output variables I could get from E+

#Remove all output variables and meters
model$Output_Variable <- NULL
model$Output_Meter <- NULL

#Create output list of interest
output_list <- list(
  Output_Variable = list(
    key_value = "*",
    Variable_Name = "Site Outdoor Air Drybulb Temperature",
    Reporting_Frequency = "Hourly"
  ),
  Output_Variable = list(
    key_value = "*",
    Variable_Name = "Zone Mean Air Temperature",
    Reporting_Frequency = "Hourly"
  ),
  Output_Variable = list(
    key_value = "*",
    Variable_Name = "Zone Operative Temperature",
    Reporting_Frequency = "Hourly"
  ),
  Output_Variable = list(
    key_value = "*",
    Variable_Name = "Zone Ideal Loads Supply Air Sensible Cooling Energy",
    Reporting_Frequency = "Hourly"
  ),
  Output_Variable = list(
    key_value = "*",
    Variable_Name = "Zone Ideal Loads Supply Air Sensible Cooling Rate",
    Reporting_Frequency = "Hourly"
  ),
  Output_Meter = list(
    key_name = "Electricity:Facility",
    Reporting_Frequency = "Hourly"
  )
)

model$add(output_list)

#Save preprocessed model
model$save(here("data", "idf", "model_preprocessed.idf"), overwrite = TRUE)

#2nd run through of sim
job <- model$run(epw, dir = tempdir())

#Look at results
report <- job$report_data()
unique(report$name)
table <- report |> 
  select(datetime, name, value) |> 
  pivot_wider(
    names_from = name,
    values_from = value
  )














#Another way, doesnt work
meters <- c(
  "Electricity:Facility",
  "Electricity:Building"
)
for (m in meters) {
  model <- model$add("Output:Meter",
                     list(key_name = m,
                          Reporting_Frequency = "Hourly"))
}
vars <- c(
  "Zone Mean Air Temperature",
  "Zone Operative Temperature",
  "Zone Ideal Loads Supply Air Sensible Cooling Energy",
  "Site Outdoor Air Drybulb Temperature",
  "Zone Ventilation Air Flow Rate"
)
for(v in vars) {
  model <- model$add(Output_Variable,
                     list(key_value = "*",
                          Variable_Name = v,
                          Reporting_Frequency = "Hourly"))
}

#Old way of adding, doesn't really work
meters <- list(
  key_name = c(
    "Electricity:Facility",
    "Electricity:Building"
  ),
  Reporting_Frequency = "Hourly"
)
variables <- list(
  key_value = "*",
  Variable_Name = c(
    "Zone Mean Air Temperature",
    "Zone Operative Temperature",
    "Zone Ideal Loads Supply Air Sensible Cooling Energy",
    "Site Outdoor Air Drybulb Temperature",
    "Zone Ventilation Air Flow Rate"
  ),
  Reporting_Frequency = "Hourly"
)
#Add list to model
model$add(Output_Variable := variables)
model$add(Output_Meter := meters)
model$Output_Variable
