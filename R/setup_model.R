library(eplusr)
library(here)
library(dplyr)
library(lubridate)

# --- Initialize EnergyPlus version (customize this once per project)
use_eplus("C:/EnergyPlusV9-3-0")

# --- Load model
load_model <- function(idf_path) {
  if (!file.exists(idf_path)) {
    stop(paste("IDF file does not exist:", idf_path))
  }
  read_idf(idf_path)
}

# --- Load weather file
load_weather <- function(epw_path) {
  if (!file.exists(epw_path)) {
    stop(paste("EPW file does not exist:", epw_path))
  }
  read_epw(epw_path)
}

# --- Add the meters / variables you need for every run
set_ouput_meters <- function(model) {
  
  # Clear previous outputs
  model$Output_Variable <- NULL
  model$Output_Meter <- NULL
  
  # Add desired meters
  output_list <- list(
    Output_Variable = list(
      key_value = "*",
      Variable_Name = "Surface Outside Face Incident Solar Radiation Rate per Area",
      Reporting_Frequency = "Monthly"
    ),
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
  return(model)
}
