# Import libraries
library(eplusr)
library(here)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)

use_eplus("C:/EnergyPlusV9-3-0")

#Assign variables
path_idf <- here("data", "idf", "Blk7_V930.idf")
model <- read_idf(path_idf)
path_epw <- here("data", "epw", "SGP_Developed_Site(Blk7)_2030s.epw")
epw <- read_epw(path_epw)

#Run simulation
job <- model$run(weather = epw)

#Summarize results
report <- job$tabular_data()
colnames(report)

#Weather data
class(epw$data())
head(epw$data())
weather_data <- epw$data() |> 
  select(datetime, dry_bulb_temperature) |> 
  mutate(month = month(datetime, label = TRUE),
         day = day(datetime),
         wday = wday(datetime, label = TRUE),
         hour = hour(datetime))

#Understanding the idf
class_groups <- model$group_name() #identifies all class groups
class_list <- model$class_name(by_group = TRUE) #identifies all classes under the group
model$object_name("Material") #shows you what is listed under the Material class
model$'Material' #shows more detail about all the objects under the Material class
model$'Material'$'Roof concrete' #look at specific object
model$'Material'$'Roof concrete'$'Roughness' #extract specific data
model$object_relation("Roof concrete") #idenity what objects are related to each other
model$ZoneList #view all zones in model

#Retrieving variables and meters after simulation
mdd <- job$read_mdd()
rdd <- job$read_rdd()

#Playing around with visuals
#Plot1
ggplot(weather_data, aes(x = datetime,
                         y = dry_bulb_temperature)) +
  geom_line(color = "#FB6A4A") +
  xlab("Time (Hours)") +
  ylab(expression("Dry bulb temperature " (degree*C)))

#Plot2
ggplot(weather_data, aes(x = hour,
                         y = dry_bulb_temperature)) +
  geom_point(color = "#FCBBA1", alpha = 0.7, size = 0.5) +
  geom_smooth(color = "#EF3B2C") +
  facet_grid(cols = vars(month)) +
  xlab("Hour of the day") +
  ylab(expression("Dry bulb temperature " (degree*C)))
