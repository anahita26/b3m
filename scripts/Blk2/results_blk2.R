rm(list = ls())
library(tidyverse)
library(here)
library(dplyr)

results <- bind_rows(
  read_csv(here("data", "results", "shading_north_blk2")) %>%
    mutate(scenario_type = "Shading", facade = "North"),
  
  read_csv(here("data", "results", "shading_south_blk2")) %>%
    mutate(scenario_type = "Shading", facade = "South"),
  
  read_csv(here("data", "results", "shading_both_blk2")) %>%
    mutate(scenario_type = "Shading", facade = "Both"),
  
  read_csv(here("data", "results", "coating_north_blk2")) %>%
    mutate(scenario_type = "Coating", facade = "North"),
  
  read_csv(here("data", "results", "coating_south_blk2")) %>%
    mutate(scenario_type = "Coating", facade = "South"),
  
  read_csv(here("data", "results", "coating_east_blk2")) %>%
    mutate(scenario_type = "Coating", facade = "East"),
  
  read_csv(here("data", "results", "coating_west_blk2")) %>%
    mutate(scenario_type = "Coating", facade = "West"),
  
  read_csv(here("data", "results", "coating_all_blk2")) %>%
    mutate(scenario_type = "Coating", facade = "All")
)

results <- results |> 
  rename(
    case = E_AC_blk2.case,
    energy_ac = E_AC_blk2.e_ac,
    case_id = ...1
  )

results$case_id <- 1:nrow(results)
results <- results |> 
  select(case_id, scenario_type, facade, case, energy_ac)

write_csv(results,
          here("data", "results", "all_scenarios_blk2"))

results_greenroof <- bind_rows(
  read_csv(here("data", "results", "greenroof_cooling_blk2")) |> 
    mutate(scenario_type = "Green Roof"),
  
  read_csv(here("data", "results", "greenroof_0.2coating_blk2")) |> 
    mutate(scenario_type = "Coating + Green Roof")
)

results_greenroof <- results_greenroof |> 
  select(scenario_type, case, e_ac)

#coating_value <- read_csv(here("data", "results", "all_scenarios_blk2")) |> 
  #filter(scenario_type == "Coating",
         #facade == "Both",
        #case == "Cool") |> 
 # pull(energy_ac)

#new_row <- tibble(scenario_type = "Coating",
              #    case = "Cool",
               #   e_ac = coating_value
#)

#results_greenroof <- bind_rows(results_greenroof, new_row)

#write_csv(results_greenroof,
#          here("data", "results", "greenroof_data_blk2"))

