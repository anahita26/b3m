# The following document uses the report data obtained in eplus_sim.R and analyzes the data for the specified output parameters

## 1) Cooling demand via Zone Ideal Loads Supply Air Total Cooling Energy
report_cooling <- report |> 
  filter(name == "Zone Ideal Loads Supply Air Total Cooling Energy")

cooling_all_cases <- numeric(length(names))
# loop through each case
for (i in seq_along(names)) {
  case_name <- names[i]
  
  # filter for particular case and sum the "value" column 
  total_value <- report_cooling |> 
    filter(case == case_name) |> 
    summarise(sum_value = sum(value, na.rm = TRUE)) |> 
    pull(sum_value)
  
  # store total value in vector
  cooling_all_cases[i] <- total_value
}

cooling_all_cases_kWh <- cooling_all_cases / 3.6e6



## 2) Looking at surfaces
solar <- report |> 
  filter(name == "Surface Outside Face Incident Solar Radiation Rate per Area") #value in W/m2

# group by surface type 
#solar <- solar |> 
 # mutate(surface_type = case_when(
 #   grepl("^OVERHANG", key_value) ~ "Overhang",
 #   grepl("^Mir-OVERHANG", key_value) ~ "Overhang_Mirror",
 #   TRUE ~ "BuildingSurface"
 # ))

# exclude mirrored overhangs
solar_clean <- solar |> 
  filter(!grepl("^Mir-", key_value))

# filter for exterior opaque surfaces only
surf_table <- model$to_table(class = "BuildingSurface:Detailed")
surf_info <- surf_table |> 
  select(name, field, value) |> 
  pivot_wider(names_from = field,
              values_from = value)
surf_info_clean <- surf_info |> 
  filter(`Outside Boundary Condition` == "Outdoors",
         `Surface Type` %in% c("Wall", "Roof"))

# join tables
solar_joined <- solar_clean |> 
  left_join(surf_info_clean, by = c("key_value" = "name"))

solar_joined_clean <- solar_joined |> 
  mutate(
    # Re-label anything that starts with "OVERHANG"
    `Surface Type` = case_when(
      grepl("^OVERHANG", key_value, ignore.case = TRUE) ~ "Overhang",
      TRUE ~ `Surface Type`
    )
  ) |> 
  # Keep only the desired surface categories
  filter(`Surface Type` %in% c("Overhang", "Wall", "Roof"))

solar_summary <- solar_joined_clean |> 
  group_by(case, `Surface Type`) |> 
  summarise(total_incident_KWhm2 = sum(value * 3600) / 3.6e6, #total sunlight energy received on that surface
            avg_incident_Wm2 = mean(value)) #power intensity - how strong sunlight usually is










# shading effectiveness
total_incident <- solar_clean |> 
  filter(surface_type == "BuildingSurface") |> 
  group_by(case) |> 
  summarise(total_incident_KWhm2 = sum(value * 3600) / 3.6e6)

overhang_energy <- solar_clean |> 
  filter(surface_type == "Overhang") |> 
  summarise(total_overhang_KWhm2 = sum(value * 3600) / 3.6e6)

shading_effectiveness <- overhang_energy$total_overhang_KWhm2 / total_incident$total_incident_KWhm2






# Need to look deeper into results that are actually affected by the green roof to show success
param_energy <- param$tabular_data(table_name = "Site and Source Energy", wide = TRUE)[[1L]] |> 
  select(case,
         row_name,
         total_energy = `Total Energy [kWh]`,
         eui =  `Energy Per Total Building Area [kWh/m2]`,
         eui_AC = `Energy Per Conditioned Building Area [kWh/m2]`) |>
  arrange(row_name, case) |> 
  filter(!grepl("Net", row_name))

# Compute savings...to be continued
# want to create nice table that shows you your savings
base_energy <- param_energy |> 
  filter(case == "Dark") |> 
  select(row_name,
         base_total = `total_energy`,
         base_eui = `eui`)


