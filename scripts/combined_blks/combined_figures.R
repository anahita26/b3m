rm(list = ls())
library(here)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(knitr)
library(gt)

# Upload data for block 7
blk7_results <- read_csv(here("data", "results", "all_scenarios_blk7"))
blk7_baseline <- read_csv( here("data", "results", "blk7_baseline"))
blk7_areas <- read_csv(here("data", "results", "blk7_surface_areas.csv"))

blk7_baseline <- blk7_baseline$e_ac
blk7_results$baseline <- blk7_baseline
blk7_results$diff <- blk7_results$baseline - blk7_results$energy_ac

area_lookup <- setNames(blk7_areas$area_m2, blk7_areas$surface)

northA <- area_lookup[["North facade"]]
southA <- area_lookup[["South facade"]]
eastA  <- area_lookup[["East facade"]]
westA  <- area_lookup[["West facade"]]
roofA  <- area_lookup[["Roof"]]
all_facadesA <- northA + southA + eastA + westA

blk7_results <- blk7_results |>
  mutate(
    facade_clean = str_to_lower(facade),
    
    area_m2 = case_when(
      str_detect(facade_clean, "north") ~ northA,
      str_detect(facade_clean, "south") ~ southA,
      str_detect(facade_clean, "east")  ~ eastA,
      str_detect(facade_clean, "west")  ~ westA,
      str_detect(facade_clean, "roof")  ~ roofA,
      
      # handle multi-facade cases (tweak keywords to match your data)
      str_detect(facade_clean, "both") ~ northA + southA,          # if "both" means N+S in your study
      str_detect(facade_clean, "all")  ~ all_facadesA,             # all vertical facades
      str_detect(facade_clean, "facades") ~ all_facadesA,
      
      TRUE ~ NA_real_
    ),
    
    kwh_per_m2 = diff / area_m2
  ) |>
  select(-facade_clean)


blk7_results_pct <- blk7_results |> 
  mutate(pct_change = 100 * (blk7_results$energy_ac - blk7_baseline) / blk7_baseline)
blk7_results_pct$block <- "Blk7"

blk7_results_greenroof <- read_csv(here("data", "results", "greenroof_data_blk7"))
blk7_results_greenroof_pct <- blk7_results_greenroof |> 
  mutate(pct_change = 100 * (blk7_results_greenroof$e_ac - blk7_baseline) / blk7_baseline)
blk7_results_greenroof_pct$block <- "Blk7"  

# Upload data for block 5
blk5_results <- read_csv(here("data", "results", "all_scenarios_blk5"))
blk5_baseline <- read_csv( here("data", "results", "blk5_baseline"))
blk5_areas <- read_csv(here("data", "results", "blk5_surface_areas.csv"))

blk5_baseline <- blk5_baseline$e_ac
blk5_results$baseline <- blk5_baseline
blk5_results$diff <- blk5_results$baseline - blk5_results$energy_ac

area_lookup <- setNames(blk5_areas$area_m2, blk5_areas$surface)

northA <- area_lookup[["North facade"]]
southA <- area_lookup[["South facade"]]
eastA  <- area_lookup[["East facade"]]
westA  <- area_lookup[["West facade"]]
roofA  <- area_lookup[["Roof"]]
all_facadesA <- northA + southA + eastA + westA

blk5_results <- blk5_results |>
  mutate(
    facade_clean = str_to_lower(facade),
    
    area_m2 = case_when(
      str_detect(facade_clean, "north") ~ northA,
      str_detect(facade_clean, "south") ~ southA,
      str_detect(facade_clean, "east")  ~ eastA,
      str_detect(facade_clean, "west")  ~ westA,
      str_detect(facade_clean, "roof")  ~ roofA,
      
      # handle multi-facade cases (tweak keywords to match your data)
      str_detect(facade_clean, "both") ~ northA + southA,          # if "both" means N+S in your study
      str_detect(facade_clean, "all")  ~ all_facadesA,             # all vertical facades
      str_detect(facade_clean, "facades") ~ all_facadesA,
      
      TRUE ~ NA_real_
    ),
    
    kwh_per_m2 = diff / area_m2
  ) |>
  select(-facade_clean)

blk5_results_pct <- blk5_results |> 
  mutate(pct_change = 100 * (blk5_results$energy_ac - blk5_baseline) / blk5_baseline)
blk5_results_pct$block <- "Blk5"

blk5_results_greenroof <- read_csv(here("data", "results", "greenroof_data_blk5"))
blk5_results_greenroof_pct <- blk5_results_greenroof |> 
  mutate(pct_change = 100 * (blk5_results_greenroof$e_ac - blk5_baseline) / blk5_baseline)
blk5_results_greenroof_pct$block <- "Blk5"  

# Upload data for block 2
blk2_results <- read_csv(here("data", "results", "all_scenarios_blk2"))
blk2_baseline <- read_csv( here("data", "results", "blk2_baseline"))
blk2_areas <- read_csv(here("data", "results", "blk2_surface_areas.csv"))

blk2_baseline <- blk2_baseline$e_ac
blk2_results$baseline <- blk2_baseline
blk2_results$diff <- blk2_results$baseline - blk2_results$energy_ac

area_lookup <- setNames(blk2_areas$area_m2, blk2_areas$surface)

northA <- area_lookup[["North facade"]]
southA <- area_lookup[["South facade"]]
eastA  <- area_lookup[["East facade"]]
westA  <- area_lookup[["West facade"]]
roofA  <- area_lookup[["Roof"]]
all_facadesA <- northA + southA + eastA + westA

blk2_results <- blk2_results |>
  mutate(
    facade_clean = str_to_lower(facade),
    
    area_m2 = case_when(
      str_detect(facade_clean, "north") ~ northA,
      str_detect(facade_clean, "south") ~ southA,
      str_detect(facade_clean, "east")  ~ eastA,
      str_detect(facade_clean, "west")  ~ westA,
      str_detect(facade_clean, "roof")  ~ roofA,
      
      # handle multi-facade cases (tweak keywords to match your data)
      str_detect(facade_clean, "both") ~ northA + southA,          # if "both" means N+S in your study
      str_detect(facade_clean, "all")  ~ all_facadesA,             # all vertical facades
      str_detect(facade_clean, "facades") ~ all_facadesA,
      
      TRUE ~ NA_real_
    ),
    
    kwh_per_m2 = diff / area_m2
  ) |>
  select(-facade_clean)

blk2_results_pct <- blk2_results |> 
  mutate(pct_change = 100 * (blk2_results$energy_ac - blk2_baseline) / blk2_baseline)
blk2_results_pct$block <- "Blk2"

blk2_results_greenroof <- read_csv(here("data", "results", "greenroof_data_blk2"))
blk2_results_greenroof_pct <- blk2_results_greenroof |> 
  mutate(pct_change = 100 * (blk2_results_greenroof$e_ac - blk2_baseline) / blk2_baseline)
blk2_results_greenroof_pct$block <- "Blk2"  

# Create data tables for graphs
all_blks_results <- bind_rows(blk7_results_pct, blk5_results_pct, blk2_results_pct)
all_blks_greenroof <- bind_rows(blk7_results_greenroof_pct, blk5_results_greenroof_pct, blk2_results_greenroof_pct)

write_csv(all_blks_results,
          here("data", "results", "all_blks_results"))
write_csv(all_blks_greenroof,
          here("data", "results", "all_blks_greenroof"))

# Cool coating on rooftop
blk7_coolroof <-  read_csv(here("data", "results", "roof_coating_blk7")) |> 
  rename(
    case = E_AC_blk7.case,
    energy_ac = E_AC_blk7.e_ac
  ) 
blk7_coolroof <- blk7_coolroof |>   
  mutate(pct_change = 100 * (blk7_coolroof$energy_ac - blk7_baseline) / blk7_baseline)
blk7_coolroof$block <- "Blk7"
blk2_coolroof <-  read_csv(here("data", "results", "roof_coating_blk2")) |> 
  rename(
    case = E_AC_blk2.case,
    energy_ac = E_AC_blk2.e_ac
  )
blk2_coolroof <- blk2_coolroof |>
  mutate(pct_change = 100 * (blk2_coolroof$energy_ac - blk2_baseline) / blk2_baseline)
blk2_coolroof$block <- "Blk2"
blk5_coolroof <-  read_csv(here("data", "results", "roof_coating_blk5")) |> 
  rename(
    case = E_AC_blk5.case,
    energy_ac = E_AC_blk5.e_ac
  )
blk5_coolroof <- blk5_coolroof |>
  mutate(pct_change = 100 * (blk5_coolroof$energy_ac - blk5_baseline) / blk5_baseline)
blk5_coolroof$block <- "Blk5"
coolroof <- bind_rows(blk2_coolroof, blk7_coolroof, blk5_coolroof)
coolroof$block <- factor(coolroof$block,
                                 levels = c("Blk2", "Blk7", "Blk5"))

blk2_roof_area <- blk2_areas |>
  filter(surface == "Roof") |>
  transmute(block = "Blk2", roof_area_m2 = area_m2)

blk5_roof_area <- blk5_areas |>
  filter(surface == "Roof") |>
  transmute(block = "Blk5", roof_area_m2 = area_m2)

blk7_roof_area <- blk7_areas |>
  filter(surface == "Roof") |>
  transmute(block = "Blk7", roof_area_m2 = area_m2)

roof_areas_all <- bind_rows(blk2_roof_area, blk5_roof_area, blk7_roof_area)

coolroof2 <- coolroof |>
  left_join(roof_areas_all, by = "block")

coolroof2 <- coolroof2 |> 
  mutate(
    baseline = case_when(
      block == "Blk7" ~ blk7_baseline,
      block == "Blk2" ~ blk2_baseline,
      block == "Blk5" ~ blk5_baseline,
      TRUE ~ NA_real_
    ),
    kwh_m2_saved = (baseline - energy_ac) / roof_area_m2
  )

write_csv(coolroof,
          here("data", "results", "coolroof"))

# Cool coating on walls and rooftop
blk7_complete_coating <-  read_csv(here("data", "results", "complete_coating_blk7")) |> 
  rename(
    case = E_AC_blk7.case,
    energy_ac = E_AC_blk7.e_ac
  ) 
blk7_complete_coating <- blk7_complete_coating |>   
  mutate(pct_change = 100 * (blk7_complete_coating$energy_ac - blk7_baseline) / blk7_baseline)
blk7_complete_coating$block <- "Blk7"
blk2_complete_coating <-  read_csv(here("data", "results", "complete_coating_blk2")) |> 
  rename(
    case = E_AC_blk2.case,
    energy_ac = E_AC_blk2.e_ac
  )
blk2_complete_coating <- blk2_complete_coating |>
  mutate(pct_change = 100 * (blk2_complete_coating$energy_ac - blk2_baseline) / blk2_baseline)
blk2_complete_coating$block <- "Blk2"
blk5_complete_coating <-  read_csv(here("data", "results", "complete_coating_blk5")) |> 
  rename(
    case = E_AC_blk5.case,
    energy_ac = E_AC_blk5.e_ac
  )
blk5_complete_coating <- blk5_complete_coating |>
  mutate(pct_change = 100 * (blk5_complete_coating$energy_ac - blk5_baseline) / blk5_baseline)
blk5_complete_coating$block <- "Blk5"

complete_coating <- bind_rows(blk2_complete_coating, blk7_complete_coating, blk5_complete_coating)
complete_coating$block <- factor(complete_coating$block,
                         levels = c("Blk2", "Blk7", "Blk5"))
write_csv(complete_coating,
          here("data", "results", "complete_coating"))


# Shading figure
all_blks_results |> 
  filter(scenario_type == "Shading") |> 
  ggplot(aes(x = block,
             y = pct_change,
             fill = case,
             color = case)) +
  geom_point(shape = 21, size = 3)+
  ylim(-.25,.25) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~facade) +
  theme_minimal()+
  labs(
    title = "Shading: Percent Change in Cooling Demand",
    x = "Block",
    y = "Δ Cooling Demand (%)"
  ) +
  scale_fill_colorblind() +
  scale_colour_colorblind()

#percent change graph
all_blks_results |> 
  filter(scenario_type == "Coating") |> 
  ggplot(aes(x = block,
             y = pct_change,
             fill = case,
             color = case)) +
  geom_point(shape = 21, size = 3)+
  #ylim(-2.5,2.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~facade) +
  theme_minimal()+
  labs(
    title = "Cool Walls: Percent Change in Cooling Demand",
    x = "Block",
    y = "Δ Cooling Demand (%)"
  ) +
  scale_fill_colorblind() +
  scale_colour_colorblind()

# kwh_m2 graph
all_blks_results |> 
  filter(scenario_type == "Coating") |> 
  ggplot(aes(x = block,
             y = kwh_per_m2,
             fill = case,
             color = case)) +
  geom_point(shape = 21, size = 3)+
  #ylim(-2.5,2.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~facade) +
  theme_minimal()+
  labs(
    x = "Block",
    y = "kWh_m2 saved"
  ) +
  scale_fill_colorblind() +
  scale_colour_colorblind()

coolroof |> 
  ggplot(aes(x = block,
             y = pct_change,
             fill = case,
             color = case)) +
  geom_point(shape = 21, size = 3) +
  ylim(-10,0) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal()+
  labs(
    title = "Cool Roof: Percent Change in Cooling Demand",
    x = "Block",
    y = "Δ Cooling Demand (%)"
  ) +
  scale_fill_colorblind() +
  scale_colour_colorblind()

#kwh_m2 graph 
coolroof2 |> 
  ggplot(aes(x = block,
             y = kwh_m2_saved,
             fill = case,
             color = case)) +
  geom_point(shape = 21, size = 3) +
  #ylim(-10,0) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal()+
  labs(
    x = "Block",
    y = "kWh_m2 saved"
  ) +
  scale_fill_colorblind() +
  scale_colour_colorblind()

# Complete coating plots
complete_coating |> 
  ggplot(aes(x = block,
             y = pct_change)) +
  geom_col(fill = "black") +
  ylim(-11,0) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal()+
  labs(
    title = "Cool Roof + Cool Walls: Percent Change in Cooling Demand",
    x = "Block",
    y = "Δ Cooling Demand (%)"
  )

# Green roof plots
all_blks_greenroof |> 
  filter(scenario_type == "Green Roof") |> 
  filter(case == "grass") |> 
  ggplot(aes(x = block,
             y = pct_change)) +
  geom_col(fill = "black") +
  ylim(-10,0) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Green Roof: Percent Change in Cooling Demand",
    x = "Block",
    y = "Δ Cooling Demand (%)"
  )


all_blks_greenroof |> 
  filter(scenario_type == "Coating + Green Roof") |> 
  filter(case == "grass") |> 
  ggplot(aes(x = block,
             y = pct_change)) +
  geom_col(fill = "black") +
  ylim(-10,0) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Green Roof + Cool Walls: Percent Change in Cooling Demand",
    x = "Block",
    y = "Δ Cooling Demand (%)"
  )

# Evaluation tables
percentages <- c("5 < P ≤ 10", "2 < P ≤ 5", "P ≤ 2")
rating <- c("High", "Moderate", "Low")
erms <- c("CR (7), CR+CW (7)", 
          "CR (2,5), CR+CW (2,5), GR+CW (2,7,5)", 
          "CW (2,7,5), GR (2,7,5), S (2,7,5)")
#col_titles <- c("Monthly Energy Saving Rate, P", "Energy Rating", "ERMs (block)")
energy_df <- tibble(
  `Monthly Energy Saving, P` = percentages,
  `Energy Rating` = rating,
  `ERMs (block)` = erms
)
energy_df |> 
  gt()|> 
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "#f0f0f0")   # light gray shading
    ),
    locations = cells_column_labels()
  )

component <- c("Windows", "Walls", "Roof", "")
existing <- c("Unshaded windows", 
              "Concrete external walls with simple paint (a = 0.7)",
              "Concrete roof with with simple paint (a = 0.7)",
              "")
retrofits <- c("Overhang shading (45 degrees)",
               "Coatings (light color paint)",
               "Coatings (light color paint)",
               "Roof vegetation")
summary_df <- tibble(
  `Building component` = component,
  `Existing construction` = existing,
  `Energy retrofit measures` = retrofits
)
summary_df |> 
  gt() |> 
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "#f0f0f0")   # light gray shading
    ),
    locations = cells_column_labels()
  )
