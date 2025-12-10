rm(list = ls())
library(here)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(knitr)
library(gt)

# Upload data
blk7_results <- read_csv(here("data", "results", "all_scenarios_blk7"))
blk7_baseline <- read_csv( here("data", "results", "blk7_baseline"))
blk7_baseline <- blk7_baseline$e_ac
blk7_results_pct <- blk7_results |> 
  mutate(pct_change = 100 * (blk7_results$energy_ac - blk7_baseline) / blk7_baseline)
blk7_results_pct$block <- "Blk7"

blk7_results_greenroof <- read_csv(here("data", "results", "greenroof_data_blk7"))
blk7_results_greenroof_pct <- blk7_results_greenroof |> 
  mutate(pct_change = 100 * (blk7_results_greenroof$e_ac - blk7_baseline) / blk7_baseline)
blk7_results_greenroof_pct$block <- "Blk7"  

blk5_results <- read_csv(here("data", "results", "all_scenarios_blk5"))
blk5_baseline <- read_csv( here("data", "results", "blk5_baseline"))
blk5_baseline <- blk5_baseline$e_ac
blk5_results_pct <- blk5_results |> 
  mutate(pct_change = 100 * (blk5_results$energy_ac - blk5_baseline) / blk5_baseline)
blk5_results_pct$block <- "Blk5"

blk5_results_greenroof <- read_csv(here("data", "results", "greenroof_data_blk5"))
blk5_results_greenroof_pct <- blk5_results_greenroof |> 
  mutate(pct_change = 100 * (blk5_results_greenroof$e_ac - blk5_baseline) / blk5_baseline)
blk5_results_greenroof_pct$block <- "Blk5"  

blk2_results <- read_csv(here("data", "results", "all_scenarios_blk2"))
blk2_baseline <- read_csv( here("data", "results", "blk2_baseline"))
blk2_baseline <- blk2_baseline$e_ac
blk2_results_pct <- blk2_results |> 
  mutate(pct_change = 100 * (blk2_results$energy_ac - blk2_baseline) / blk2_baseline)
blk2_results_pct$block <- "Blk2"

blk2_results_greenroof <- read_csv(here("data", "results", "greenroof_data_blk2"))
blk2_results_greenroof_pct <- blk2_results_greenroof |> 
  mutate(pct_change = 100 * (blk2_results_greenroof$e_ac - blk2_baseline) / blk2_baseline)
blk2_results_greenroof_pct$block <- "Blk2"  

all_blks_results <- bind_rows(blk7_results_pct, blk5_results_pct, blk2_results_pct)
all_blks_results$scenario_type <- factor(all_blks_results$scenario_type,
                           levels = c("Shading", "Coating", "Combined"))
all_blks_results$block <- factor(all_blks_results$block,
                                 levels = c("Blk2", "Blk7", "Blk5"))
#all_blks_results$case <- factor(all_blks_results$case,
#                                 levels = c("Cool", "Med-Cool", "Medium", "Med-Dark", "Dark"))
all_blks_results$facade <- factor(all_blks_results$facade,
                                 levels = c("North", "South", "Both"))
all_blks_greenroof <- bind_rows(blk7_results_greenroof_pct, blk5_results_greenroof_pct, blk2_results_greenroof_pct)
all_blks_greenroof$block <- factor(all_blks_greenroof$block,
                                 levels = c("Blk2", "Blk7", "Blk5"))

write_csv(all_blks_results,
          here("data", "results", "all_blks_results"))

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

# Only coating plots
all_blks_results |> 
  filter(scenario_type == "Shading") |> 
  ggplot(aes(x = block,
             y = pct_change,
             fill = case,
             color = case)) +
  geom_point(shape = 21, size = 3)+
  ylim(-10,0) +
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

all_blks_results |> 
  filter(scenario_type == "Coating") |> 
  ggplot(aes(x = block,
             y = pct_change,
             fill = case,
             color = case)) +
  geom_point(shape = 21, size = 3)+
  ylim(-10,0) +
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

# Complete coating plots
complete_coating |> 
  ggplot(aes(x = block,
             y = pct_change)) +
  geom_col(fill = "black") +
  ylim(-10,0) +
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
