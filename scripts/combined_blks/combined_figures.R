rm(list = ls())
library(here)
library(tidyverse)
library(ggplot2)
library(dplyr)

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
all_blks_greenroof <- bind_rows(blk7_results_greenroof_pct, blk5_results_greenroof_pct, blk2_results_greenroof_pct)

write_csv(all_blks_results,
          here("data", "results", "all_blks_results"))

# Create plots
all_blks_results |> 
  filter(facade == "North") |> 
  filter(scenario_type %in% c("Shading", "Coating")) |> 
  ggplot(aes(x = scenario_type,
             y = pct_change)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~block) +
  labs(
    title = "Percent Change in Cooling Demand on North Facade",
    x = "Scenario",
    y = "Δ Cooling Demand (%)"
  )

all_blks_results |> 
  filter(facade == "South") |> 
  filter(scenario_type %in% c("Shading", "Coating")) |> 
  ggplot(aes(x = scenario_type,
             y = pct_change)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~block) +
  labs(
    title = "Percent Change in Cooling Demand on South Facade",
    x = "Scenario",
    y = "Δ Cooling Demand (%)"
  )

all_blks_results |> 
  filter(facade == "Both") |> 
  filter(scenario_type %in% c("Shading", "Coating")) |> 
  ggplot(aes(x = scenario_type,
             y = pct_change)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~block) +
  labs(
    title = "Percent Change in Cooling Demand on Both Facades",
    x = "Scenario",
    y = "Δ Cooling Demand (%)"
  )

# Green roof plots
all_blks_greenroof |> 
  filter(scenario_type == "Green Roof") |> 
  ggplot(aes(x = case,
             y = pct_change,
             fill = case)) +
  geom_col(width = 0.3) +
  facet_wrap(~block) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Percent Change in Cooling Demand due to Green Roofs",
    x = "Case",
    y = "Δ Cooling Demand (%)"
  )

all_blks_greenroof |> 
  filter(scenario_type == "Coating + Green Roof") |> 
  ggplot(aes(x = case,
             y = pct_change,
             fill = case)) +
  geom_col(width = 0.3) +
  facet_wrap(~block) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Percent Change in Cooling Demand due to Green Roofs and Cool Coating",
    x = "Case",
    y = "Δ Cooling Demand (%)"
  )
