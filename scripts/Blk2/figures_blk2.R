rm(list = ls())
library(here)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

results <- read_csv(here("data", "results", "all_scenarios_blk2"))
results_greenroof <- read_csv(here("data", "results", "greenroof_data_blk2"))

baseline <- 5952.95

results_pct <- results |> 
  mutate(pct_change = 100 * (results$energy_ac - baseline) / baseline)

results_greenroof_pct <- results_greenroof |> 
  mutate(pct_change = 100 * (results_greenroof$e_ac - baseline) / baseline)

results |> 
  ggplot(aes(x = scenario_type, 
             y = energy_ac)) +
  geom_point() +
  geom_hline(yintercept = baseline, color = "red", linetype = "dashed") +
  facet_wrap(~facade) +
  labs(
    title = "Basline vs Retrofit Scenarios Blk2",
    x = "Scenario",
    y = "Cooling Demand (kWh)"
  )

results_pct |> 
  ggplot(aes(x = scenario_type,
             y = pct_change)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~facade) +
  labs(
    title = "Percent Change in Cooling Demand vs Baseline Blk2",
    x = "Scenario",
    y = "Δ Cooling Demand (%)"
  )

results_greenroof |> 
  ggplot(aes(x = case,
             y = e_ac)) +
  geom_point() +
  #geom_col(width = 0.3) +
  facet_wrap(~scenario_type) +
  labs(
    title = "Effects of Green Roof Blk2",
    x = "Case",
    y = "Cooling Demand (kWh)"
  )

results_greenroof_pct |> 
  ggplot(aes(x = case,
             y = pct_change,
             fill = case)) +
  #geom_point() +
  geom_col(width = 0.3) +
  facet_wrap(~scenario_type) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(
    title = "Percent Change in Green Roof Effects Blk2",
    x = "Case",
    y = "Δ Cooling Demand (%)"
  )
