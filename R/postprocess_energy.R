filter_weekdays <- function(report) {
  report |>
    mutate(datetime = ymd(datetime)) |>
    filter(!(wday(datetime) %in% c(1, 7)))
}

summarise_meters <- function(report_weekday,
                             cop = 3) {
  energy_by_meter <- report_weekday |>
    group_by(name, case) |>
    summarise(e_J = sum(value), .groups = "drop") |>
    mutate(e_kWh = e_J / 3.6e6)
  
  list(
    e_light = energy_by_meter |>
      filter(name == "InteriorLights:Electricity") |>
      select(case, e_kWh),
    e_plug  = energy_by_meter |>
      filter(name == "InteriorEquipment:Electricity") |>
      select(case, e_kWh),
    e_ac    = energy_by_meter |>
      filter(name == "Cooling:EnergyTransfer") |>
      mutate(e_ac = e_kWh / cop) |> 
      select(case, e_ac)
  )
}

compute_E_total_blk <- function(e_light, e_ac, e_plug,
                                ac_own, n_occ, n_flats, blk = 7) {
  (e_light + e_ac * ac_own[blk] + e_plug) * (n_occ[blk] / n_flats[blk])
}