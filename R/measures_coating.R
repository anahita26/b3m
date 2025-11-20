set_coating <- function(model, 
                        coating = NA,
                        material_name = "M15 150mm heavyweight concrete") {
  if (is.na(coating)) return(model)
  
  coat_tbl <- model$to_table(class = "Material") |> 
    filter(name == material_name) |> 
    mutate(value = case_when(
        field == "Solar Absorptance" ~ sprintf("%.2f", coating),
        TRUE ~ value)
    )
  
  model$update(coat_tbl)
  model
}