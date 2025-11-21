set_coating <- function(model, 
                        coating = NA) {
  if (is.na(coating)) return(model)
  
  coat_tbl <- model$to_table(class = "Material") |> 
    filter(name == "M15 150mm heavyweight concrete") |> 
    mutate(value = case_when(
        field == "Solar Absorptance" ~ sprintf("%.2f", coating),
        TRUE ~ value)
    )
  
  model$update(coat_tbl)
  model
}