create_overhangs <- function(model, 
                             facade, 
                             depth = 0) {
  for (i in seq_len(nrow(facade))) {
    win_name <- facade$name[i]
    new_mat <- list(
      'Shading:Overhang:Projection' = list(
        Name = paste0("Overhang_", win_name),
        'Window or Door Name' = win_name,
        'Height above Window or Door' = 0.1,
        'Tilt Angle from Window/Door' = 0,
        'Left extension from Window/Door Width' = 0,
        'Right extension from Window/Door Width' = 0,
        'Depth as Fraction of Window/Door Height' = depth
      ))
    model$add(new_mat)
  }
  model
}

set_shading <- function(model, shading = NA) {
  if (is.na(shading)) return(model)
  
  shade_tbl <- model$to_table(class = "Shading:Overhang:Projection")
  
  shade_tbl <- shade_tbl |> 
    mutate(value = ifelse(field == "Depth as Fraction of Window/Door Height",
                          sprintf("%0.2f", shading),
                          value))
  
  model$update(shade_tbl)
  model
}