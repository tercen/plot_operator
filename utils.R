### Operator utilities

# get_values <- function(ctx) {
#   
#   # Get y axis, ci, and ri
# 
#   # Get x axis, if any
#   
#   # Get colors, if any
#   
#   # Get labels, if any
#   
#   # Get error bars, if any
#   
#   return(df)
# }


get_palettes <- function(ctx) {
  wf <- ctx$client$workflowService$get(ctx$workflowId)
  ds <- Find(function(s) identical(s$id, ctx$stepId), wf$steps)
  palettes <- lapply(ds$model$axis$xyAxis, "[[", "colors")
  palettes
}

tercen_palette <- function(palette_list) {
  palette_kind <- class(palette_list[[1]]$palette)[1]
  if(palette_kind == "JetPalette") {
    palette.colors <- colorRampPalette(
      c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    )
  } else if (palette_kind == "RampPalette") {
    cols <- unlist(lapply(palette_list[[1]]$palette$doubleColorElements, "[[", "color"))
    palette.colors <- colorRampPalette(
       int_to_rgb(cols)
    )
  } else {
    palette.colors <- colorRampPalette(
      c("white", "red")
    )
  }
  return(palette.colors)
}

