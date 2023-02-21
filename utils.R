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
get_workflow_id <- function(ctx) {
  if(is.null(ctx$task)) {
    return(ctx$workflowId)
  } else {
    workflowIdPair <- Find(function(pair) identical(pair$key, "workflow.id"), task$environment)
    workflowId <- workflowIdPair$value
    return(workflowId)
  }
}

get_step_id <- function(ctx) {
  if(is.null(ctx$task)) {
    return(ctx$stepId)
  } else {
    stepIdPair <- Find(function(pair) identical(pair$key, "step.id"), task$environment)
    stepId <- stepIdPair$value
    return(stepId)
  }
}

get_palettes <- function(ctx) {
  wf <- ctx$client$workflowService$get(get_workflow_id(ctx))
  ds <- Find(function(s) identical(s$id, get_step_id(ctx)), wf$steps)
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

