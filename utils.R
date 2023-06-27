### Operator utilities

#####
### Workflow queries
get_workflow_id <- function(ctx) {
  if(is.null(ctx$task)) {
    return(ctx$workflowId)
  } else {
    workflowIdPair <- Find(function(pair) identical(pair$key, "workflow.id"), ctx$task$environment)
    workflowId <- workflowIdPair$value
    return(workflowId)
  }
}

get_step_id <- function(ctx) {
  if(is.null(ctx$task)) {
    return(ctx$stepId)
  } else {
    stepIdPair <- Find(function(pair) identical(pair$key, "step.id"), ctx$task$environment)
    stepId <- stepIdPair$value
    return(stepId)
  }
}



get_data_step <- function(ctx) {
  wf <- ctx$client$workflowService$get(get_workflow_id(ctx))
  ds <- Find(function(s) identical(s$id, get_step_id(ctx)), wf$steps)
  return(ds)
}
get_palettes <- function(ds) {
  palettes <- lapply(ds$model$axis$xyAxis, "[[", "colors")
  palettes
}
get_chart_types <- function(ds) {
  charts <- lapply(ds$model$axis$xyAxis, "[[", "chart")
  chart_types <- unlist(lapply(charts, function(x) class(x)[1]))
  chart_types
}

tercen_palette <- function(palette_list, n = 32) {
  palette_kind <- class(palette_list[[1]]$palette)[1]
  if(palette_kind == "JetPalette") {
    pal <- colorRampPalette(
      c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    )
    palette.colors <- pal(n) 
  } else if (palette_kind == "RampPalette") {
    cols <- unlist(lapply(palette_list[[1]]$palette$doubleColorElements, "[[", "color"))
    pal <- colorRampPalette(int_to_rgb(cols))
    palette.colors <- pal(n) 
  }  else if (palette_kind == "CategoryPalette") {
    pal_name <- palette_list[[1]]$palette$colorList$name
    if(pal_name == "") {
      palette.colors <- int_to_rgb(COLOR_LIST_1)
    }
  } else {
    palette.colors <- colorRampPalette(
      c("white", "red")
    )
  }
  return(palette.colors)
}

get_facet_formula <- function(ctx, wrap.1d, scales_mode) {
  cnames <- unlist(ctx$cnames)
  if(ctx$cnames[[1]] == "") cnames <- "."
  rnames <- unlist(ctx$rnames)
  if(ctx$rnames[[1]] == "") rnames <- "."
  
  # Handle spaces in variable names
  rnames <- paste0("`", rnames, "`")
  cnames <- paste0("`", cnames, "`")
    
  if(any(c(rnames, cnames) %in% ".") & wrap.1d) {
    facet <- facet_wrap(
      as.formula(paste(
        "~",
        paste(rnames, collapse = "+"),
        "+",
        paste(cnames, collapse = "+")
      )),
      scales = scales_mode
    )
  } else {
    facet <- facet_grid(
      as.formula(paste(
        paste(rnames, collapse = "+"),
        "~",
        paste(cnames, collapse = "+")
      )),
      scales = scales_mode
    )
  }
  
  return(facet)
    
}

#####
### Operator settings

get_settings <- function(ctx) {
  input.par <- list(
    plot.type    = ctx$op.value("plot_type", type = as.character, default = "png"),
    plot.width   = ctx$op.value("plot.width", type = as.double, default = 750),
    plot.height  = ctx$op.value("plot.height", type = as.double, default = 750),
    xlab         = ctx$op.value("xlab", type = as.character, default = ""),
    ylab         = ctx$op.value("ylab", type = as.character, default = ""),
    title        = ctx$op.value("title", type = as.character, default = ""),
    subtitle     = ctx$op.value("subtitle", type = as.character, default = ""),
    caption      = ctx$op.value("caption", type = as.character, default = ""),
    theme        = ctx$op.value("theme", type = as.character, default = "light"),
    base.size    = ctx$op.value("base.size", type = as.double, default = 11),
    dot.size     = ctx$op.value("dot.size", type = as.double, default = 0.5),
    bar.position = ctx$op.value("bar.position", type = as.character, default = "dodge"),
    bar.width    = ctx$op.value("bar.width", type = as.double, default = 0.25),
    dodge.width  = ctx$op.value("dodge.width", type = as.double, default = 1.1),
    jitter.width = ctx$op.value("jitter.width", type = as.double, default = 0.05),
    color.palette= ctx$op.value("color.palette", type = as.character, default = "crosstab"),
    scales       = ctx$op.value("scales", type = as.character, default = "fixed"),
    wrap.1d      = ctx$op.value("wrap.1d", type = as.logical, default = TRUE)
  )
  return(input.par)
}


#####
### Data preprocessing

getValues <- function(ctx) {
  data <- ctx %>% select(.y, .ri, .ci, .axisIndex)
  if(ctx$hasXAxis) data$.x <- select(ctx, .x)[[".x"]]
  
  if(length(ctx$colors)) data <- data %>% dplyr::bind_cols(ctx$select(unique(ctx$colors)))
  if(length(ctx$labels)) data <- data %>% dplyr::bind_cols(ctx$select(unique(ctx$labels)))
  if(length(ctx$errors)) {
    data$.error <- select(ctx, .error)[[".error"]]
    data$.ymin <- data$.y - data$.error
    data$.ymax <- data$.y + data$.error
  } 
  
  rnames <- ctx$rselect() 
  rnames$.ri <- seq_len(nrow(rnames)) - 1
  data <- left_join(data, rnames, by = ".ri", suffix = c("", ".YYY")) %>%
    select(-ends_with(".YYY"))
  
  cnames <- ctx$cselect()
  cnames$.ci <- seq_len(nrow(cnames)) - 1
  data <- left_join(data, cnames, by = ".ci", suffix = c("", ".YYY")) %>%
    select(-ends_with(".YYY"))
  
  return(data)
}



get_axis_labels <- function(ctx, lab, type) {
  nms <- switch(
    type,
    "x" = ctx$xAxis,
    "y" = ctx$yAxis,
    "col"= ctx$cnames,
    "row"= ctx$rnames
  )
  if_else(lab == "", paste0(nms, collapse = " - "), lab)
}
