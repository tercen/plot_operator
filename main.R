suppressPackageStartupMessages({
  library(tercen)
  library(dplyr)
  library(ggplot2)
  library(tim)
})


source("./utils_colors.R")
source("./utils.R")

ctx = tercenCtx()
input.par <- get_settings(ctx)

#####
### Get data step and meta data

ds <- get_data_step(ctx)
chart_types <- get_chart_types(ds)

ctx$log(message = paste0("Generating chart: ", chart_types))
## add labels
## reorder rows
## scales parameter

df <- getValues(ctx)
pl <- get_palettes(ds)

# stop if different colors in layers
if(!any(unlist(lapply(ctx$colors, identical, ctx$colors[[1]])))) {
  stop("The same color factors must be used across layers.")
}

# Initialise chart
if(any(chart_types == "ChartHeatmap")) {
  
  if(length(chart_types) > 1) {
    layer <- which(chart_types == "ChartHeatmap")[1] - 1L
    ctx$log("Multiple layers found. Only the first Heatmap will be represented.")
  }
  
  df_plot <- df %>% filter(.axisIndex == layer)

  plt <- ggplot(df_plot, aes_string(x = ".ci", y = ".ri", fill = unlist(ctx$colors))) +
    geom_tile()
  
} else if(all(chart_types %in% c("ChartPoint", "ChartLine", "ChartBar"))) {
  
  ncells <- ctx$cschema$nRows * ctx$rschema$nRows
  if(ncells > 25) stop("This chart can only be produced with less than 25 projected cells.")
  
  plt <- ggplot(mapping = aes_string(x = ".x", y = ".y", fill = unique(unlist(ctx$colors)), color = unique(unlist(ctx$colors)))) 
  
  for(j in seq_along(chart_types)) {
    type <- chart_types[j]
    df_plot <- df %>% filter(.axisIndex == j - 1L)
    
    if(type == "ChartPoint") plt <- plt + geom_point(data = df_plot, size = 1)
    if(type == "ChartLine") plt <- plt + geom_line(data = df_plot)
    if(type == "ChartBar") plt <- plt + geom_bar(data = df_plot)
  } 
  # apply per axis index: + geom_point, geom_line
  
} else {
  
  stop("This chart type is not supported.")
  
}

# Add colors
palette_kind <- class(pl[[1]]$palette)[1]
if(palette_kind == "CategoryPalette") {
  plt <- plt + 
    scale_colour_manual(values = tercen_palette(pl, n = 32))
} else {
  plt <- plt + 
    scale_color_gradientn(colours = tercen_palette(pl, n = 32)) +
    scale_fill_gradientn(colours = tercen_palette(pl, n = 32))
}

#####
### Facets based on rows and columns
if(!any(chart_types == "ChartHeatmap")) {
  plt <- plt + get_facet_formula(ctx, input.par$wrap.1d)
}

#####
## Set theme
plt <- plt + 
  labs(
    title = input.par$title,
    subtitle = input.par$subtitle,
    caption = input.par$caption,
    x = if_else(input.par$xlab == "", paste0(ctx$xAxis, collapse = " - "), input.par$xlab),
    y = if_else(input.par$ylab == "", paste0(ctx$yAxis, collapse = " - "), input.par$ylab),
    color = "Legend",
    fill = "Legend"
  ) +
  theme_classic()

#####
## Save plot file
if(input.par$plot.type ==  "svg2") {
  input.par$plot.type <- "svg"
  device <- svg
} else {
  device <- NULL
}

plt_files <- tim::save_plot(
  plt,
  type = input.par$plot.type,
  width = input.par$plot.width / 144,
  height = input.par$plot.height / 144,
  units = "in",
  dpi = 144,
  device = device
)

tim::plot_file_to_df(plt_files) %>%
  select(-checksum) %>%
  as_relation() %>%
  as_join_operator(list(), list()) %>%
  save_relation(ctx)
