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

## add labels
## reorder rows
## scales parameter

df <- getValues(ctx)
pl <- get_palettes(ds)

if(chart_types == "ChartHeatmap") {
  
  plt <- ggplot(df, aes(x = ".ci", y = ".ri", fill = unlist(ctx$colors))) +
    geom_tile()
  
} else if(chart_types == "ChartPoint") {
  
  ncells <- ctx$cschema$nRows * ctx$rschema$nRows
  if(ncells > 25) stop("This chart can only be produced with less than 25 projected cells.")
  
  plt <- ggplot(df, aes_string(x = ".x", y = ".y", fill = unlist(ctx$colors), color = unlist(ctx$colors))) +
    geom_point(size = 1)
  
} else {
  
  stop("This chart type is not supported.")
  
}

palette_kind <- class(pl[[1]]$palette)[1]
if(palette_kind == "CategoryPalette") {
  plt <- plt + 
    scale_colour_manual(values = tercen_palette(pl, n = 32))
} else {
  plt <- plt + 
    scale_color_gradientn(colours = tercen_palette(pl, n = 32), trans = "log") +
    scale_fill_gradientn(colours = tercen_palette(pl, n = 32), trans = "log")
}


#####
### Facets based on rows and columns
if(ctx$chartTypes != "heatmap") {
  plt <- plt + get_facet_formula(ctx, input.par$wrap.1d)
}

#####
## Set theme
plt <- plt + 
  labs(
    title = input.par$title,
    subtitle = input.par$subtitle,
    caption = input.par$caption,
    x = input.par$xlab,
    y = input.par$ylab,
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
