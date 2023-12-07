suppressPackageStartupMessages({
  library(tercen)
  library(tercenApi)
  library(dplyr)
  library(ggplot2)
  library(tim)
  library(svglite)
})

source("./utils_colors.R")
source("./utils.R")

ctx = tercenCtx()
input.par <- get_settings(ctx)
input.par$plot.width <- as.numeric(input.par$plot.width)
input.par$plot.height <- as.numeric(input.par$plot.height)

default_color <- input.par$default.color

ds <- get_data_step(ctx)
df <- getValues(ctx)
pl <- get_palettes(ds)

n_cells <- ctx$cschema$nRows * ctx$rschema$nRows

chart_types <- get_chart_types(ds)
hm <- any(chart_types == "ChartHeatmap")

if(!hm & (n_cells > input.par$n_cells_max | input.par$split_cells)) {
  
  if(n_cells > 1000) stop("Too many cells (> 1000) to use this operator.")
  
  df <- df %>% group_by(.ci, .ri)
  
  plt_names <- df %>% 
    group_data %>% 
    select(-.rows) %>% 
    tidyr::unite("label", sep = "_r")
  
  plt_files <- df %>%
    group_map(~ generate_plot(ctx, ., pl, input.par, ds, multipanel = FALSE)) %>%
    unlist
  
  new_names <- paste0(
    dirname(plt_files)[1],
    "/Tercen_Plot_c",
    plt_names$label,
    ".", tools::file_ext(plt_files)
  )
  
  file.rename(plt_files, new_names)
  on.exit(unlink(new_names))
  
  zip_file <- paste0(dirname(new_names)[1], "/Tercen_Plots.zip")
  on.exit(unlink(zip_file))
  
  zip::zipr(zipfile = zip_file, files = new_names)
  max_plots <- 10
  first_plots <- lapply(new_names[1:max_plots][!is.na(new_names[1:max_plots])], tercen::file_to_tercen) %>%
    bind_rows()
  
  tercen::file_to_tercen(zip_file) %>%
    bind_rows(first_plots) %>%
    select(-checksum) %>%
    as_relation() %>%
    as_join_operator(list(), list()) %>%
    save_relation(ctx)
  
} else {
  
  plt_files <- generate_plot(ctx, df, pl, input.par, ds)
  on.exit(unlink(plt_files))
  
  tercen::file_to_tercen(plt_files, filename = paste0("Tercen_Plot.", tools::file_ext(plt_files))) %>%
    as_relation() %>%
    as_join_operator(list(), list()) %>%
    save_relation(ctx)
  
}
