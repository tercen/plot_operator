suppressPackageStartupMessages({
  library(tercen)
  library(dplyr)
  library(ggplot2)
  library(tim)
  library(svglite)
})

source("./utils_colors.R")
source("./utils.R")

ctx = tercenCtx()
input.par <- get_settings(ctx)

split_cells <- TRUE

default_color <- "#36454f"

#####
### Get data step and meta data

ds <- get_data_step(ctx)

df <- getValues(ctx)
pl <- get_palettes(ds)


n_cells <- ctx$cschema$nRows * ctx$rschema$nRows

if(n_cells > 1000) stop("Too many cells (> 1000) to use this operator.")

if(n_cells > 25 | split_cells) {
  df <- df %>% group_by(.ci, .ri)
  plt_names <- df %>% 
    group_data %>% 
    select(-.rows) %>% 
    tidyr::unite("label")
  plt_files <- df %>%
    group_map(~ generate_plot(ctx, ., pl, input.par, ds, multipanel = FALSE)) %>%
    unlist
  new_names <- paste0(dirname(plt_files)[1], "/Tercen_Plot_", plt_names$label)
  file.rename(plt_files, new_names)
  on.exit(unlink(new_names))
  
  zip_file <- paste0(dirname(new_names)[1], "/Tercen_Plots.zip")
  on.exit(unlink(zip_file))
  zip::zipr(zipfile = zip_file, files = new_names)
  
  tim::plot_file_to_df(zip_file) %>%
    select(-checksum) %>%
    as_relation() %>%
    as_join_operator(list(), list()) %>%
    save_relation(ctx)
  
} else {
  plt_files <- generate_plot(ctx, df, pl, input.par, ds)
  on.exit(unlink(plt_files))
  
  tim::plot_file_to_df(plt_files, filename = paste0("Tercen_Plot.", tools::file_ext(plt_files))) %>%
    select(-checksum) %>%
    as_relation() %>%
    as_join_operator(list(), list()) %>%
    save_relation(ctx)
}

# handle filenames
