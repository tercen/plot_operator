suppressPackageStartupMessages({
  library(tercen)
  library(tercenApi)
  library(dplyr)
  library(ggplot2)
})

source("./utils_colors.R")
source("./utils.R")

ctx = tercenCtx()

input.par <- get_settings(ctx)
input.par$plot.width <- as.numeric(input.par$plot.width)
input.par$plot.height <- as.numeric(input.par$plot.height)

default_color <- input.par$default.color

## replace by ctx$query
is_2d_histogram <- lapply(ctx$schema$columns, "[[", "name") %>%
  unlist() %>%
  `%in%`(c(".histogram_count", ".x_bin_size", ".y_bin_size"), .) %>%
  all()

ds <- get_data_step(ctx)

df <- getValues(ctx, is_2d_histogram)
pl <- get_palettes(ds)

palette_df <- jsonlite::fromJSON("palettes.json", simplifyVector = TRUE)
palette <- try(palette_df %>%
  filter(name == pl[[1]]$palette$properties[[1]]$value))

if(inherits(pl[[1]]$palette, "JetPalette")) {
  palette <- try(palette_df %>%
                   filter(name == "Jet"))
}


if(inherits(palette, "try-error")) {
  palette_name <- pl[[1]]$palette$colorList$name
  if(palette_name == "") palette_name <- "Palette-1"
  palette <- try(palette_df %>%
                   filter(name == palette_name))
}

## Get operator specs and page factors
specs <- ctx$query$operatorSettings$operatorRef$operatorSpec

if(length(specs$inputSpecs)) {
  metafactors <- specs$inputSpecs[[1]]$metaFactors
  spec_names <- lapply(metafactors, "[[", "name")
  page_factors <- lapply(metafactors[grepl("Page|page", unlist(spec_names))], "[[", "factors")[[1]]
} else {
  metafactors <- NULL
  spec_names <- NULL
  page_factors <- NULL
  
}


has_page <- length(page_factors) != 0
if(has_page) page_factor_names <- lapply(page_factors, "[[", "name") %>% unlist()

n_cells <- ctx$cschema$nRows * ctx$rschema$nRows

chart_types <- get_chart_types(ds)
hm <- any(chart_types == "ChartHeatmap")

if(is_2d_histogram) chart_types <- "2D_Histogram"

if(input.par$split_cells | has_page) {
  
  if(!hm & n_cells > 1000) stop("Too many cells (> 1000) to use this operator.")
  
  if(has_page) {
    df <- df %>% group_by(across(all_of(page_factor_names)))
  } else {
    df <- df %>% group_by(.ci, .ri) 
  }
  
  plt_names <- df %>% 
    group_data %>% 
    select(-.rows) %>% 
    tidyr::unite("label", sep = "_r")
  
  if(hm) {
    plts <- df %>%
      group_map(~ generate_plot(ctx, ., pl, palette, input.par, ds, chart_types = chart_types, multipanel = TRUE), .keep = TRUE) %>%
      bind_rows()
    
  } else {
    plts <- df %>%
      group_map(~ generate_plot(ctx, ., pl, palette, input.par, ds, chart_types = chart_types, multipanel = FALSE), .keep = TRUE) %>%
      bind_rows()
    
  }
  plt_files <- plts$plot_file
  
  new_names <- file.path(
    dirname(plt_files)[1],
    paste0(
      input.par$file.name.prefix,
      "_c",
      plt_names$label,
      ".",
      tools::file_ext(plt_files)
    )
  )
  
  file.rename(plt_files, new_names)
  on.exit(unlink(new_names))
  
  zip_file <- file.path(dirname(new_names)[1], paste0(input.par$file.name.prefix, ".zip"))
  on.exit(unlink(zip_file))
  
  zip::zipr(zipfile = zip_file, files = new_names)
  max_plots <- 10
  first_plots <- lapply(new_names[1:max_plots][!is.na(new_names[1:max_plots])], tercen::file_to_tercen) %>%
    bind_rows() %>%
    mutate(plot_width = plts$plot.width, plot_height = plts$plot.height) 
  
  tercen::file_to_tercen(zip_file) %>%
    bind_rows(first_plots) %>%
    as_relation() %>%
    as_join_operator(list(), list()) %>%
    save_relation(ctx)
  
} else {
  
  plts <- generate_plot(ctx, df, pl, palette, input.par, ds, chart_types = chart_types)
  plt_files <- plts$plot_file
  on.exit(unlink(plt_files))
  
  tercen::file_to_tercen(plt_files, filename = paste0("Tercen_Plot.", tools::file_ext(plt_files))) %>%
    mutate(plot_width = plts$plot.width, plot_height = plts$plot.height) %>%
    as_relation() %>%
    as_join_operator(list(), list()) %>%
    save_relation(ctx)
  
}

