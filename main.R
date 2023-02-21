suppressPackageStartupMessages({
  library(tercen)
  library(dplyr)
  library(ggplot2)
  library(tim)
})

ctx = tercenCtx()

## add labels
## reorder rows
## scales parameter

# row_index <- ".ci"
# row_names <- ".ci"
# row_values <- ctx$rselect()
# unlist(ctx$rnames)

if(ctx$chartTypes == "heatmap") {
  mat <- ctx %>% select(.ci, .ri, .y)
  plt <- ggplot(mat, aes(x = .ci, y = .ri, fill = .y)) +
    geom_tile()
} else {
  stop("This chart type is not supported.")
}

#####
## Set colors
pl <- get_palettes(ctx)
pal <- tercen_palette(pl)
plt <- plt + 
  scale_fill_gradientn(colours = pal(32), trans = "log")


#####
## Set theme
plt <- plt + 
  theme_classic()

#####
## Save plot file

# remotes::install_github("tercen/tim")
# library(svglite)
tt <- tim::save_plot(plt, type = "png", width = 5, height = 5, unit = "cm")

df_out <- tim::plot_file_to_df(tt) %>%
  select(-checksum)

df_out %>%
  as_relation() %>%
  as_join_operator(list(), list()) %>%
  save_relation(ctx)
