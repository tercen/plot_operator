### Operator utilities

#####
### Workflow queries
get_workflow_id <- function(ctx) {
  if (is.null(ctx$task)) {
    return(ctx$workflowId)
  } else {
    workflowIdPair <-
      Find(function(pair)
        identical(pair$key, "workflow.id"),
        ctx$task$environment)
    workflowId <- workflowIdPair$value
    return(workflowId)
  }
}

get_step_id <- function(ctx) {
  if (is.null(ctx$task)) {
    return(ctx$stepId)
  } else {
    stepIdPair <-
      Find(function(pair)
        identical(pair$key, "step.id"),
        ctx$task$environment)
    stepId <- stepIdPair$value
    return(stepId)
  }
}



get_data_step <- function(ctx) {
  wf <- ctx$client$workflowService$get(get_workflow_id(ctx))
  ds <-
    Find(function(s)
      identical(s$id, get_step_id(ctx)), wf$steps)
  return(ds)
}
get_palettes <- function(ds) {
  palettes <- lapply(ds$model$axis$xyAxis, "[[", "colors")
  palettes
}

get_chart_types <- function(ds) {
  charts <- lapply(ds$model$axis$xyAxis, "[[", "chart")
  chart_types <- unlist(lapply(charts, function(x)
    class(x)[1]))
  chart_names <- unlist(lapply(charts, "[[", "name"))
  
  chart_types[chart_names == "h-grid"] <- "ChartHLine"
  chart_types[chart_names == "v-grid"] <- "ChartVLine"

  chart_types
}

get_sizes <- function(ds, size_scale_factor = 4) {
  charts <- lapply(ds$model$axis$xyAxis, "[[", "chart")
  chart_sizes <- (lapply(charts, "[[", "pointSize"))
  idx <- unlist(lapply(chart_sizes, is.null))
  chart_sizes[idx] <- size_scale_factor
  chart_sizes <- unlist(chart_sizes)
  chart_sizes
}

tercen_palette <- function(palette_list, n = 32) {
  palette_kind <- class(palette_list[[1]]$palette)[1]
  if (palette_kind == "JetPalette") {
    pal <- colorRampPalette(
      c(
        "#00007F",
        "blue",
        "#007FFF",
        "cyan",
        "#7FFF7F",
        "yellow",
        "#FF7F00",
        "red",
        "#7F0000"
      )
    )
    palette.colors <- pal(n)
  } else if (palette_kind == "RampPalette") {
    cols <-
      unlist(lapply(palette_list[[1]]$palette$doubleColorElements, "[[", "color"))
    pal <- colorRampPalette(int_to_rgb(cols))
    palette.colors <- pal(n)
  }  else if (palette_kind == "CategoryPalette") {
    pal_name <- palette_list[[1]]$palette$colorList$name
    if (pal_name %in% c("", "Palette-1")) {
      palette.colors <- int_to_rgb(COLOR_LIST_1)
    } else if (pal_name %in% c("Palette-2")) {
      palette.colors <- int_to_rgb(COLOR_LIST_2)
    } else {
      palette.colors <- int_to_rgb(COLOR_LIST_1)
    }
  } else {
    palette.colors <- colorRampPalette(c("white", "red"))
  }
  return(palette.colors)
}

get_facet_formula <- function(ctx, wrap.1d, scales_mode) {
  cnames <- unlist(ctx$cnames)
  if (ctx$cnames[[1]] == "")
    cnames <- "."
  rnames <- unlist(ctx$rnames)
  if (ctx$rnames[[1]] == "")
    rnames <- "."
  
  # Handle spaces in variable names
  rnames <- paste0("`", rnames, "`")
  cnames <- paste0("`", cnames, "`")
  
  if (any(c(rnames, cnames) %in% ".") & wrap.1d) {
    facet <- facet_wrap(as.formula(paste(
      "~",
      paste(rnames, collapse = "+"),
      "+",
      paste(cnames, collapse = "+")
    )),
    scales = scales_mode,
    switch = "y")
  } else {
    facet <- facet_grid(as.formula(paste(
      paste(rnames, collapse = "+"),
      "~",
      paste(cnames, collapse = "+")
    )),
    scales = scales_mode,
    switch = "y")
  }
  
  return(facet)
  
}

#####
### Operator settings

get_settings <- function(ctx) {
  props <- jsonlite::read_json("operator.json")$properties
  
  input.par <- lapply(props, function(x) {
    kind <- switch (
      x$kind,
      "StringProperty" = as.character,
      "EnumeratedProperty" = as.character,
      "BooleanProperty" = as.logical,
      "DoubleProperty" = as.double
    )
    ctx$op.value(x$name, kind, x$defaultValue)
  })
  names(input.par) <- lapply(props, "[[", "name")
  
  return(input.par)
}


#####
### Data preprocessing

getValues <- function(ctx) {
  data <- ctx %>% select(.y, .ri, .ci, .axisIndex)
  if (ctx$hasXAxis)
    data$.x <- select(ctx, .x)[[".x"]]
  
  if (length(ctx$colors))
    data <- data %>% dplyr::bind_cols(ctx$select(unique(ctx$colors)))
  if (length(ctx$labels)) {
    text_labels <- ctx$select(unique(ctx$labels)) %>%
      tidyr::unite(col = "text_labels", sep = " - ")
    data <- data %>% dplyr::bind_cols(text_labels)
  }
  if (length(ctx$errors)) {
    data$.error <- select(ctx, .error)[[".error"]]
    data$.ymin <- data$.y - data$.error
    data$.ymax <- data$.y + data$.error
  }
  
  rnames <- ctx$rselect()
  rnames$.ri <- seq_len(nrow(rnames)) - 1
  data <-
    left_join(data, rnames, by = ".ri", suffix = c("", ".YYY")) %>%
    select(-ends_with(".YYY"))
  
  cnames <- ctx$cselect()
  cnames$.ci <- seq_len(nrow(cnames)) - 1
  data <-
    left_join(data, cnames, by = ".ci", suffix = c("", ".YYY")) %>%
    select(-ends_with(".YYY"))
  
  return(data)
}



get_axis_labels <- function(ctx, lab, type) {
  nms <- switch(
    type,
    "x" = ctx$xAxis,
    "y" = ctx$yAxis,
    "col" = ctx$cnames,
    "row" = ctx$rnames
  )
  if_else(lab == "", paste0(nms, collapse = " - "), lab)
}



generate_plot <-
  function(ctx, df, pl, input.par, ds, multipanel = TRUE, size_scale_factor = 4) {
    chart_types <- get_chart_types(ds)
    chart_sizes <- get_sizes(ds)
    
    ### Default width and height
    if (input.par$plot.width == "" | is.na(input.par$plot.width)) {
      N <-
        if_else(
          multipanel,
          ds$model$columnTable$cellSize * ctx$cschema$nRows,
          ds$model$columnTable$cellSize
        )
      input.par$plot.width <- round(250 + 1.50 * N)
    }
    if (input.par$plot.height == "" | is.na(input.par$plot.height)) {
      N <-
        if_else(
          multipanel,
          ds$model$rowTable$cellSize * ctx$rschema$nRows,
          ds$model$rowTable$cellSize
        )
      input.par$plot.height <- round(150 + 1.25 * N)
    }
    
    ctx$log(message = paste0("Generating charts: ", paste0(chart_types, collapse = " + ")))
    
    # stop if different colors in layers
    if (length(ctx$colors) > 0 &&
        !any(unlist(lapply(ctx$colors, identical, ctx$colors[[1]])))) {
      stop("The same color factors must be used across layers.")
    }
    
    # Initialise chart
    if (any(chart_types == "ChartHeatmap")) {
      if (length(chart_types) > 1) {
        ctx$log("Multiple layers found. Only the first Heatmap will be represented.")
      }
      layer <- which(chart_types == "ChartHeatmap")[1] - 1L
      df_plot <- df %>% filter(.axisIndex == layer)
      
      x_labels <- ctx$cselect() %>%
        tidyr::unite("x_label") %>%
        mutate(.ci = seq_len(nrow(.)) - 1L)
      y_labels <- ctx$rselect() %>%
        tidyr::unite("y_label") %>%
        mutate(.ri = seq_len(nrow(.)) - 1L)
      df_plot <- df_plot %>%
        left_join(x_labels, by = ".ci") %>%
        left_join(y_labels, by = ".ri")
      
      plt <-
        ggplot(df_plot,
               aes_string(
                 x = "x_label",
                 y = "y_label",
                 fill = unlist(ctx$colors)
               )) +
        geom_tile() +
        scale_y_discrete(limits = rev)
      
      if (input.par$heatmap_annotation %in% c("rows", "none"))
        plt <- plt + scale_x_discrete(labels = NULL)
      if (input.par$heatmap_annotation %in% c("columns", "none"))
        plt <- plt + scale_y_discrete(limits = rev, labels = NULL)
      
    } else if (all(chart_types %in% c("ChartPoint", "ChartLine", "ChartHLine", "ChartVLine", "ChartBar"))) {
      ncells <- ctx$cschema$nRows * ctx$rschema$nRows
      # if(ncells > 25) stop("This chart can only be produced with less than 25 projected cells.")
      
      col_factors <- unique(unlist(ctx$colors))
      if (!is.null(col_factors)) {
        if(length(col_factors) == 1) {
          col_factors <- paste0("`", col_factors, "`")
        }
        else {
          col_factors <- paste0("interaction(", paste0("`", col_factors, "`", collapse = ","), ", lex.order = TRUE)")
        }
      }
      
      plt <- ggplot(
        mapping = aes_string(
          x = ".x",
          y = ".y",
          fill = col_factors,
          group = col_factors,
          order = col_factors
        )
      )
      
      pos <- switch (
        input.par$bar.position,
        "dodge" = position_dodge(width = input.par$dodge.width),
        "stack" = "stack",
        "fill" = "fill"
      )
      
      for (j in seq_along(chart_types)) {
        type <- chart_types[j]
        df_plot <- df %>% filter(.axisIndex == j - 1L)
        
        if (type == "ChartPoint") {
          plt <- plt + geom_point(
            data = df_plot,
            shape = 21,
            size = 2 * chart_sizes[j] / size_scale_factor,
            stroke = 0.
          )
        }
        if (type == "ChartLine") {
          plt <- plt + geom_line(
            data = df_plot,
            mapping = aes_string(color = col_factors),
            size = chart_sizes[j] / size_scale_factor
          )
        }
        if (type == "ChartHLine") {
          plt <- plt + geom_hline(
            data = df_plot,
            mapping = aes_string(yintercept = ".y", color = col_factors),
            size = chart_sizes[j] / size_scale_factor
          )
        }
        if (type == "ChartVLine") {
          plt <- plt + geom_vline(
            data = df_plot,
            mapping = aes_string(xintercept = ".y", color = col_factors),
            size = chart_sizes[j] / size_scale_factor
          )
        }
        if (type == "ChartBar") {
          plt <- plt + geom_bar(
            data = df_plot,
            position = pos,
            stat = "identity",
            size = 0.5 * chart_sizes[j] / size_scale_factor,
            width = 0.5,
            color = default_color
          )
        }
        
        # Error bars
        if (".error" %in% colnames(df)) {
          plt <- plt +
            geom_errorbar(
              data = df_plot,
              aes_string(ymin = ".ymin", ymax = ".ymax"),
              size = 1,
              width = 0.2,
              color = default_color
            )
        }
        
        # Text labels
        if ("text_labels" %in% colnames(df)) {
          df_plot2 <- df_plot %>% filter(text_labels != "")
          if(nrow(df_plot2) > 0) {
            plt <- plt +
              geom_text(
                data = df_plot2,
                aes(label = text_labels),
                size = chart_sizes[j] / size_scale_factor,
                show.legend = FALSE
              )
          }
        }
      }
      
    } else {
      stop("This chart type is not supported.")
      
    }
    
    # Add colors
    palette_kind <- class(pl[[1]]$palette)[1]
    brks <-
      lapply(pl[[1]]$palette$doubleColorElements, "[[", "stringValue") %>%
      unlist() %>%
      as.double()
    
    if (palette_kind == "CategoryPalette") {
      
      cat_pal <- tercen_palette(pl, n = 32)
      n_color_levels <- df_plot %>% 
        select(all_of(unique(unlist(ctx$colors)))) %>% 
        unique() %>% 
        nrow()
      
      if(length(cat_pal) < n_color_levels) {
        cat_pal <- rep(cat_pal, 1L + n_color_levels %/% length(cat_pal))
      }
      
      plt <- plt +
        scale_colour_manual(
          values = cat_pal
        ) +
        scale_fill_manual(
          values = cat_pal
        )
    } else {
      if (length(brks) != 3) {
        plt <- plt +
          scale_color_gradientn(
            colours = tercen_palette(pl, n = 32),
            breaks = brks,
            limits = range(brks),
            oob = scales::squish
          ) +
          scale_fill_gradientn(
            colours = tercen_palette(pl, n = 32),
            breaks = brks,
            limits = range(brks),
            oob = scales::squish
          )
      } else {
        pal <-
          unlist(lapply(pl[[1]]$palette$doubleColorElements, "[[", "color")) %>% int_to_rgb()
        plt <- plt +
          scale_color_gradient2(
            low = pal[1],
            mid = pal[2],
            high = pal[3],
            midpoint = brks[2],
            limits = range(brks),
            oob = scales::squish
          ) +
          scale_fill_gradient2(
            low = pal[1],
            mid = pal[2],
            high = pal[3],
            midpoint = brks[2],
            limits = range(brks),
            oob = scales::squish
          )
      }
    }
    
    #####
    ### Axes ranges
    x_range <-
      as.numeric(trimws(strsplit(input.par$x_range, ",")[[1]]))
    y_range <-
      as.numeric(trimws(strsplit(input.par$y_range, ",")[[1]]))
    if (length(x_range) > 0)
      plt <- plt + xlim(x_range)
    if (length(y_range) > 0)
      plt <- plt + ylim(y_range)
    
    #####
    ### Facets based on rows and columns
    if (!any(chart_types == "ChartHeatmap")) {
      plt <-
        plt + get_facet_formula(ctx, input.par$wrap.1d, input.par$scales)
      xlab <- get_axis_labels(ctx, input.par$xlab, "x")
      ylab <- get_axis_labels(ctx, input.par$ylab, "y")
    } else {
      xlab <- get_axis_labels(ctx, input.par$xlab, "row")
      ylab <- get_axis_labels(ctx, input.par$ylab, "col")
    }
    
    if (input.par$rotate_x_axis)
      plt <-
      plt + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    #####
    ## Set theme
    plt <- plt +
      labs(
        title = input.par$title,
        subtitle = input.par$subtitle,
        caption = input.par$caption,
        x = xlab,
        y = ylab
      )
    
    if(input.par$legend.title != "") {
      if(!is.null(plt$labels$fill)) plt$labels$fill <- input.par$legend.title 
      if(!is.null(plt$labels$color)) plt$labels$color <- input.par$legend.title 
    }
    
    th <- get(paste0("theme_", input.par$theme))
    theme_set(th(base_size = input.par$base.size))
    
    if (input.par$flip)
      plt <- plt + coord_flip()
    
    #####
    ## Save plot file
    if (input.par$plot_type ==  "svg2") {
      input.par$plot_type <- "svg"
      device <- svg
    } else {
      device <- NULL
    }
    
    plt_files <- tim::save_plot(
      plt,
      type = input.par$plot_type,
      width = input.par$plot.width / 144,
      height = input.par$plot.height / 144,
      units = "in",
      dpi = 144,
      device = device
    )
    
    
    return(plt_files)
  }
