#' Data Process for plotting function
#'
#' Data process for plotting function, returns a list with two
#' data frame: one for plain lines, one for ribbons plotting (in a wide format)
#'
#' @param df a `data.frame` object containing the columns: `model_id`,
#' `output_type_id`, `target_date`, `value`
#' @param plain_line a `numeric` output_type_id value, value will be used to
#' create a plain line in the plot. Should be a unique value
#' (for example: 0.5)
#' @param plain_type a `string` output_type value, value will be used to
#' create a plain line in the plot. Should be a unique value
#' (for example: "quantile")
#' @param intervals a named list of `numeric`  output_type_id value, value will
#' be used a to create one or multiple prediction intervals in the plot.
#' @param x_col_name column name containing the date information for the x-axis.
#' By default, "target_date".
#'
#' @importFrom stats reshape
plot_prep_data <- function(df, plain_line, plain_type, intervals,
                           x_col_name = "target_date") {
  # Median
  if (is.null(plain_line)) {
    plain_df <- df[which(
      df$output_type_id == plain_line & df$output_type == plain_type), ]
  } else if (!is.na(plain_line)) {
    plain_df <- df[which(
      df$output_type_id == plain_line & df$output_type == plain_type), ]
  } else {
    plain_df <- df[which(
      is.na(df$output_type_id) & df$output_type == plain_type), ]
  }
  plain_df[[x_col_name]] <- as.Date(plain_df[[x_col_name]])
  # Intervals
  if (is.null(intervals)) {
    ribbon_list <- NULL
  } else {
    ribbon_list <- lapply(intervals, function(ribbon) {
      ribbon_df <- df[which(df$output_type_id %in% ribbon), ]
      ribbon_df <- transform(
        ribbon_df, output_type_id = ifelse(
          ribbon_df$output_type_id == min(ribbon), "min", "max"))
      id_col <- colnames(ribbon_df)[!colnames(ribbon_df) %in%
                                      c("output_type_id", "value")]
      ribbon_df <- reshape(
        ribbon_df, timevar = "output_type_id", direction = "wide",
        idvar = id_col)
      ribbon_df[[x_col_name]] <- as.Date(ribbon_df[[x_col_name]])
      colnames(ribbon_df) <- gsub("^value\\.", "" , colnames(ribbon_df))
      return(ribbon_df)
    })
    ribbon_list <- setNames(ribbon_list, names(intervals))
  }
  # List output
  return(c(list("median" = plain_df), ribbon_list))
}

#' Plot Truth data with Plotly
#'
#' Use Plotly to plot truth data
#'
#' @param plot_model a plot_ly object to add lines and/or ribbons, if NULL will
#'  create an empty object
#' @param truth_data a `data.frame` object containing the ground truth data,
#'  containing the columns: `time_idx` and `value`.
#'  Ignored, if `plot_truth = FALSE`
#' @param plot_truth a `boolean` for showing the truth data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter `truth_data`
#' @param show_legend a `boolean` for showing the legend in the plot.
#' @param arguments list of others Plotly parameters.
#' @param x_col_name column name containing the date information for the x-axis.
#' By default, "time_idx".
#'
#' @importFrom plotly add_trace layout
plotly_truth_data <- function(plot_model, truth_data, plot_truth, show_legend,
                              arguments, x_col_name = "time_idx") {
  if (plot_truth) {
    truth_data[[x_col_name]] <- as.Date(truth_data[[x_col_name]])
    arg_list <- list(
      p = plot_model, data = truth_data, x = truth_data[[x_col_name]],
      y = ~value, type = "scatter", mode = "lines+markers",
      line = list(color = "#6e6e6e"), hoverinfo = "text", name = "ground truth",
      legendgroup = "ground truth", hovertext =
        paste("Date: ", truth_data[[x_col_name]], "<br>Ground truth: ",
              format(truth_data$value, big.mark = ","), sep = ""),
      marker = list(color = "#6e6e6e", size = 7), showlegend = show_legend)
    arg_list <- c(arg_list, arguments)
    plot_model <- do.call(plotly::add_trace, arg_list)
  }
  plot_model <- plotly::layout(
    plot_model, xaxis = list(title = 'Date'), yaxis = list(title = 'Value'))
  return(plot_model)
}

#' Plot Truth data with GGplot
#'
#' Use ggplot2 to plot truth data
#'
#' @param plot_model a plot_ly object to add lines and/or ribbons, if NULL will
#'  create an empty object
#' @param truth_data a `data.frame` object containing the ground truth data,
#'  containing the columns: `time_idx` and `value`.
#'  Ignored, if `plot_truth = FALSE`
#' @param plot_truth a `boolean` for showing the truth data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter
#'  `truth_data`.
#' @param x_col_name column name containing the date information for the x-axis.
#'  By default, "time_idx".
#'
#' @importFrom ggplot2 geom_line geom_point aes .data
static_truth_data <- function(plot_model, truth_data, plot_truth,
                              x_col_name = "time_idx") {
  if (plot_truth) {
    truth_data[[x_col_name]] <- as.Date(truth_data[[x_col_name]])
    plot_model <- plot_model  +
      geom_line(data = truth_data,
                aes(x = .data[[x_col_name]],y = .data$value),
                color = "#6e6e6e", inherit.aes = FALSE) +
      geom_point(data = truth_data,
                 aes(x = .data[[x_col_name]], y = .data$value),
                 color = "#6e6e6e", inherit.aes = FALSE)
  }
  return(plot_model)
}

#' Plot Projection data with Plotly
#'
#' Use Plotly to plot projection model output
#'
#' @param plot_model a plot_ly object to add lines and/or ribbons, if NULL will
#'  create an empty object.
#' @param df_point a `data.frame` with "target_date" and "value" columns, use to
#'  add lines on the plot.
#' @param df_ribbon a `data.frame` with "target_date", "min", and "max" columns,
#'  use to add ribbons on the plot.
#' @param line_color a `string`, specific color associated with plot.
#' @param opacity a `numeric`, opacity of the ribbons, default 0.25.
#' @param arguments list of others Plotly parameters.
#' @param fill_by name of a column for specifying colors and legend in plot.
#' The `pal_color` parameter can be use to change the palette.
#' Default to `model_id`.
#' @param x_col_name column name containing the date information for the x-axis.
#' By default, "target_date".
#'
#' @importFrom plotly add_lines add_ribbons
plotly_proj_data <- function(plot_model, df_point, df_ribbon,
                             line_color, opacity, arguments,
                             fill_by = "model_id", x_col_name = "target_date") {
  if (nrow(df_point) > 0) {
    arg_list <- list(p = plot_model, data = df_point,
                     x = df_point[[x_col_name]], y = ~value,
                     legendgroup = df_point[[fill_by]],
                     name = df_point[[fill_by]],
                     hoverinfo = "text", hovertext = paste(
                       "Date: ", df_point[[x_col_name]], "<br>",
                       "Median: ", format(round(df_point$value, 2),
                                          big.mark = ","), sep = ""))
    if (is.null(line_color)) {
      arg_list <- c(arg_list, list(color =  df_point[[fill_by]]), arguments)
      plot_model <- do.call(plotly::add_lines, arg_list)
      plot_model <- plotly::layout(
        plot_model, xaxis = list(title = 'Date'),
        yaxis = list(title = 'Value'))
    } else {
      arg_list <- c(arg_list, list(line = list(color = line_color)), arguments)
      plot_model <- do.call(plotly::add_lines, arg_list)
      plot_model <- plotly::layout(
        plot_model, xaxis = list(title = 'Date'),
        yaxis = list(title = 'Value'))
    }
    show_legend = FALSE
  } else {
    if (exists("arguments")) {
      show_legend <- arguments$showlegend
      if (is.null(show_legend)) show_legend <- TRUE
    } else {
      show_legend = TRUE
    }
  }

  if (!is.null(df_ribbon)) {
    for (n_rib in seq_along(df_ribbon)) {
      df_rib <- df_ribbon[[n_rib]]
      if (n_rib > 1) show_legend <- FALSE
      if (nrow(df_rib) > 0) {
        arg_list <- list(plot_model, data = df_rib, x = df_rib[[x_col_name]],
                         ymin = ~min, ymax = ~max, opacity = opacity,
                         showlegend = show_legend, name = df_rib[[fill_by]],
                         legendgroup = df_rib[[fill_by]], hoverinfo = "text",
                         hovertext = paste(
                           "Date: ", df_rib[[x_col_name]], "<br>",
                           scales::percent(as.numeric(names(df_ribbon)[n_rib])),
                           "Intervals: ",
                           format(round(df_rib$min, 2), big.mark = ","), " - ",
                           format(round(df_rib$max, 2), big.mark = ","),
                           sep = ""))
        if (is.null(line_color)) {
          arg_list <- c(
            arg_list, list(color = df_rib[[fill_by]], line = list(width = 0)),
            arguments)
          plot_model <- do.call(plotly::add_ribbons, arg_list)
        } else {
          arg_list <- c(
            arg_list, list(fillcolor = line_color,
                           line = list(width = 0, color = line_color)),
            arguments)
          plot_model <- do.call(plotly::add_ribbons, arg_list)
          plot_model <- plotly::layout(
            plot_model, xaxis = list(title = 'Date'),
            yaxis = list(title = 'Value'))
        }
      }
    }
  }
  return(plot_model)
}

#' Plot Projection data with GGPLOT2
#'
#' Use ggplot2 to plot projection model output
#'
#' @param plot_model a plot_ly object to add lines and/or ribbons, if NULL will
#'  create an empty object.
#' @param df_point a `data.frame` with "target_date" and "value" columns, use to
#'  add lines on the plot.
#' @param df_ribbon a `data.frame` with "target_date", "min", and "max" columns,
#'  use to add ribbons on the plot.
#' @param line_color a `string`, specific color associated with plot.
#' @param opacity a `numeric`, opacity of the ribbons, default 0.25.
#' @param fill_by name of a column for specifying colors and legend in plot.
#' The `pal_color` parameter can be use to change the palette.
#' Default to `model_id`.
#' @param x_col_name column name containing the date information for the x-axis.
#' By default, "target_date".
#'
#' @importFrom ggplot2 geom_ribbon
#' @importFrom purrr map
static_proj_data <- function(plot_model, df_point, df_ribbon,
                             line_color, opacity, fill_by = "model_id",
                             x_col_name = "target_date") {

  if (!is.null(df_ribbon)) {
    for (fill in unique(unlist(purrr::map(df_ribbon, fill_by)))) {
      for (n_rib in seq_along(df_ribbon)) {
        df_rib <- df_ribbon[[n_rib]]
        df_rib_mod <- df_rib[which(df_rib[[fill_by]] == fill), ]
        if (nrow(df_rib) > 0) {
          plot_model <- plot_model +
            geom_ribbon(data = df_rib_mod,
                        aes(.data[[x_col_name]], ymin = .data$min,
                            ymax = .data$max, fill = .data[[fill_by]]),
                        alpha = opacity, inherit.aes = FALSE)
        }
      }
    }
  }

  if (nrow(df_point) > 0) {
    plot_model <- plot_model  +
      geom_line(data = df_point, aes(x = .data[[x_col_name]], y = .data$value,
                                     color = .data[[fill_by]]),
                inherit.aes = FALSE, linewidth = 1)
  }

  return(plot_model)
}


#' Plot simple projection data
#'
#' Use Plotly or ggplot2 to plot simple projection model output with or
#' without truth data.
#' Simple projection model output are defined as projection associated with one
#' particular set of "tasks_ids" value. For more information, please refer to
#' [HubDocs website](https://hubdocs.readthedocs.io/en/latest/format/tasks.html).
#'
#' @param plot_model a plot_ly object to add lines and/or ribbons, if NULL will
#'  create an empty object.
#' @param df_point a `data.frame` with a column containing date information
#' (`x_col_name` parameter) and "value" columns, use to add lines on the plot.
#' @param df_ribbon a `data.frame` with a column containing date information
#' (`x_col_name` parameter), and a "min", and "max" columns, use to add ribbons
#' on the plot.
#' @param plot_truth a `boolean` for showing the truth data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter `truth_data`
#' @param truth_data a `data.frame` object containing the ground truth data,
#'  containing the columns: date information (`x_truth_col_name` parameter) and
#'  `value`. Ignored, if `plot_truth = FALSE`.
#' @param opacity a `numeric`, opacity of the ribbons, default 0.25.
#' @param line_color a `string`, specific color associated with plot.
#' @param top_layer character vector, where the first element indicates the top
#'  layer of the resulting plot. Possible options are `"model_output"` (default)
#'  and `"truth"`.
#' @param show_truth_legend a `boolean` to show legend of the truth data, by
#'  default `TRUE`.
#' @param interactive a `boolean` to output an "interactive" version of the
#'  plot (using Plotly) or a "static" plot (using ggplot2). By default, `TRUE`
#'  (interactive plot).
#' @param ... additional Plotly parameters.
#' @param fill_by name of a column for specifying colors and legend in plot.
#' The `pal_color` parameter can be use to change the palette.
#' Default to `model_id`.
#' @param x_col_name column name containing the date information for `df_point`
#' and `df_ribbon` data frames, value will be map to the x-axis of the plot.
#' By default, "target_date".
#' @param x_truth_col_name  column name containing the date information for
#' `truth_data` data frame, value will be map to the x-axis of the plot.
#' By default, "time_idx".
#'
#' @importFrom plotly plot_ly
simple_model_plot <- function(
    plot_model, df_point, df_ribbon, plot_truth, truth_data, opacity = 0.25,
    line_color = NULL, top_layer = "model_output", show_truth_legend = TRUE,
    interactive = TRUE, fill_by = "model_id", x_col_name = "target_date",
    x_truth_col_name = "time_idx", ...) {
  # prerequisite
  if (is.null(plot_model)) {
    if (interactive) {
      plot_model <- plotly::plot_ly()
    } else {
      plot_model <- ggplot2::ggplot()
    }

  }
  arguments <- list(...)

  if (top_layer == "model_output") {
    if (interactive) {
      # Truth Data
      plot_model <- plotly_truth_data(plot_model, truth_data, plot_truth,
                                      show_truth_legend, arguments,
                                      x_col_name = x_truth_col_name)
      # Projection data
      plot_model <- plotly_proj_data(plot_model, df_point, df_ribbon,
                                     line_color, opacity, arguments,
                                     fill_by = fill_by, x_col_name = x_col_name)
    } else {
      # Truth Data
      plot_model <- static_truth_data(plot_model, truth_data, plot_truth,
                                      x_col_name = x_truth_col_name)
      # Projection data
      plot_model <- static_proj_data(plot_model, df_point, df_ribbon,
                                     line_color, opacity, fill_by = fill_by,
                                     x_col_name = x_col_name)
    }

  } else if (top_layer == "truth") {
    if (interactive) {
      # Projection data
      plot_model <- plotly_proj_data(plot_model, df_point, df_ribbon,
                                     line_color, opacity, arguments,
                                     fill_by = fill_by, x_col_name = x_col_name)
      # Truth Data
      plot_model <- plotly_truth_data(plot_model, truth_data, plot_truth,
                                      show_truth_legend, arguments,
                                      x_col_name = x_truth_col_name)
    } else {
      # Projection data
      plot_model <- static_proj_data(plot_model, df_point, df_ribbon,
                                     line_color, opacity,
                                     fill_by = fill_by, x_col_name = x_col_name)
      # Truth Data
      plot_model <- static_truth_data(plot_model, truth_data, plot_truth,
                                      x_col_name = x_truth_col_name)
    }
  }

  return(plot_model)
}

#' Plot projection data
#'
#' Use Plotly or ggplot2 to plot projection model output with or without
#' truth data.
#'
#' @param all_plot a list with two data frame: one for plain lines,
#' one for ribbons plotting (in a wide format)
#' @param all_ens a list with two data frame: one for plain lines,
#'  one for ribbons plotting (in a wide format) for a unique `model_id` value
#'  associated with specific color (`ens_color`). NULL is no specific layout
#'  required
#' @param truth_data a `data.frame` object containing the ground truth data,
#'  containing the columns: date information (`x_truth_col_name` parameter) and
#'  `value`. Ignored, if `plot_truth = FALSE`.
#' @param plot_truth a `boolean` for showing the truth data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter `truth_data`
#' @param intervals a vector of `numeric` values indicating which central
#'  prediction interval levels to plot. `NULL` means no interval levels.
#'  If not provided, it will default to `c(.5, .8, .95)`.
#'  When plotting 6 models or more, the plot will be reduced to show `.95`
#'  interval only. Value possibles: `0.5, 0.8, 0.9, 0.95`
#' @param pal_color a `character` string for specifying the palette color in the
#'  plot. For `plotly` plots, please refer to
#'  [RColorBrewer::display.brewer.all()]. Default to `"Set2"`
#' @param fill_transparency numeric value used to set transparency of intervals.
#'  0 means fully transparent, 1 means opaque. Default to `0.25`
#' @param pal_value  a `named vector` containing the `model_id` (names) and
#'  associated color. Default `NULL`, used only for static plot (ggplot2)
#' @param top_layer character vector, where the first element indicates the top
#'  layer of the resulting plot. Possible options are `"model_output"` (default)
#'  and `"truth"`
#' @param ens_color a `character` string of a color name, if not NULL, will be
#' use as color for the model name associated with the parameter `ens_name`
#' @param facet a unique value corresponding as a task_id variable name
#' (interpretable as facet option for ggplot)
#' @param facet_scales argument for scales as in [ggplot2::facet_wrap] or
#'  equivalent to `shareX`, `shareY` in [plotly::subplot]. Default to "fixed"
#'  (x and y axes are shared).
#' @param facet_nrow a numeric, number of rows in the layout.
#' @param facet_ncol a numeric, number of columns in the layout.
#' @param facet_title a `string`, position of each subplot tile (value
#'  associated with the `facet` parameter). "top right", "top left" (default),
#'  "bottom right", "bottom left" are the possible values, `NULL` to remove the
#'  title. For interactive plot only.
#' @param facet_value a vector of all the possible unique values in the
#'  associated column `facet`
#' @param interactive a `boolean` to output an "interactive" version of the
#'  plot (using Plotly) or a "static" plot (using ggplot2). By default, `TRUE`
#'  (interactive plot)
#' @param fill_by name of a column for specifying colors and legend in plot.
#' The `pal_color` parameter can be use to change the palette.
#' Default to `model_id`.
#' @param x_col_name column name containing the date information for `all_plot`
#' and `all_ens` data frames, value will be map to the x-axis of the plot.
#' By default, "target_date".
#' @param x_truth_col_name  column name containing the date information for
#' `truth_data` data frame, value will be map to the x-axis of the plot.
#' By default, "time_idx".
#'
#' @importFrom plotly plot_ly layout subplot
#' @importFrom ggplot2 ggplot scale_color_manual scale_fill_manual facet_wrap
output_plot <-  function(all_plot, all_ens, truth_data, plot_truth = TRUE,
                         intervals = c(.5, .8, .95), pal_color = "Set2",
                         fill_transparency = 0.25, pal_value = NULL,
                         top_layer = "model_output", ens_color = NULL,
                         facet = NULL, facet_scales = "fixed",
                         facet_nrow = NULL, facet_ncol = NULL,
                         facet_title = "top left", facet_value = NULL,
                         interactive = TRUE, fill_by = "model_id",
                         x_col_name = "target_date",
                         x_truth_col_name = "time_idx") {
  if (interactive) {
    plot_model <- plotly::plot_ly(colors =  pal_color)
  } else {
    plot_model <- ggplot2::ggplot(colors =  pal_color)
  }

  if (!is.null(facet) & interactive) {
    sharex = FALSE
    sharey = FALSE
    if (facet_scales == "fixed") {
      sharex = TRUE
      sharey = TRUE
    } else if (facet_scales == "free_x") {
      sharey = TRUE
    } else if (facet_scales == "free_y") {
      sharex = TRUE
    }
    if (is.null(facet_nrow)) {
      facet_nrow = 1
    }
    subplot <- lapply(facet_value, function(x) {
      # Data preparation
      df_point <- all_plot$median[which(all_plot$median[[facet]] == x), ]
      df_ribbon <- all_plot[names(all_plot) %in% intervals]
      df_ribbon <- setNames(lapply(df_ribbon, function(df_rib) {
        df_rib[which(df_rib[[facet]] == x), ]
      }), names(df_ribbon))
      if (!is.null(all_ens)) {
        df_point_ens <- all_ens$median[which(all_ens$median[[facet]] == x), ]
        df_ribbon_ens <- all_ens[names(all_ens) %in% intervals]
        df_ribbon_ens <- setNames(lapply(df_ribbon_ens, function(df_rib) {
          df_rib[which(df_rib[[facet]] == x), ]
        }), names(df_ribbon_ens))
      }
      if (plot_truth & facet %in% colnames(truth_data)) {
        truth_data <- truth_data[which(truth_data[[facet]] == x), ]
      }

      # Plot
      if (x == facet_value[1]) {
        plot_model <- simple_model_plot(
          plot_model, df_point, df_ribbon, plot_truth, truth_data,
          opacity = fill_transparency, top_layer = top_layer,
          interactive = TRUE, fill_by = fill_by, x_col_name = x_col_name,
          x_truth_col_name = x_truth_col_name)
      } else if (facet == fill_by) {
         plot_model <- simple_model_plot(
           plot_model, df_point, df_ribbon, plot_truth, truth_data,
           opacity = fill_transparency, top_layer = top_layer,
           show_truth_legend = FALSE, interactive = TRUE,
           fill_by = fill_by, x_col_name = x_col_name,
           x_truth_col_name = x_truth_col_name)
      } else {
        plot_model <- simple_model_plot(
          plot_model, df_point, df_ribbon, plot_truth, truth_data,
          opacity = fill_transparency, showlegend = FALSE,
          top_layer = top_layer, show_truth_legend = FALSE,
          interactive = TRUE, fill_by = fill_by, x_col_name = x_col_name,
          x_truth_col_name = x_truth_col_name)
      }
      # Ensemble color
      if (!is.null(all_ens)) {
        if (x == facet_value[1]) {
          plot_model <- simple_model_plot(
            plot_model, df_point_ens, df_ribbon_ens, FALSE, truth_data,
            line_color = ens_color, opacity = fill_transparency,
            top_layer = top_layer, interactive = TRUE, fill_by = fill_by,
            x_col_name = x_col_name, x_truth_col_name = x_truth_col_name)
        } else if (facet == fill_by) {
          plot_model <- simple_model_plot(
            plot_model, df_point_ens, df_ribbon_ens, TRUE, truth_data,
            line_color = ens_color, opacity = fill_transparency,
            top_layer = top_layer, show_truth_legend = FALSE,
            interactive = TRUE, fill_by = fill_by, x_col_name = x_col_name,
            x_truth_col_name = x_truth_col_name)
        } else {
          plot_model <- simple_model_plot(
            plot_model, df_point_ens, df_ribbon_ens, FALSE, truth_data,
            line_color = ens_color, opacity = fill_transparency,
            showlegend = FALSE, top_layer = top_layer,
            show_truth_legend = FALSE, interactive = TRUE, fill_by = fill_by,
            x_col_name = x_col_name, x_truth_col_name = x_truth_col_name)
        }
      }
      if (!is.null(facet_title)) {
        if (grepl("top", facet_title)) {
          y_title <- 1
          y_anchor <- "top"
        } else if  (grepl("bottom", facet_title)) {
          y_title <- 0
          y_anchor <- "bottom"
        }
        if (grepl("left", facet_title)) {
          x_title <- 0
          x_anchor <- "left"
        } else if  (grepl("right", facet_title)) {
          x_title <- 1
          x_anchor <- "right"
        }
        plot_model <- plotly::layout(
          plot_model,
          annotations = list(x = x_title, y = y_title, xref = "paper",
                             yref = "paper", xanchor = x_anchor,
                             yanchor = y_anchor, showarrow = FALSE, text = x))
      }
      return(plot_model)
    })
    plot_model <- plotly::subplot(subplot, nrows = facet_nrow, shareX = sharex,
                                  shareY = sharey)
    if (facet == fill_by) {
      for (i in seq_along(plot_model$x$data)) {
        if (purrr::map(plot_model$x$data, "name")[[i]] %in% facet_value) {
          plot_model$x$data[[i]]$fillcolor <-
            plot_model$x$data[[i]]$line$color <-
            pal_value[purrr::map(plot_model$x$data, "name")[[i]]]
        }
      }
    }
  } else {
    df_point <- all_plot$median
    df_ribbon <- all_plot[names(all_plot) %in% intervals]
    if (!is.null(all_ens)) {
      df_point_ens <- all_ens$median
      df_ribbon_ens <- all_ens[names(all_ens) %in% intervals]
    }
    plot_model <- simple_model_plot(
      plot_model, df_point, df_ribbon, plot_truth, truth_data,
      opacity = fill_transparency, top_layer = top_layer, fill_by = fill_by,
      interactive = interactive, x_col_name = x_col_name,
      x_truth_col_name = x_truth_col_name)

    # Ensemble color
    if (!is.null(all_ens)) {
      plot_model <- simple_model_plot(
        plot_model, df_point_ens, df_ribbon_ens, truth_data = truth_data,
        plot_truth = FALSE, opacity = fill_transparency, line_color = ens_color,
        top_layer = top_layer, interactive = interactive, fill_by = fill_by,
        x_col_name = x_col_name, x_truth_col_name = x_truth_col_name)
    }
    if (!is.null(facet)) {
      plot_model <-  plot_model +
        ggplot2::facet_wrap(facet, nrow = facet_nrow, ncol = facet_ncol,
                            scales = facet_scales)
    }
  }

  if (!interactive) {
    plot_model <- plot_model +
      scale_color_manual(values = pal_value, name = "Legend") +
      scale_fill_manual(values = pal_value, name = "Legend")
  }
  return(plot_model)
}


#' Basic Plot for model outputs
#'
#' Create a simple Plotly time-series plot for model projection outputs.
#'
#'@param model_output_data a `model_out_tbl` object, containing all the required
#' columns, a column containing date information (`x_col_name` parameter) and
#' a column `value`.
#'@param truth_data a `data.frame` object containing the ground truth data,
#' with a column containing date information (`x_truth_col_name` parameter) and
#' a column `value`. Ignored, if `plot_truth = FALSE`.
#'@param use_median_as_point a `Boolean` for using median quantile as point
#' in plot. Default to FALSE. If TRUE, will select first any `median`
#' output type value and if no `median` value included in `model_output_data`;
#' will select `quantile = 0.5` output type value.
#'@param show_plot a `boolean` for showing the plot. Default to TRUE.
#'@param plot_truth a `boolean` for showing the truth data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter `truth_data`
#'@param show_legend a `boolean` for showing the legend in the plot.
#'  Default to TRUE.
#'@param facet a unique value corresponding as a task_id variable name
#' (interpretable as facet option for ggplot)
#'@param facet_scales argument for scales as in [ggplot2::facet_wrap] or
#' equivalent to `shareX`, `shareY` in [plotly::subplot]. Default to "fixed"
#' (x and y axes are shared).
#'@param facet_nrow a numeric, number of rows in the layout.
#'@param facet_ncol a numeric, number of columns in the layout
#' (ignored in [plotly::subplot])
#'@param facet_title a `string`, position of each subplot tile (value
#' associated with the `facet` parameter). "top right", "top left" (default),
#' "bottom right", "bottom left" are the possible values, `NULL` to remove the
#' title. For interactive plot only.
#'@param interactive a `boolean` to output an "interactive" version of the
#'  plot (using Plotly) or a "static" plot (using ggplot2). By default, `TRUE`
#'  (interactive plot)
#'@param fill_by name of a column for specifying colors and legend in plot.
#' The `pal_color` parameter can be use to change the palette.
#' Default to `model_id`.
#'@param pal_color a `character` string for specifying the palette color in the
#' plot. Please refer to [RColorBrewer::display.brewer.all()]. If `NULL`,
#' only `one_color` parameter will be used for all models. Default to `"Set2"`
#' @param one_color a `character` string for specifying the color in the
#' plot if `pal_color` is set to `NULL`. Please refer
#' to [colors()] for accepted color names. Default to `"blue"`
#'@param fill_transparency numeric value used to set transparency of intervals.
#' 0 means fully transparent, 1 means opaque. Default to `0.25`
#'@param intervals a vector of `numeric` values indicating which central
#' prediction interval levels to plot. `NULL` means no interval levels.
#' If not provided, it will default to `c(.5, .8, .95)`.
#' When plotting 6 models or more, the plot will be reduced to show `.95`
#' interval only. Value possibles: `0.5, 0.8, 0.9, 0.95`
#'@param top_layer character vector, where the first element indicates the top
#'  layer of the resulting plot. Possible options are `"model_output"` (default)
#'  and `"truth"`
#'@param title a `character` string, if not NULL, will be added as title to the
#' plot
#'@param ens_color a `character` string of a color name, if not NULL, will be
#' use as color for the model name associated with the parameter `ens_name`
#' (both parameter need to be provided)
#'@param ens_name a `character` string of a model name, if not NULL, will be
#' use to change the color for the model name, associated with the parameter
#' `ens_color`(both parameter need to be provided)
#'@param x_col_name column name containing the date information for `all_plot`
#' and `all_ens` data frames, value will be map to the x-axis of the plot.
#' By default, "target_date".
#'@param x_truth_col_name  column name containing the date information for
#' `truth_data` data frame, value will be map to the x-axis of the plot.
#' By default, "time_idx".
#'
#' @importFrom cli cli_abort cli_warn
#' @importFrom scales percent
#' @importFrom methods show
#' @importFrom stats setNames
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices col2rgb rgb colors
#' @importFrom ggplot2 labs guides
#'
#' @export
#'
plot_step_ahead_model_output <- function(
    model_output_data, truth_data, use_median_as_point = FALSE, show_plot = TRUE,
    plot_truth = TRUE, x_col_name = "target_date",
    x_truth_col_name = "time_idx", show_legend = TRUE, facet = NULL,
    facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL,
    facet_title = "top left", interactive = TRUE, fill_by = "model_id",
    pal_color = "Set2", one_color = "blue", fill_transparency = 0.25,
    intervals = c(.5, .8, .95), top_layer = "model_output", title = NULL,
    ens_color = NULL, ens_name = NULL) {

  # Test format input
  ## Model Output data
  if (!is.data.frame(model_output_data)) {
    cli::cli_abort(c("x" = "{.arg model_output_data} must be a `data.frame`."))
  }
  if (isFALSE("model_out_tbl" %in% class(model_output_data))) {
    cli::cli_warn(c("!" = "{.arg model_output_data} must be a `model_out_tbl`.
                    Class applied by default"))
    model_output_data <- hubUtils::as_model_out_tbl(model_output_data,
                                                    remove_empty = TRUE)
  }
  exp_f_col <- unique(c("model_id", "output_type_id", x_col_name, "value",
                        fill_by))
  model_output_col <- colnames(model_output_data)
  if (!all(exp_f_col %in% model_output_col)) {
    cli::cli_abort(c("x" = "{.arg model_output_data} did not have all required
                     columns {.val {exp_f_col}}"))
  }
  valid_types <- c("median", "quantile")
  model_output_type <- unique(model_output_data$output_type)
  if (!any(valid_types %in% model_output_type)) {
    cli::cli_abort(c(
      "x" = "{.arg model_output_data} should contain at least one supported
      output type.",
      "i" = "Supported output types: {.val {valid_types}}."
    ))
  }

  ## Truth Data
  if (plot_truth) {
    if (!is.data.frame(truth_data)) {
      cli::cli_abort(c("x" = "{.arg truth_data} must be a `data.frame`."))
    }
    exp_td_col <- c(x_truth_col_name, "value")
    truth_data_col <- colnames(truth_data)
    if (!all(exp_td_col %in% truth_data_col)) {
      cli::cli_abort(c("x" = "{.arg truth_data} did not have all required
                     columns {.val {exp_td_col}}"))
    }
  }
  ## Parameters
  ### Intervals
  list_intervals <- list(
    "0.95" = c(0.975, 0.025), "0.9" = c(0.95, 0.05),
    "0.8" = c(0.9, 0.1), "0.5" = c(0.75, 0.25)
  )
  if (!is.null(intervals)) {
    intervals <- as.character(intervals)
    if (any(!intervals %in% names(list_intervals))) {
      cli::cli_warn(c("!" = "{.arg intervals} should correspond to one or
                      multiple of these possible values
                      {.val {names(list_intervals)}}.
                      Only the matching value(s) will be used (if no matching
                      value, the default will be used)."))
      intervals <- intervals[intervals %in% names(list_intervals)]
      if (length(intervals) == 0) {
        intervals <- as.character(c(.5, .8, .95))
      }
    }
    if (length(unique(model_output_data[["model_id"]])) > 5 &
        length(intervals) > 1) {
      intervals <- max(intervals)[1]
      cli::cli_warn(c("!" = "{.arg model_output_data} contains 6 or more models,
                      the plot will be reduced to show only one interval (the
                      maximum interval value): {.val {intervals}}"))
    }
    ribbon <- list_intervals[as.character(sort(intervals, decreasing = TRUE))]
  } else {
    ribbon <- NULL
  }

  ### Median
  if (isTRUE(use_median_as_point)) {
    if (any(grepl("median", model_output_data$output_type))) {
      plain_line <- NA
      plain_type <- "median"
    } else {
      plain_line <- 0.5
      plain_type <- "quantile"
    }
  } else {
    plain_line <- NULL
    plain_type <- NULL
  }
  exp_value <- c(plain_line, unlist(ribbon))
  model_output_type_val <- unique(model_output_data$output_type_id)
  if (!all(exp_value %in% model_output_type_val)) {
    cli::cli_abort(c("x" = "{.arg model_output_type_val} did not have the
                     expected output_type_id value {.val {exp_value}}"))
  }
  ### Ensemble specific color
  if (is.null(ens_color) + is.null(ens_name) == 1) {
    cli::cli_abort(c("x" = "Both {.arg ens_color} and {.arg ens_name} should
                     be set to a non NULL value"))
  }
  ### Facet
  if (!is.null(facet)) {
    if ((length(facet) != 1) |
        !(facet %in% grep("output_type|value", colnames(model_output_data),
                          value = TRUE, invert = TRUE))) {
      cli::cli_abort(c("x" = "if {.arg facet} is not NULL, the argument should
                       be of length 1 and should match one of the task_id column
                       of {.arg model_output_data}"))
    }
  }
  if (!is.null(facet_title)) {
    facet_title_opt <- c("top right", "top left", "bottom right", "bottom left")
    if (!facet_title %in% facet_title_opt) {
      cli::cli_abort(c("x" = "{.arg facet_title} should correspond to one of
                       these possible values: {.val {facet_title_opt}}"))
    }
  }
  #### Top layer
  if (!any(top_layer %in% c("model_output", "truth"))) {
    cli::cli_abort(c("x" = "{.arg top_layer} should correspond to one of
                       these possible values: {.val model_output},
                     {.val truth}"))
  }

  #### Palette
  fill_by_vect <- unique(model_output_data[[fill_by]])
  if (!is.null(pal_color)) {
    if (!pal_color %in% row.names(RColorBrewer::brewer.pal.info)) {
      cli::cli_warn(c("!" = "{.arg pal_color} is not one of the accepted palette
                       name, accepted values are:
                      {.val {row.names(RColorBrewer::brewer.pal.info)}}.
                      {.val Set2} used by default."))
      pal_color <- "Set2"
    }
    if (length(fill_by_vect) < 3) {
      n_pal <- 3
    } else {
      n_pal <- length(fill_by_vect)
    }
    pal_value <- RColorBrewer::brewer.pal(n_pal, pal_color)
  } else {
    if (!one_color %in% colors()) {
      cli::cli_warn(c("!" = "{.arg one_color} is not one of the accepted color
                       name, accepted values are: {.val {colors()}}.
                      {.val blue} used by default."))
      one_color <- "blue"
    }
    pal_color = one_color
    pal_value <- rep(pal_color, length(fill_by_vect))
  }
  names(pal_value) <- fill_by_vect
  if (!is.null(ens_color) & !is.null(ens_name))
    pal_value[ens_name] <- grDevices::rgb(
      grDevices::col2rgb(ens_color)[1], grDevices::col2rgb(ens_color)[2],
      grDevices::col2rgb(ens_color)[3])
  if (plot_truth) {
    pal_value <- c(pal_value, "Truth Data" = "#6e6e6e")
  }


  # Data process
  if (!is.null(ens_color) & !is.null(ens_name)) {
    ens_df <- model_output_data[which(model_output_data$model_id == ens_name), ]
    all_ens <- plot_prep_data(ens_df, plain_line, plain_type, ribbon,
                              x_col_name = x_col_name)
    plot_df <- model_output_data[which(
      model_output_data$model_id != ens_name), ]
  } else {
    all_ens <- NULL
    plot_df <- model_output_data
  }
  all_plot <- plot_prep_data(plot_df, plain_line, plain_type, ribbon,
                             x_col_name = x_col_name)

  # Plot
  if (!is.null(facet)) {
    facet_value = sort(unique(model_output_data[[facet]]))
  } else {
    facet_value = NULL
  }
  plot_model <- output_plot(all_plot, all_ens, truth_data,
                            plot_truth = plot_truth,
                            intervals =  intervals, pal_color = pal_color,
                            fill_transparency = fill_transparency,
                            pal_value = pal_value, top_layer = top_layer,
                            ens_color = ens_color, facet = facet,
                            facet_scales = facet_scales,
                            facet_nrow = facet_nrow,  facet_title = facet_title,
                            facet_value = facet_value,
                            interactive = interactive, fill_by = fill_by,
                            x_col_name = x_col_name,
                            x_truth_col_name = x_truth_col_name)
  # Layout
  if (interactive) {
    plot_model <- plotly::layout(
      plot_model, xaxis = list(title = 'Date'), yaxis = list(title = 'Value'),
      showlegend = show_legend)
    if (!is.null(title)) {
      plot_model <- plotly::layout(plot_model, title = title)
    }
  } else {
    plot_model <- plot_model + labs(x =  "Date", y = "Value")
    if (!is.null(title)) {
      plot_model <- plot_model + labs(title = title)
    }
    if (isFALSE(show_legend)) {
      plot_model <- plot_model + guides(fill = "none", color = "none")
    }
  }

  if (isTRUE(show_plot)) {
    if (interactive) show(plot_model)
    return(plot_model)
  } else {
    invisible(plot_model)
  }

}
