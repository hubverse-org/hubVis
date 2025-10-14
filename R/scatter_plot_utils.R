#' Data Process for plotting function
#'
#' Data process for plotting function, returns a list with two
#' data frame: one for plain lines, one for ribbons plotting (in a wide format)
#'
#' @param df a `data.frame` object containing the columns: `model_id`,
#' `output_type_id`, `target_date`, `value`
#' @param plain_line a unique `numeric` output_type_id value, value will be
#' used to create a plain line in the plot (for example: `0.5`).
#' @param plain_type a `string` output_type value, value will be used to
#' create a plain line in the plot. Should be a unique value
#' (for example: "quantile")
#' @param intervals a named list of `numeric`  output_type_id value, value will
#' be used a to create one or multiple prediction intervals in the plot.
#' @param x_col_name column name containing the date information for the x-axis.
#' By default, "target_date".
#'
#' @noRd
#' @importFrom dplyr near
#' @importFrom cli cli_warn
#' @importFrom stats reshape setNames na.omit
#' @importFrom hubUtils convert_output_type
plot_prep_data <- function(df, plain_line, plain_type, intervals,
                           x_col_name = "target_date", fill_by = "model_id") {
  # Factorize the fill_by column
  df[[fill_by]] <- as.factor(df[[fill_by]])

  # Create quantiles if necessary
  quant <- c(stats::na.omit(plain_line), unlist(intervals))
  if (!is.null(quant) && !("quantile" %in% df$output_type) &&
        ("sample" %in% df$output_type)) {
    df_sample <- dplyr::filter(df, .data[["output_type"]] == "sample")
    quant_df <- hubUtils::convert_output_type(df_sample,
                                              to = list("quantile" = quant))
    df <- rbind(df, quant_df)
  }

  # Median
  if (is.null(plain_line)) {
    plain_df <- data.frame(matrix(nrow = 0, ncol = length(colnames(df)))) |>
      setNames(colnames(df)) |>
      dplyr::as_tibble()
  } else if (!is.na(plain_line)) {
    plain_df <- df[which(df$output_type_id == plain_line &
                           df$output_type == plain_type), ]
  } else {
    plain_df <- df[which(is.na(df$output_type_id) &
                           df$output_type == plain_type), ]
  }
  plain_df[[x_col_name]] <- as.Date(plain_df[[x_col_name]])

  # Remove empty column to avoid issue
  empty_cols <- sapply(df, function(k) all(is.na(k)))
  if (any(empty_cols)) {
    empty_colnames <- colnames(df)[sapply(df, function (k) all(is.na(k)))] # nolint
    cli::cli_warn(c("!" = "{.arg model_out_tbl} contains some empty
                    columns: {.value {empty_colnames}.}"))
    df <- df[!empty_cols]
  }

  # Intervals & samples
  if (is.null(intervals)) {
    ribbon_list <- NULL
    sample_df <- dplyr::filter(df, .data[["output_type"]] == "sample")
  } else {
    ribbon_list <- lapply(intervals, function(ribbon) {
      ribbon_df <- df[which(df$output_type_id %in% ribbon), ] |>
        dplyr::mutate(output_type_id = as.numeric(.data[["output_type_id"]]))
      ribbon_df <-
        transform(ribbon_df,
                  output_type_id = ifelse(dplyr::near(ribbon_df$output_type_id,
                                                      min(ribbon)), "min",
                                          "max"))
      id_col <- colnames(ribbon_df)[!colnames(ribbon_df) %in%
                                      c("output_type_id", "value")]
      ribbon_df <- reshape(ribbon_df, timevar = "output_type_id",
                           direction = "wide", idvar = id_col)
      ribbon_df[[x_col_name]] <- as.Date(ribbon_df[[x_col_name]])
      colnames(ribbon_df) <- gsub("^value\\.", "", colnames(ribbon_df))
      ribbon_df
    })
    ribbon_list <- stats::setNames(ribbon_list, names(intervals))
    sample_df <- NULL
  }

  # List output
  return(c(list("median" = plain_df), ribbon_list, list("sample" = sample_df)))
}

#' Plot Target data with Plotly
#'
#' Use Plotly to plot target data
#'
#' @param plot_model a plot_ly object to add lines and/or ribbons, if NULL will
#'  create an empty object
#' @param target_data a `data.frame` object containing the target data,
#'  containing the columns: `date` and `observation`.
#'  Ignored, if `plot_target = FALSE`
#' @param plot_target a `boolean` for showing the target data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter
#'  `target_data`
#' @param show_legend a `boolean` for showing the legend in the plot.
#' @param arguments list of others Plotly parameters.
#' @param x_col_name column name containing the date information for the x-axis.
#' By default, "date".
#'
#' @noRd
#' @importFrom plotly add_trace layout
plotly_target_data <- function(plot_model, target_data, plot_target,
                               show_legend, arguments,
                               x_col_name = "date") {
  if (plot_target) {
    target_data[[x_col_name]] <- as.Date(target_data[[x_col_name]])
    arg_list <- list(p = plot_model, data = target_data,
                     x = target_data[[x_col_name]], y = ~observation,
                     type = "scatter", mode = "lines+markers",
                     line = list(color = "#6e6e6e"), hoverinfo = "text",
                     name = "target", legendgroup = "target",
                     hovertext = paste("Date: ", target_data[[x_col_name]],
                                       "<br>target: ",
                                       format(target_data$observation,
                                              big.mark = ","), sep = ""),
                     marker = list(color = "#6e6e6e", size = 7),
                     showlegend = show_legend)
    arg_list <- c(arg_list, arguments)
    plot_model <- do.call(plotly::add_trace, arg_list)
  }
  plot_model <- plotly::layout(plot_model, xaxis = list(title = "Date"))
}

#' Plot Target data with GGplot
#'
#' Use ggplot2 to plot target data
#'
#' @param plot_model a plot_ly object to add lines and/or ribbons, if NULL will
#'  create an empty object
#' @param target_data a `data.frame` object containing the target data,
#'  containing the columns: `date` and `observation`.
#'  Ignored, if `plot_target = FALSE`
#' @param plot_target a `boolean` for showing the target data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter
#'  `target_data`.
#' @param x_col_name column name containing the date information for the x-axis.
#'  By default, "date".
#'
#' @noRd
#' @importFrom ggplot2 geom_line geom_point aes .data
static_target_data <- function(plot_model, target_data, plot_target,
                               x_col_name = "date") {
  if (plot_target) {
    target_data[[x_col_name]] <- as.Date(target_data[[x_col_name]])
    plot_model <- plot_model  +
      geom_line(data = target_data,
                aes(x = .data[[x_col_name]], y = .data$observation),
                color = "#6e6e6e", inherit.aes = FALSE) +
      geom_point(data = target_data,
                 aes(x = .data[[x_col_name]], y = .data$observation),
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
#' @param group column name for partitioning the data in the data according
#'  the the value in the column. Please refer to [ggplot2::aes_group_order] for
#'  more information. By default, NULL (no partitioning).
#'
#' @noRd
#' @importFrom plotly add_lines add_ribbons
#' @importFrom dplyr group_by
plotly_proj_data <- function(plot_model, df_point, df_ribbon, df_sample,
                             line_color, opacity, arguments,
                             fill_by = "model_id", x_col_name = "target_date",
                             group = NULL) {
  if (nrow(df_point) > 0) {
    if (!is.null(group))
      df_point <- dplyr::group_by(df_point, .data[[group]])
    if (!is.null(df_sample)) {
      line_width <- 4
    } else {
      line_width <- 2
    }
    arg_list <-
      list(p = plot_model, data = df_point, x = df_point[[x_col_name]],
           y = ~value, legendgroup = df_point[[fill_by]],
           line = list(width = line_width),
           name = df_point[[fill_by]], hoverinfo = "text",
           hovertext = paste("Date: ", df_point[[x_col_name]], "<br>",
                             "Median: ", format(round(df_point$value, 2),
                                                big.mark = ","), sep = ""))
    if (is.null(line_color)) {
      arg_list <- c(arg_list, list(color =  df_point[[fill_by]]), arguments)
      plot_model <- do.call(plotly::add_lines, arg_list)
      plot_model <- plotly::layout(plot_model, xaxis = list(title = "Date"))
    } else {
      arg_list$line <-  list(width = line_width, color = line_color)
      arg_list <- c(arg_list, arguments)
      plot_model <- do.call(plotly::add_lines, arg_list)
      plot_model <- plotly::layout(plot_model, xaxis = list(title = "Date"))
    }
    show_legend <- FALSE
  } else {
    show_legend <- arguments$showlegend
    if (is.null(show_legend)) show_legend <- TRUE
  }

  if (!is.null(df_sample)) {
    for (j in unique(df_sample[[fill_by]])) {
      show_leg <- show_legend
      df_sample_id <- dplyr::filter(df_sample,
                                    .data[[fill_by]] == j)
      if (!is.null(group)) {
        df_sample_id  <- dplyr::group_by(df_sample_id, .data[[group]],
                                         .data[["output_type_id"]])
      } else {
        df_sample_id <- dplyr::group_by(df_sample_id,
                                        .data[["output_type_id"]])
      }
      arg_list <-
        list(p = plot_model, data = df_sample_id,
             x = df_sample_id[[x_col_name]], y = df_sample_id[["value"]],
             type = "scatter", mode = "lines", showlegend = show_leg,
             opacity = opacity, name = df_sample_id[[fill_by]],
             legendgroup = df_sample_id[[fill_by]])
      if (is.null(line_color)) {
        arg_list <- c(arg_list, list(color =  df_sample_id[[fill_by]]),
                      arguments)
      } else {
        arg_list <- c(arg_list, list(line = list(color = line_color)),
                      arguments)
      }
      plot_model <- do.call(plotly::add_trace, arg_list)
      plot_model <- plotly::layout(plot_model, xaxis = list(title = "Date"))
      show_leg <- FALSE
    }
    show_legend <- FALSE
  }

  if (!is.null(df_ribbon)) {
    for (n_rib in seq_along(df_ribbon)) {
      df_rib <- df_ribbon[[n_rib]]
      if (n_rib > 1) show_legend <- FALSE
      if (nrow(df_rib) > 0) {
        hover_text <-
          paste("Date: ", df_rib[[x_col_name]], "<br>",
                scales::percent(as.numeric(names(df_ribbon)[n_rib])),
                " Intervals: ", format(round(df_rib$min, 2), big.mark = ","),
                " - ", format(round(df_rib$max, 2), big.mark = ","), sep = "")
        if (!is.null(group))
          df_rib <- dplyr::group_by(df_rib, .data[[group]])
        arg_list <-
          list(plot_model, data = df_rib, x = df_rib[[x_col_name]], ymin = ~min,
               ymax = ~max, opacity = opacity, showlegend = show_legend,
               name = df_rib[[fill_by]], legendgroup = df_rib[[fill_by]],
               hoverinfo = "text", hovertext = hover_text)
        if (is.null(line_color)) {
          arg_list <- c(arg_list, list(color = df_rib[[fill_by]],
                                       line = list(width = 0)), arguments)
          plot_model <- do.call(plotly::add_ribbons, arg_list)
        } else {
          arg_list <- c(arg_list, list(fillcolor = line_color,
                                       line = list(width = 0,
                                                   color = line_color)),
                        arguments)
          plot_model <- do.call(plotly::add_ribbons, arg_list)
          plot_model <- plotly::layout(plot_model, xaxis = list(title = "Date"))
        }
        #show_legend <- FALSE
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
#' @param group column name for partitioning the data in the data according
#'  the the value in the column. Please refer to [ggplot2::aes_group_order] for
#'  more information. By default, NULL (no partitioning)
#'
#' @noRd
#' @importFrom ggplot2 geom_ribbon
#' @importFrom purrr map
static_proj_data <- function(plot_model, df_point, df_ribbon, df_sample,
                             line_color, opacity, fill_by = "model_id",
                             x_col_name = "target_date", group = NULL) {
  if (!is.null(df_ribbon)) {
    for (fill in unique(unlist(purrr::map(df_ribbon, fill_by)))) {
      for (n_rib in seq_along(df_ribbon)) {
        df_rib <- df_ribbon[[n_rib]]
        df_rib_mod <- df_rib[which(df_rib[[fill_by]] == fill), ]
        if (nrow(df_rib) > 0) {
          if (is.null(group)) {
            plot_aes <- aes(.data[[x_col_name]], ymin = .data$min,
                            ymax = .data$max, fill = .data[[fill_by]])
          } else {
            plot_aes <- aes(.data[[x_col_name]], ymin = .data$min,
                            ymax = .data$max, fill = .data[[fill_by]],
                            group = .data[[group]])
          }
          plot_model <- plot_model +
            geom_ribbon(data = df_rib_mod, plot_aes,
                        alpha = opacity, inherit.aes = FALSE)
        }
      }
    }
  }

  if (!is.null(df_sample)) {
    for (fill in unique(df_sample[[fill_by]])) {
      df_sample_id <- dplyr::filter(df_sample, .data[[fill_by]] == fill)
      plot_model <- plot_model +
        geom_line(data = df_sample_id,
                  aes(x = .data[[x_col_name]], y = .data$value,
                      color = .data[[fill_by]],
                      group = paste0(.data[[group]], "-",
                                     .data[["output_type_id"]])),
                  alpha = opacity)
    }
  }

  if (nrow(df_point) > 0) {
    if (!is.null(df_sample)) {
      line_width <- 1.3
    } else {
      line_width <- 1
    }

    if (is.null(group)) {
      plot_aes <- aes(x = .data[[x_col_name]], y = .data$value,
                      color = .data[[fill_by]])
    } else {
      df_point$group <- as.factor(paste0(df_point[[fill_by]],
                                         df_point[[group]]))
      plot_aes <- aes(x = .data[[x_col_name]], y = .data$value,
                      color = .data[[fill_by]], group = .data[["group"]])
    }
    plot_model <- plot_model  +
      geom_line(data = df_point, plot_aes, inherit.aes = FALSE,
                linewidth = line_width)
  }

  return(plot_model)
}


#' Plot simple projection data
#'
#' Use Plotly or ggplot2 to plot simple projection model output with or
#' without target data.
#' Simple projection model output are defined as projection associated with one
#' particular set of "tasks_ids" value. For more information, please refer to
#' [HubDocs website](https://docs.hubverse.io/en/latest/user-guide/tasks.html)
#' .
#'
#' @param plot_model a plot_ly object to add lines and/or ribbons.
#' @param df_point a `data.frame` with a column containing date information
#' (`x_col_name` parameter) and "value" columns, use to add lines on the plot.
#' @param df_ribbon a `data.frame` with a column containing date information
#' (`x_col_name` parameter), and a "min", and "max" columns, use to add ribbons
#' on the plot.
#' @param plot_target a `boolean` for showing the target data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter
#'  `target_data`
#' @param target_data a `data.frame` object containing the target data,
#'  containing the columns: date information (`x_target_col_name` parameter) and
#'  `observation`. Ignored, if `plot_target = FALSE`.
#' @param opacity a `numeric`, opacity of the ribbons, default 0.25.
#' @param line_color a `string`, specific color associated with plot.
#' @param top_layer character vector, where the first element indicates the top
#'  layer of the resulting plot. Possible options are `"model_output"` (default)
#'  and `"target"`.
#' @param show_target_legend a `boolean` to show legend of the target data, by
#'  default `TRUE`.
#' @param interactive a `boolean` to output an "interactive" version of the
#'  plot (using Plotly) or a "static" plot (using ggplot2). By default, `TRUE`
#'  (interactive plot).
#' @param fill_by name of a column for specifying colors and legend in plot.
#' The `pal_color` parameter can be use to change the palette.
#' Default to `model_id`.
#' @param x_col_name column name containing the date information for `df_point`
#' and `df_ribbon` data frames, value will be map to the x-axis of the plot.
#' By default, "target_date".
#' @param x_target_col_name  column name containing the date information for
#' `target_data` data frame, value will be map to the x-axis of the plot.
#' By default, "date".
#' @param group column name for partitioning the data in the data according
#'  the the value in the column. Please refer to [ggplot2::aes_group_order] for
#'  more information. By default, NULL (no partitioning)
#' @param ... additional Plotly parameters.
#'
#' @noRd
#' @importFrom plotly plot_ly
simple_model_plot <- function(
    plot_model, df_point, df_ribbon, df_sample, plot_target, target_data,
    opacity = 0.25, line_color = NULL, top_layer = "model_output",
    show_target_legend = TRUE, interactive = TRUE, fill_by = "model_id",
    x_col_name = "target_date", x_target_col_name = "date", group = NULL, ...) {
  # prerequisite
  arguments <- list(...)

  if (top_layer == "model_output") {
    if (interactive) {
      # Target Data
      plot_model <- plotly_target_data(plot_model, target_data, plot_target,
                                       show_target_legend, arguments,
                                       x_col_name = x_target_col_name)
      # Projection data
      plot_model <- plotly_proj_data(plot_model, df_point, df_ribbon, df_sample,
                                     line_color, opacity, arguments,
                                     fill_by = fill_by, x_col_name = x_col_name,
                                     group = group)
    } else {
      # Target Data
      plot_model <- static_target_data(plot_model, target_data, plot_target,
                                       x_col_name = x_target_col_name)
      # Projection data
      plot_model <- static_proj_data(plot_model, df_point, df_ribbon, df_sample,
                                     line_color, opacity, fill_by = fill_by,
                                     x_col_name = x_col_name, group = group)
    }

  } else if (top_layer == "target") {
    if (interactive) {
      # Projection data
      plot_model <- plotly_proj_data(plot_model, df_point, df_ribbon, df_sample,
                                     line_color, opacity, arguments,
                                     fill_by = fill_by, x_col_name = x_col_name,
                                     group = group)
      # Target Data
      plot_model <- plotly_target_data(plot_model, target_data, plot_target,
                                       show_target_legend, arguments,
                                       x_col_name = x_target_col_name)
    } else {
      # Projection data
      plot_model <- static_proj_data(plot_model, df_point, df_ribbon, df_sample,
                                     line_color, opacity, group = group,
                                     fill_by = fill_by, x_col_name = x_col_name)
      # Target Data
      plot_model <- static_target_data(plot_model, target_data, plot_target,
                                       x_col_name = x_target_col_name)
    }
  }

  return(plot_model)
}


#' Layout attributes updates
#'
#' Update plotly layout attributes information by appending a named list of
#' attributes to the existing plotly layout attributes.
#' The name of each element is extracted from the layout information in the
#' plot by using the regex or name inputted in `attribute_name`, for example
#' `xaxis`.
#' If multiple names are extracted from the layout information, each names
#' will be assign the new `attributes`, for example:
#' `list(type = "log", matches = "x")`.
#'
#'
#' @param plot_model a plot_ly or ggplot object
#' @param attribute_name string, name or regex of the name of attribute(s) to
#' update, extracted from the layout of the plot.
#' @param attributes a list, new attributes to add in the `layoutAttrs` of the
#' plot in a list (for example, `list(type = "log")` to transform axis to log
#' scale)
#'
#' @noRd
#' @importFrom purrr map
plot_model_layout_attr <- function(plot_model, attribute_name, attributes) {
  attr_name <- grep(attribute_name, names(plot_model$x$layout), value = TRUE)
  attr_update <- purrr::map(attr_name,
                            function(x) setNames(list(attributes), x))
  plot_model$x$layoutAttrs <- c(plot_model$x$layoutAttrs, attr_update)
  plot_model
}

#' Output plot layout
#'
#' Function to modify the plot layout: add title, transform the y-axis into a
#' log scale, etc.
#'
#' @param plot_model a plot_ly or ggplot object
#' @param interactive a `boolean` to output an "interactive" version of the
#'  plot (using Plotly) or a "static" plot (using ggplot2). By default, `TRUE`
#'  (interactive plot)
#' @param log_scale a `boolean` to plot y-axis output on a log scale. Default to
#'  FALSE
#' @param show_legend a `boolean` for showing the legend in the plot.
#'  Default to TRUE.
#' @param title a `character` string, if not NULL, will be added as title to the
#'  plot
#' @param facet a unique value corresponding to a task_id variable name
#'  (interpretable as facet option for ggplot)
#' @param facet_scales argument for scales as in
#'  [ggplot2::facet_wrap] or equivalent to `shareX`, `shareY` in
#'  [plotly::subplot]. Default to "fixed" (x and y axes are shared).le as facet
#'  option for ggplot)
#'
#' @noRd
#' @importFrom plotly layout
#' @importFrom purrr map
#' @importFrom ggplot2 scale_y_log10 labs guides
plot_layout <- function(plot_model, interactive = TRUE, log_scale = FALSE,
                        show_legend = TRUE, title = NULL, facet = NULL,
                        facet_scales = "fixed") {
  if (interactive) {
    plot_model <- plotly::layout(plot_model, xaxis = list(title = "Date"),
                                 showlegend = show_legend)
    if (!is.null(title)) {
      plot_model <- plotly::layout(plot_model, title = title)
    }
    if (!is.null(facet)) {
      plot_model$x$layout <-
        purrr::map(plot_model$x$layout, function(x) {
          if (any(grepl("title", names(x)))) {
            if (grepl("x", x$anchor)) {
              x$title <- "Value"
            }
          }
          x
        })
      if (log_scale) {
        plot_model <- plot_model_layout_attr(plot_model, "yaxis",
                                             list(type = "log"))
      }
      if (facet_scales == "fixed" ||  facet_scales == "free_y") {
        plot_model <- plot_model_layout_attr(plot_model, "xaxis",
                                             list(matches = "x"))
      }
      if (facet_scales == "fixed" || facet_scales == "free_x") {
        plot_model <- plot_model_layout_attr(plot_model, "yaxis",
                                             list(matches = "y"))
      }

    } else {
      plot_model <- plotly::layout(plot_model,
                                   yaxis = list(title = "Value"))
      if (log_scale) {
        plot_model <- plotly::layout(plot_model,
                                     yaxis = list(type = "log"))
      }
    }
  } else {
    plot_model <- plot_model + labs(x =  "Date", y =  "Value")
    if (!is.null(title)) {
      plot_model <- plot_model + labs(title = title)
    }
    if (isFALSE(show_legend)) {
      plot_model <- plot_model + guides(fill = "none", color = "none")
    }
    if (log_scale) {
      plot_model <- plot_model + scale_y_log10()
    }
  }
  return(plot_model)
}
