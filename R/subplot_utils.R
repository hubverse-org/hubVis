#' Reformat Data for "facet" plotting function
#'
#' Data process for plotting function, returns a named list for a specific facet
#'
#'@param df a `data.frame` object containing the column expressed in the
#' facet parameter
#'@param facet a unique value corresponding as a task_id variable name
#' (interpretable as facet option for ggplot)
#'@param facet_value a vector of one of the possible unique values in the
#'  associated column `facet`
#'
#'@noRd
prep_facet_data <- function(df, facet, facet_value) {
  list_df <- stats::setNames(lapply(df, function(df_rib) {
    df_rib[which(df_rib[[facet]] == facet_value), ]
  }), names(df))
  return(list_df)
}

#' Reformat Data for "facet" plotting function
#'
#' Data process for plotting function, returns a named list with 2 elements,
#' one for line and one for ribbon plotting, for a specific facet
#'
#'@param df a `data.frame` object containing the column expressed in the
#'  facet paramter
#'@param facet a unique value corresponding as a task_id variable name
#' (interpretable as facet option for ggplot)
#'@param facet_value a vector of one of the possible unique values in the
#'  associated column `facet`
#'@param intervals a vector of `numeric` values indicating which central
#' prediction interval levels to plot. `NULL` means no interval levels.
#' If not provided, it will default to `c(.5, .8, .95)`.
#' When plotting 6 models or more, the plot will be reduced to show `.95`
#' interval only. Value possibles: `0.5, 0.8, 0.9, 0.95`
#'
#'@noRd
plotly_facet_data <- function(data, facet, facet_value,
                              intervals = c(.5, .8, .95)) {
  df_point <- data$median[which(data$median[[facet]] == facet_value), ]
  df_ribbon <- data[names(data) %in% intervals]
  df_ribbon <- prep_facet_data(df_ribbon, facet, facet_value)
  return(list(point = df_point, ribbon = df_ribbon))
}

#' Facet plot for Plotly
#'
#' Use Plotly to plot model output data for one specific facet with or without
#' target data.
#'
#' @param plot_model a plot_ly object to add lines and/or ribbons
#' @param all_plot a list with two data frame: one for plain lines,
#'  one for ribbons plotting (in a wide format)
#' @param all_ens a list with two data frame: one for plain lines,
#'  one for ribbons plotting (in a wide format) for a unique `model_id` value
#'  associated with specific color (`ens_color`). NULL is no specific layout
#'  required
#' @param target_data a `data.frame` object containing the target data,
#'  containing the columns: date information (`x_target_col_name` parameter) and
#'  `value`. Ignored, if `plot_target = FALSE`.
#' @param facet a unique value corresponding as a task_id variable name
#' (interpretable as facet option for ggplot)
#' @param facet_value a vector of one of the possible unique values in the
#'  associated column `facet` (use to create one specific facet)
#' @param all_facet_value a vector of oall the possible unique values in the
#'  associated column `facet`
#' @param plot_target a `boolean` for showing the target data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter
#'  `target_data`
#' @param intervals a vector of `numeric` values indicating which central
#'  prediction interval levels to plot. `NULL` means no interval levels.
#'  If not provided, it will default to `c(.5, .8, .95)`.
#'  When plotting 6 models or more, the plot will be reduced to show `.95`
#'  interval only. Value possibles: `0.5, 0.8, 0.9, 0.95`
#' @param top_layer character vector, where the first element indicates the top
#'  layer of the resulting plot. Possible options are `"model_output"` (default)
#'  and `"target"`
#' @param fill_transparency numeric value used to set transparency of intervals.
#'  0 means fully transparent, 1 means opaque. Default to `0.25`
#' @param fill_by name of a column for specifying colors and legend in plot.
#'  The `pal_color` parameter can be use to change the palette.
#'  Default to `model_id`.
#' @param x_col_name column name containing the date information for `all_plot`
#'  and `all_ens` data frames, value will be map to the x-axis of the plot.
#'  By default, "target_date".
#' @param x_target_col_name  column name containing the date information for
#' `target_data` data frame, value will be map to the x-axis of the plot.
#'  By default, "time_idx".
#' @param ens_color a `character` string of a color name, if not NULL, will be
#' use as color for the model name associated with the parameter `ens_name`
#'@param ens_name a `character` string of a model name, if not NULL, will be
#' use to change the color for the model name, associated with the parameter
#' `ens_color`(both parameter need to be provided)
#'
#' @noRd
plotly_facet_plot <- function(plot_model, all_plot, all_ens, target_data,
                              facet, facet_value, all_facet_value,
                              plot_target = TRUE, intervals = c(.5, .8, .95),
                              top_layer = "model_output",
                              fill_transparency = 0.25, fill_by = "model_id",
                              x_col_name = "target_date",
                              x_target_col_name = "time_idx",
                              ens_name = NULL, ens_color = NULL) {
  # Data
  all_plot_data <- plotly_facet_data(all_plot, facet, facet_value, intervals)
  if (plot_target && facet %in% colnames(target_data)) {
    target_data <- target_data[which(target_data[[facet]] == facet_value), ]
  }
  # plot
  args <-
    list(plot_model, all_plot_data$point,  all_plot_data$ribbon, plot_target,
         target_data, opacity = fill_transparency, top_layer = top_layer,
         interactive = TRUE, fill_by = fill_by, x_col_name = x_col_name,
         x_target_col_name = x_target_col_name)
  if (facet_value == all_facet_value[1]) {
    args <-  c(args, show_target_legend = TRUE)
  } else if (facet == fill_by) {
    args <- c(args, show_target_legend = FALSE)
  } else {
    args <- c(args, show_target_legend = FALSE, showlegend = FALSE)
  }
  plot_model <- do.call(simple_model_plot, args)
  if (!is.null(all_ens)) {
    ens_plot_data <- plotly_facet_data(all_ens, facet, facet_value, intervals)
    args <-
      list(plot_model, ens_plot_data$point, ens_plot_data$ribbon, FALSE,
           target_data, opacity = fill_transparency, top_layer = top_layer,
           interactive = TRUE, fill_by = fill_by, x_col_name = x_col_name,
           x_target_col_name = x_target_col_name, line_color = ens_color)
    if (facet_value == all_facet_value[1]) {
      args <-  c(args, show_target_legend = TRUE)
    } else if (facet == fill_by) {
      if (facet == "model_id" && ens_name == facet_value)
        args <- c(args, show_target_legend = FALSE)
    } else {
      args <- c(args, show_target_legend = FALSE, showlegend = FALSE)
    }
    plot_model <- do.call(simple_model_plot, args)
  }
  return(plot_model)
}

#' Facet plot Layout for Plotly
#'
#' Layout of a plot of model output data for one specific facet with or without
#' target data.
#'
#' @param plot_model a plot_ly object to add lines and/or ribbons
#' @param text a vector, facet title
#' @param facet_title a `string`, position of each subplot tile (value
#'  associated with the `facet` parameter). "top right", "top left" (default),
#'  "bottom right", "bottom left" are the possible values, `NULL` to remove the
#'  title.
#' @noRd
plotly_facet_layout <- function(plot_model, text, facet_title = "top left") {
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
    plot_model <-
      plotly::layout(plot_model,
                     annotations = list(x = x_title, y = y_title,
                                        xref = "paper", yref = "paper",
                                        xanchor = x_anchor,
                                        yanchor = y_anchor,
                                        showarrow = FALSE, text = text))
  }
  return(plot_model)
}

#' Facet plot for Plotly
#'
#' Use Plotly to plot model output data for one specific facet with or without
#' target data and with specific layout (see parameters below).
#'
#' @param facet_value a vector of one of the possible unique values in the
#'  associated column `facet` (use to create one specific facet and as facet
#'  title)
#' @param plot_model a plot_ly object to add lines and/or ribbons
#' @param all_plot a list with two data frame: one for plain lines,
#'  one for ribbons plotting (in a wide format)
#' @param all_ens a list with two data frame: one for plain lines,
#'  one for ribbons plotting (in a wide format) for a unique `model_id` value
#'  associated with specific color (`ens_color`). NULL is no specific layout
#'  required
#' @param facet a unique value corresponding as a task_id variable name
#'  (interpretable as facet option for ggplot)
#' @param all_facet_value a vector of oall the possible unique values in the
#'  associated column `facet`
#' @param target_data a `data.frame` object containing the target data,
#'  containing the columns: date information (`x_target_col_name` parameter) and
#'  `value`. Ignored, if `plot_target = FALSE`.
#' @param plot_target a `boolean` for showing the target data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter
#'  `target_data`
#' @param intervals a vector of `numeric` values indicating which central
#'  prediction interval levels to plot. `NULL` means no interval levels.
#'  If not provided, it will default to `c(.5, .8, .95)`.
#'  When plotting 6 models or more, the plot will be reduced to show `.95`
#'  interval only. Value possibles: `0.5, 0.8, 0.9, 0.95`
#' @param top_layer character vector, where the first element indicates the top
#'  layer of the resulting plot. Possible options are `"model_output"` (default)
#'  and `"target"`
#' @param facet_title a `string`, position of each subplot tile (value
#'  associated with the `facet` parameter). "top right", "top left" (default),
#'  "bottom right", "bottom left" are the possible values, `NULL` to remove the
#'  title.
#' @param fill_transparency numeric value used to set transparency of intervals.
#'  0 means fully transparent, 1 means opaque. Default to `0.25`
#' @param fill_by name of a column for specifying colors and legend in plot.
#'  The `pal_color` parameter can be use to change the palette.
#'  Default to `model_id`.
#' @param x_col_name column name containing the date information for `all_plot`
#'  and `all_ens` data frames, value will be map to the x-axis of the plot.
#'  By default, "target_date".
#' @param x_target_col_name  column name containing the date information for
#' `target_data` data frame, value will be map to the x-axis of the plot.
#'  By default, "time_idx".
#' @param ens_color a `character` string of a color name, if not NULL, will be
#' use as color for the model name associated with the parameter `ens_name`
#'@param ens_name a `character` string of a model name, if not NULL, will be
#' use to change the color for the model name, associated with the parameter
#' `ens_color`(both parameter need to be provided)
#'
#' @noRd
plotly_facet <- function(facet_value, plot_model, all_plot, all_ens, facet,
                         all_facet_value, target_data,
                         intervals = c(.5, .8, .95), plot_target = TRUE,
                         fill_transparency = 0.25,
                         top_layer = "model_output", facet_title = "top left",
                         fill_by = "model_id", x_col_name = "target_date",
                         x_target_col_name = "time_idx", ens_name = NULL,
                         ens_color = NULL) {
  plot_model <-
    plotly_facet_plot(plot_model, all_plot, all_ens, target_data,
                      facet, facet_value, all_facet_value,
                      plot_target = plot_target, intervals = intervals,
                      top_layer = top_layer, fill_by = fill_by,
                      fill_transparency = fill_transparency,
                      x_col_name = x_col_name,
                      x_target_col_name = x_target_col_name,
                      ens_name = ens_name, ens_color = ens_color)
  plot_model <- plotly_facet_layout(plot_model, facet_value, facet_title)
  return(plot_model)
}

#' Multifaceted plot for Plotly
#'
#' Use Plotly to plot multifaceted plot of model output data
#'
#' @param plot_model a plot_ly object to add lines and/or ribbons
#' @param all_plot a list with two data frame: one for plain lines,
#'  one for ribbons plotting (in a wide format)
#' @param all_ens a list with two data frame: one for plain lines,
#'  one for ribbons plotting (in a wide format) for a unique `model_id` value
#'  associated with specific color (`ens_color`). NULL is no specific layout
#'  required
#' @param facet a unique value corresponding as a task_id variable name
#'  (interpretable as facet option for ggplot)
#' @param all_facet_value a vector of all the possible unique values in the
#'  associated column `facet`
#' @param target_data a `data.frame` object containing the target data,
#'  containing the columns: date information (`x_target_col_name` parameter) and
#'  `value`. Ignored, if `plot_target = FALSE`.
#' @param plot_target a `boolean` for showing the target data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter
#'  `target_data`
#' @param intervals a vector of `numeric` values indicating which central
#'  prediction interval levels to plot. `NULL` means no interval levels.
#'  If not provided, it will default to `c(.5, .8, .95)`.
#'  When plotting 6 models or more, the plot will be reduced to show `.95`
#'  interval only. Value possibles: `0.5, 0.8, 0.9, 0.95`
#' @param facet_scales argument for scales as in [ggplot2::facet_wrap] or
#' equivalent to `shareX`, `shareY` in [plotly::subplot]. Default to "fixed"
#' (x and y axes are shared).
#' @param facet_nrow a numeric, number of rows in the layout.
#' @param fill_transparency numeric value used to set transparency of intervals.
#'  0 means fully transparent, 1 means opaque. Default to `0.25`
#' @param top_layer character vector, where the first element indicates the top
#'  layer of the resulting plot. Possible options are `"model_output"` (default)
#'  and `"target"`
#' @param facet_title a `string`, position of each subplot tile (value
#'  associated with the `facet` parameter). "top right", "top left" (default),
#'  "bottom right", "bottom left" are the possible values, `NULL` to remove the
#'  title.
#' @param fill_by name of a column for specifying colors and legend in plot.
#'  The `pal_color` parameter can be use to change the palette.
#'  Default to `model_id`.
#' @param x_col_name column name containing the date information for `all_plot`
#'  and `all_ens` data frames, value will be map to the x-axis of the plot.
#'  By default, "target_date".
#' @param x_target_col_name  column name containing the date information for
#' `target_data` data frame, value will be map to the x-axis of the plot.
#'  By default, "time_idx".
#' @param ens_color a `character` string of a color name, if not NULL, will be
#' use as color for the model name associated with the parameter `ens_name`
#'@param ens_name a `character` string of a model name, if not NULL, will be
#' use to change the color for the model name, associated with the parameter
#' `ens_color`(both parameter need to be provided)
#' @param pal_value  a `named vector` containing the `model_id` (names) and
#'  associated color. Default `NULL`, used only for static plot (ggplot2)
#'
#' @noRd
plotly_subplot <- function(plot_model, all_plot, all_ens, facet,
                           all_facet_value, target_data, plot_target = TRUE,
                           intervals = c(.5, .8, .95), facet_scales = "fixed",
                           facet_nrow = NULL, fill_transparency = 0.25,
                           top_layer = "model_output", facet_title = "top left",
                           fill_by = "model_id", x_col_name = "target_date",
                           x_target_col_name = "time_idx", ens_name = NULL,
                           ens_color = NULL, pal_value = NULL) {
  sharex <- FALSE
  sharey <- FALSE
  if (facet_scales == "fixed") {
    sharex <- TRUE
    sharey <- TRUE
  } else if (facet_scales == "free_x") {
    sharey <- TRUE
  } else if (facet_scales == "free_y") {
    sharex <- TRUE
  }
  if (is.null(facet_nrow)) {
    facet_nrow <- 1
  }
  subplot <- lapply(all_facet_value, plotly_facet, plot_model, all_plot,
                    all_ens, facet, all_facet_value, target_data,
                    intervals = intervals, plot_target = plot_target,
                    fill_transparency = fill_transparency,
                    top_layer = top_layer, facet_title = facet_title,
                    fill_by = fill_by, x_col_name = x_col_name,
                    x_target_col_name = x_target_col_name,
                    ens_name = ens_name,
                    ens_color = ens_color)
  plot_model <- plotly::subplot(subplot, nrows = facet_nrow, shareX = sharex,
                                shareY = sharey)
  if (facet == fill_by) {
    for (i in seq_along(plot_model$x$data)) {
      if (purrr::map(plot_model$x$data, "name")[[i]] %in% all_facet_value) {
        plot_model$x$data[[i]]$fillcolor <-
          plot_model$x$data[[i]]$line$color <-
          pal_value[purrr::map(plot_model$x$data, "name")[[i]]]
      }
    }
  }
  return(plot_model)
}

#' "Simple" output plot
#'
#' Plot model output data without facet or multifaceted ggplot object.
#'
#' @param plot_model a plot_ly object to add lines and/or ribbons
#' @param all_plot a list with two data frame: one for plain lines,
#'  one for ribbons plotting (in a wide format)
#' @param all_ens a list with two data frame: one for plain lines,
#'  one for ribbons plotting (in a wide format) for a unique `model_id` value
#'  associated with specific color (`ens_color`). NULL is no specific layout
#'  required
#' @param target_data a `data.frame` object containing the target data,
#'  containing the columns: date information (`x_target_col_name` parameter) and
#'  `value`. Ignored, if `plot_target = FALSE`.
#' @param facet a unique value corresponding as a task_id variable name
#'  (interpretable as facet option for ggplot). If set to `NULL` (default),
#'  no facet
#' @param intervals a vector of `numeric` values indicating which central
#'  prediction interval levels to plot. `NULL` means no interval levels.
#'  If not provided, it will default to `c(.5, .8, .95)`.
#'  When plotting 6 models or more, the plot will be reduced to show `.95`
#'  interval only. Value possibles: `0.5, 0.8, 0.9, 0.95`
#' @param plot_target a `boolean` for showing the target data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter
#'  `target_data`
#' @param interactive a `boolean` to output an "interactive" version of the
#'  plot (using Plotly) or a "static" plot (using ggplot2). By default, `TRUE`
#'  (interactive plot)
#' @param fill_transparency numeric value used to set transparency of intervals.
#'  0 means fully transparent, 1 means opaque. Default to `0.25`
#' @param top_layer character vector, where the first element indicates the top
#'  layer of the resulting plot. Possible options are `"model_output"` (default)
#'  and `"target"`
#' @param fill_by name of a column for specifying colors and legend in plot.
#'  The `pal_color` parameter can be use to change the palette.
#'  Default to `"model_id"`.
#' @param x_col_name column name containing the date information for `all_plot`
#'  and `all_ens` data frames, value will be map to the x-axis of the plot.
#'  By default,`"target_date"`.
#' @param x_target_col_name  column name containing the date information for
#' `target_data` data frame, value will be map to the x-axis of the plot.
#'  By default, `"time_idx"`.
#' @param facet_scales argument for scales as in [ggplot2::facet_wrap] or
#' equivalent to `shareX`, `shareY` in [plotly::subplot]. Default to "`fixed"`
#' (x and y axes are shared).
#' @param facet_nrow a numeric, number of rows in the layout.
#' @param facet_ncol a numeric, number of columns in the layout.
#' @param ens_color a `character` string of a color name, if not NULL, will be
#' use as color for the model name associated with the parameter `ens_name`
#' @param group column name for partitioning the data in the data according
#'  the the value in the column. Please refer to [ggplot2::aes_group_order] for
#'  more information. By default, NULL (no partitioning).ONLY available for
#'  "static" plot.
#'
#' @noRd
simple_subplot <- function(plot_model, all_plot, all_ens, target_data,
                           facet = NULL, intervals = c(.5, .8, .95),
                           plot_target = TRUE, interactive = TRUE,
                           fill_transparency = 0.25, top_layer = "model_output",
                           fill_by = "model_id", x_col_name = "target_date",
                           x_target_col_name = "time_idx",
                           facet_scales = "fixed",
                           facet_nrow = NULL, facet_ncol = NULL,
                           group = NULL, ens_color = NULL) {
  df_point <- all_plot$median
  df_ribbon <- all_plot[names(all_plot) %in% intervals]
  args <- list(plot_model, df_point = df_point, df_ribbon = df_ribbon,
               plot_target = plot_target, target_data = target_data,
               opacity = fill_transparency,
               top_layer = top_layer, fill_by = fill_by,
               interactive = interactive,
               x_col_name = x_col_name,
               x_target_col_name = x_target_col_name,
               group = group)
  plot_model <- do.call(simple_model_plot, args)
  # Ensemble color
  if (!is.null(all_ens)) {
    args$plot_model <- plot_model
    args$plot_target <- FALSE
    args$df_point <- all_ens$median
    args$df_ribbon <- all_ens[names(all_ens) %in% intervals]
    args <- c(args, line_color = ens_color)
    plot_model <- do.call(simple_model_plot, args)
  }
  if (!is.null(facet)) {
    plot_model <-  plot_model +
      ggplot2::facet_wrap(facet, nrow = facet_nrow, ncol = facet_ncol,
                          scales = facet_scales)
  }
  return(plot_model)
}

#' Plot projection data
#'
#' Use Plotly or ggplot2 to plot projection model output with or without
#' target data.
#'
#' @param all_plot a list with two data frame: one for plain lines,
#' one for ribbons plotting (in a wide format)
#' @param all_ens a list with two data frame: one for plain lines,
#'  one for ribbons plotting (in a wide format) for a unique `model_id` value
#'  associated with specific color (`ens_color`). NULL is no specific layout
#'  required
#' @param target_data a `data.frame` object containing the target data,
#'  containing the columns: date information (`x_target_col_name` parameter) and
#'  `value`. Ignored, if `plot_target = FALSE`.
#' @param plot_target a `boolean` for showing the target data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter
#'  `target_data`
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
#'  and `"target"`
#' @param ens_color a `character` string of a color name, if not NULL, will be
#' use as color for the model name associated with the parameter `ens_name`
#'@param ens_name a `character` string of a model name, if not NULL, will be
#' use to change the color for the model name, associated with the parameter
#' `ens_color`(both parameter need to be provided)
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
#' @param x_target_col_name  column name containing the date information for
#' `target_data` data frame, value will be map to the x-axis of the plot.
#' By default, "time_idx".
#' @param group column name for partitioning the data in the data according
#'  the the value in the column. Please refer to [ggplot2::aes_group_order] for
#'  more information. By default, NULL (no partitioning).ONLY available for
#'  "static" plot.
#'
#' @noRd
#' @importFrom plotly plot_ly layout subplot
#' @importFrom ggplot2 ggplot scale_color_manual scale_fill_manual facet_wrap
output_plot <-  function(
    all_plot, all_ens, target_data, plot_target = TRUE,
    intervals = c(.5, .8, .95), pal_color = "Set2", fill_transparency = 0.25,
    pal_value = NULL, top_layer = "model_output", ens_color = NULL,
    ens_name = NULL, facet = NULL, facet_scales = "fixed", facet_nrow = NULL,
    facet_ncol = NULL, facet_title = "top left", facet_value = NULL,
    interactive = TRUE, fill_by = "model_id", x_col_name = "target_date",
    x_target_col_name = "time_idx", group = NULL) {

  if (interactive) {
    plot_model <- plotly::plot_ly(colors =  pal_color)
  } else {
    plot_model <- ggplot2::ggplot(colors =  pal_color)
  }

  if (!is.null(facet) && interactive) {
    plot_model <- plotly_subplot(plot_model, all_plot, all_ens, facet,
                                 facet_value, target_data,
                                 plot_target = plot_target,
                                 intervals = intervals,
                                 facet_scales = facet_scales,
                                 facet_nrow = facet_nrow,
                                 fill_transparency = fill_transparency,
                                 top_layer = top_layer,
                                 facet_title = facet_title,
                                 fill_by = fill_by, x_col_name = x_col_name,
                                 x_target_col_name = x_target_col_name,
                                 ens_name = ens_name, ens_color = ens_color,
                                 pal_value = pal_value)
  } else {
    plot_model <- simple_subplot(plot_model, all_plot, all_ens, target_data,
                                 intervals = intervals, facet = facet,
                                 plot_target = plot_target,
                                 interactive = interactive,
                                 fill_transparency = fill_transparency,
                                 top_layer = top_layer, fill_by = fill_by,
                                 x_col_name = x_col_name,
                                 facet_scales = facet_scales,
                                 facet_nrow = facet_nrow,
                                 facet_ncol = facet_ncol,
                                 x_target_col_name = x_target_col_name,
                                 group = group, ens_color = ens_color)
  }

  if (!interactive) {
    plot_model <- plot_model +
      scale_color_manual(values = pal_value, name = "Legend") +
      scale_fill_manual(values = pal_value, name = "Legend")
  }
  return(plot_model)
}
