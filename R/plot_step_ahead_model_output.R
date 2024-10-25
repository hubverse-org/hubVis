#' Basic Plot for model outputs
#'
#' Create a simple Plotly time-series plot for model projection outputs.
#'
#'@param model_out_tbl a `model_out_tbl` object, containing all the required
#' columns including a column containing date information (`x_col_name`
#' parameter) and a column `value`.
#'@param target_data a `data.frame` object containing the target data,
#' with a column containing date information (`x_target_col_name` parameter) and
#' a column `observation`. Ignored, if `plot_target = FALSE`.
#'@param use_median_as_point a `Boolean` for using median quantile as point
#' in plot. Default to FALSE. If TRUE, will select first any `median`
#' output type value and if no `median` value included in `model_out_tbl`;
#' will select `quantile = 0.5` output type value.
#'@param show_plot a `boolean` for showing the plot. Default to TRUE.
#'@param plot_target a `boolean` for showing the target data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter
#'  `target_data`
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
#'  and `"target"`
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
#'@param x_target_col_name  column name containing the date information for
#' `target_data` data frame, value will be map to the x-axis of the plot.
#' By default, "date".
#' @param group column name for partitioning the data in the data according
#'  the the value in the column. Please refer to [ggplot2::aes_group_order] for
#'  more information. By default, NULL (no partitioning).
#'
#' @importFrom cli cli_abort cli_warn
#' @importFrom scales percent
#' @importFrom methods show
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices col2rgb rgb colors
#' @importFrom ggplot2 labs guides
#'
#' @export
#'
#' @examples
#'
#' # Load and Prepare Data
#' # The package hubExmaple contains example files, please consult the
#' # documentation associated with the package, for more information.
#' library(hubExamples)
#' head(scenario_outputs)
#' head(scenario_target_ts)
#' projection_data <- dplyr::mutate(scenario_outputs,
#'      target_date = as.Date(origin_date) + (horizon * 7) - 1)
#' projection_data <- dplyr::filter(projection_data,
#'      scenario_id == "A-2021-03-05", location == "US")
#' projection_data <- hubUtils::as_model_out_tbl(projection_data)
#'
#' target_data_us <- dplyr::filter(scenario_target_ts, location == "US",
#'                                 date < min(projection_data$target_date) + 21,
#'                                 date > "2020-10-01")
#' # Plot
#' plot_step_ahead_model_output(projection_data, target_data_us)
#'
plot_step_ahead_model_output <- function(
    model_out_tbl, target_data, use_median_as_point = FALSE,
    show_plot = TRUE, plot_target = TRUE, x_col_name = "target_date",
    x_target_col_name = "date", show_legend = TRUE, facet = NULL,
    facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL,
    facet_title = "top left", interactive = TRUE, fill_by = "model_id",
    pal_color = "Set2", one_color = "blue", fill_transparency = 0.25,
    intervals = c(.5, .8, .95), top_layer = "model_output", title = NULL,
    ens_color = NULL, ens_name = NULL, group = NULL) {

  # Test format input
  ## Model Output Table
  exp_f_col <- unique(c("model_id", "output_type_id", x_col_name, "value",
                        fill_by, group))
  mdl_out_validation(model_out_tbl, col_names = exp_f_col)

  ## Target Data
  if (plot_target) {
    exp_td_col <- c(x_target_col_name, "observation")
    target_validation(target_data, col_names = exp_td_col)
  }
  ## Parameters
  ### Intervals
  list_intervals <- list("0.95" = c(0.975, 0.025), "0.9" = c(0.95, 0.05),
                         "0.8" = c(0.9, 0.1), "0.5" = c(0.75, 0.25))
  if (!is.null(intervals)) {
    intervals <- interval_validation(model_out_tbl, as.character(intervals),
                                     list_intervals)
    ribbon <- list_intervals[as.character(sort(intervals, decreasing = TRUE))]
  } else {
    ribbon <- NULL
  }
  ### Median
  if (isTRUE(use_median_as_point)) {
    if (any(grepl("median", model_out_tbl$output_type))) {
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
  model_out_tbl <- output_type_validation(model_out_tbl, exp_value)
  ### Ensemble specific color
  ensemble_validation(ens_color, ens_name)
  ### Facet
  facet_nrow <- facet_validation(model_out_tbl, facet = facet,
                                 interactive = interactive,
                                 facet_nrow = facet_nrow,
                                 facet_title = facet_title)
  #### Top layer
  layer_validation(top_layer)
  #### Palette
  palette <- make_palette(model_out_tbl, fill_by = fill_by,
                          pal_color = pal_color, one_color = one_color,
                          ens_color = ens_color, ens_name = ens_name,
                          plot_target = plot_target)

  # Data process
  if (!is.null(ens_color) && !is.null(ens_name)) {
    ens_df <- model_out_tbl[which(model_out_tbl$model_id == ens_name), ]
    all_ens <- plot_prep_data(ens_df, plain_line, plain_type, ribbon,
                              x_col_name = x_col_name)
    plot_df <- model_out_tbl[which(model_out_tbl$model_id != ens_name), ]
  } else {
    all_ens <- NULL
    plot_df <- model_out_tbl
  }
  all_plot <- plot_prep_data(plot_df, plain_line, plain_type, ribbon,
                             x_col_name = x_col_name)

  # Plot
  if (!is.null(facet)) {
    facet_value <- sort(unique(model_out_tbl[[facet]]))
  } else {
    facet_value <- NULL
  }
  plot_model <- output_plot(all_plot, all_ens, target_data,
                            plot_target = plot_target,
                            intervals =  intervals, pal_color = palette$color,
                            fill_transparency = fill_transparency,
                            pal_value = palette$value, top_layer = top_layer,
                            ens_color = ens_color, ens_name = ens_name,
                            facet = facet, facet_scales = facet_scales,
                            facet_nrow = facet_nrow,  facet_title = facet_title,
                            facet_value = facet_value, facet_ncol = facet_ncol,
                            interactive = interactive, fill_by = fill_by,
                            x_col_name = x_col_name,
                            x_target_col_name = x_target_col_name,
                            group = group)

  # Layout
  if (interactive) {
    plot_model <- plotly::layout(plot_model, xaxis = list(title = "Date"),
                                 yaxis = list(title = "Value"),
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
