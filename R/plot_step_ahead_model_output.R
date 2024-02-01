#' Basic Plot for model outputs
#'
#' Create a simple Plotly time-series plot for model projection outputs.
#'
#'@param model_output_data a `model_out_tbl` object, containing all the required
#' columns including a column containing date information (`x_col_name`
#' parameter) and a column `value`.
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
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices col2rgb rgb colors
#' @importFrom ggplot2 labs guides
#'
#' @export
#'
#' @examples
#'
#' # Load and Prepare Data
#' # The package contains example files, please consult the vignette for
#' # more information. The vignette also contains more examples.
#' projection_path <- system.file("example_round1.csv", package = "hubVis")
#' projection_data <- read.csv(projection_path, stringsAsFactors = FALSE)
#'
#' projection_data <- dplyr::mutate(projection_data,
#'      target_date = as.Date(origin_date) + (horizon * 7) - 1)
#' projection_data <- dplyr::filter(projection_data,
#'      scenario_id == "A-2021-03-05", location == "US")
#' projection_data <- hubUtils::as_model_out_tbl(projection_data)
#'
#' truth_path <- system.file("truth_data.csv", package = "hubVis")
#' truth_data <- read.csv(truth_path, stringsAsFactors = FALSE)
#' truth_data_us <- dplyr::filter(truth_data, location == "US",
#'      time_idx < min(projection_data$target_date) + 21,
#'      time_idx > "2020-10-01")
#'
#' # Plot
#' plot_step_ahead_model_output(projection_data, truth_data_us)
#'
plot_step_ahead_model_output <- function(
    model_output_data, truth_data, use_median_as_point = FALSE,
    show_plot = TRUE, plot_truth = TRUE, x_col_name = "target_date",
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
    if (length(unique(model_output_data[["model_id"]])) > 5 &&
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
  if (!class(exp_value) %in% class(model_output_type_val)) {
    model_output_data$output_type_id <-
      as.numeric(model_output_data$output_type_id)
    cli::cli_warn(c("!" = "{.arg output_type_id} column must be a numeric.
                    Class applied by default."))
  }


  ### Ensemble specific color
  if (is.null(ens_color) + is.null(ens_name) == 1) {
    cli::cli_abort(c("x" = "Both {.arg ens_color} and {.arg ens_name} should
                     be set to a non NULL value"))
  }
  ### Facet
  if (!is.null(facet)) {
    if ((length(facet) != 1) ||
          !all(facet %in%
                 setdiff(colnames(model_output_data),
                         hubUtils::std_colnames[names(hubUtils::std_colnames) !=
                                                  "model_id"]))) {
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
    pal_color <- one_color
    pal_value <- rep(pal_color, length(fill_by_vect))
  }
  names(pal_value) <- fill_by_vect
  if (!is.null(ens_color) && !is.null(ens_name))
    pal_value[ens_name] <- grDevices::rgb(grDevices::col2rgb(ens_color)[1],
                                          grDevices::col2rgb(ens_color)[2],
                                          grDevices::col2rgb(ens_color)[3])
  if (plot_truth) {
    pal_value <- c(pal_value, "Truth Data" = "#6e6e6e")
  }


  # Data process
  if (!is.null(ens_color) && !is.null(ens_name)) {
    ens_df <- model_output_data[which(model_output_data$model_id == ens_name), ]
    all_ens <- plot_prep_data(ens_df, plain_line, plain_type, ribbon,
                              x_col_name = x_col_name)
    plot_df <- model_output_data[which(model_output_data$model_id !=
                                         ens_name), ]
  } else {
    all_ens <- NULL
    plot_df <- model_output_data
  }
  all_plot <- plot_prep_data(plot_df, plain_line, plain_type, ribbon,
                             x_col_name = x_col_name)

  # Plot
  if (!is.null(facet)) {
    facet_value <- sort(unique(model_output_data[[facet]]))
  } else {
    facet_value <- NULL
  }
  plot_model <- output_plot(all_plot, all_ens, truth_data,
                            plot_truth = plot_truth,
                            intervals =  intervals, pal_color = pal_color,
                            fill_transparency = fill_transparency,
                            pal_value = pal_value, top_layer = top_layer,
                            ens_color = ens_color, ens_name = ens_name,
                            facet = facet, facet_scales = facet_scales,
                            facet_nrow = facet_nrow,  facet_title = facet_title,
                            facet_value = facet_value, facet_ncol = facet_ncol,
                            interactive = interactive, fill_by = fill_by,
                            x_col_name = x_col_name,
                            x_truth_col_name = x_truth_col_name)
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
