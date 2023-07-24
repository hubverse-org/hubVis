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
#'@param ribbon a vector of `numeric`  output_type_id value, value will be
#' used a to create a ribbon in the plot.
#'
#' @importFrom stats reshape
plot_prep_data <- function(df, plain_line, plain_type, ribbon) {
  plain_df <- df[which(
    df$output_type_id == plain_line & df$output_type == plain_type), ]
  plain_df$target_date <- as.Date(plain_df$target_date)
  ribbon_df <- df[which(df$output_type_id %in% ribbon), ]
  ribbon_df <- transform(
    ribbon_df, output_type_id = ifelse(
      ribbon_df$output_type_id == min(ribbon), "min", "max"))
  id_col <- colnames(ribbon_df)[!colnames(ribbon_df) %in%
                                  c("output_type_id", "value")]
  ribbon_df <- reshape(
    ribbon_df, timevar = "output_type_id", direction = "wide", idvar = id_col)
  ribbon_df$target_date <- as.Date(ribbon_df$target_date)
  colnames(ribbon_df) <- gsub("^value\\.", "" , colnames(ribbon_df))
  return(list("plain_df" = plain_df, "ribbon_df" = ribbon_df))
}

#' Plot forecast data with Plotly
#'
#' Use Plotly to plot projection model output
#'
#' @param plot_model a plot_ly object to add lines and/or ribbons, if NULL will
#'  create an empty object
#' @param df_point a `data.frame` with "target_date" and "value" columns, use to
#'  add lines on the plot
#' @param df_ribbon a `data.frame` with "target_date", "min", and "max" columns,
#'  use to add ribbons on the plot
#' @param plot_truth a `boolean` for showing the truth data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter `truth_data`
#' @param truth_data a `data.frame` object containing the ground truth data,
#'  containing the columns: `time_idx` and `value`.
#'  Ignored, if `plot_truth = FALSE`
#' @param opacity a `numeric`, opacity of the ribbons, default 0.25
#' @param line_color a `string`, specific color associated with plot
#' @param ... ploty parameters
#'
#' @importFrom plotly plot_ly add_lines add_ribbons
plotly_model_plot <- function(plot_model, df_point, df_ribbon, plot_truth,
                              truth_data, opacity = 0.25, line_color = NULL,
                              ...) {
  if (is.null(plot_model)) {
    plotly::plot_ly(height = 1050)
  }
  arguments <- list(...)
  if (plot_truth) {
    truth_data$time_idx <- as.Date(truth_data$time_idx)
    arg_list <- list(
      p = plot_model, data = truth_data, x = ~time_idx, y = ~value,
      type = "scatter", mode = "lines+markers", line = list(color = "#6e6e6e"),
      hoverinfo = "text", name = "ground truth", legendgroup = "ground truth",
      hovertext = paste("Date: ", truth_data$time_idx, "<br>Ground truth: ",
                        format(truth_data$value, big.mark = ","), sep = ""),
      marker = list(color = "#6e6e6e", size = 7))
    arg_list <- c(arg_list, arguments)
    plot_model <- do.call(plotly::add_trace, arg_list)
  }
  if (nrow(df_point) > 0) {
    arg_list <- list(p = plot_model, data = df_point, x = ~target_date,
                     y = ~value, legendgroup = ~model_id, name = ~model_id)
    if (is.null(line_color)) {
      arg_list <- c(arg_list, list(color = ~model_id), arguments)
      plot_model <- do.call(plotly::add_lines, arg_list)
    } else {
      arg_list <- c(arg_list, list(line = list(color = line_color)), arguments)
      plot_model <- do.call(plotly::add_lines, arg_list)
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
  if (nrow(df_ribbon) > 0) {
    arg_list <- list(plot_model, data = df_ribbon, x = ~target_date,
                     ymin = ~min, ymax = ~max, opacity = opacity,
                     showlegend = show_legend, name = ~model_id,
                     legendgroup = ~model_id)
    if (is.null(line_color)) {
      arg_list <- c(arg_list, list(color = ~model_id, line = list(width = 0)),
                    arguments)
      plot_model <- do.call(plotly::add_ribbons, arg_list)
    } else {
      arg_list <- c(arg_list,
                    list(fillcolor = line_color,
                         line = list(width = 0, color = line_color)), arguments)
      plot_model <- do.call(plotly::add_ribbons, arg_list)
      plot_model <- do.call(plotly::add_ribbons, arg_list)
    }
  }
  return(plot_model)
}

#' Basic Plot for model outputs
#'
#' Create a simple Plotly time-series plot for model projection outputs.
#'
#'@param forecast_data a `model_output_df` object, containing all the required
#' columns, and a "target_date" and a "model_id" column.
#'@param truth_data a `data.frame` object containing the ground truth data,
#' containing the columns: `time_idx` and `value`.
#' Ignored, if `plot_truth = FALSE`
#'@param use_median_as_point a `Boolean` for using median quantile as point
#' forecasts in plot. Default to FALSE.
#'@param plot a `boolean` for showing the plot. Default to TRUE.
#'@param plot_truth a `boolean` for showing the truth data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter `truth_data`
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
#' title
#'@param ribbon a vector of `numeric`  output_type_id value, value will be
#' used a to create a ribbon in the plot.
#'@param title a `character` string, if not NULL, will be added as title to the
#' plot
#'@param ens_color a `character` string of a color name, if not NULL, will be
#' use as color for the model name associated with the parameter `ens_name`
#' (both parameter need to be provided)
#' @param ens_name a `character` string of a model name, if not NULL, will be
#' use to change the color for the model name, associated with the parameter
#' `ens_color`(both parameter need to be provided)
#'
#' @importFrom plotly plot_ly add_trace add_lines add_ribbons layout subplot
#' @importFrom cli cli_abort cli_warn
#' @importFrom scales hue_pal
#' @importFrom methods show
#'
#' @export
#'
plot_step_ahead_forecasts <- function(forecast_data, truth_data,
                                      use_median_as_point = FALSE, plot = TRUE,
                                      plot_truth = TRUE,
                                      facet = NULL, facet_scales = "fixed",
                                      facet_nrow = NULL, facet_ncol = NULL,
                                      facet_title = "top left",
                                      ribbon = c(0.975, 0.025), title = NULL,
                                      ens_color = NULL, ens_name = NULL) {
  # Test format input
  ## Forecast data
  if (!is.data.frame(forecast_data)) {
    cli::cli_abort(c("x" = "{.arg forecast_data} must be a `data.frame`."))
  }
  if (isFALSE("model_out_tbl" %in% class(forecast_data))) {
    cli::cli_warn(c("!" = "{.arg forecast_data} must be a `model_output_df`.
                    Class applied by default"))
    forecast_data <- hubUtils::as_model_out_tbl(forecast_data,
                                                remove_empty = TRUE)
  }
  exp_f_col <- c("model_id", "output_type_id", "target_date", "value")
  forecast_col <- colnames(forecast_data)
  if (!all(exp_f_col %in% forecast_col)) {
    cli::cli_abort(c("x" = "{.arg forecast_type_val} did not have all required
                     columns {.val {exp_f_col}}"))
  }
  valid_types <- c("mean", "median", "quantile")
  forecast_type <- unique(forecast_data$output_type)
  if (!any(valid_types %in% forecast_type)) {
    cli::cli_abort(c(
      "x" = "{.arg forecast_data} should contain at least one supported output
      type.",
      "i" = "Supported output types: {.val {valid_types}}."
    ))
  }
  ## Truth Data
  if (plot_truth) {
    if (!is.data.frame(truth_data)) {
      cli::cli_abort(c("x" = "{.arg truth_data} must be a `data.frame`."))
    }
    exp_td_col <- c("time_idx", "value")
    truth_data_col <- colnames(truth_data)
    if (!all(exp_td_col %in% truth_data_col)) {
      cli::cli_abort(c("x" = "{.arg truth_data} did not have all required
                     columns {.val {expploy_td_col}}"))
    }
  }
  ## Parameters
  if (isTRUE(use_median_as_point)) {
    if (any(grepl("median", forecast_data$output_type))) {
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
  exp_value <- c(plain_line, ribbon)
  forecast_type_val <- unique(forecast_data$output_type_id)
  if (!all(exp_value %in% forecast_type_val)) {
    cli::cli_abort(c("x" = "{.arg forecast_type_val} did not have the expected
                     output_type_id value {.val {exp_value}}"))
  }
  if (is.null(ens_color) + is.null(ens_name) == 1) {
    cli::cli_abort(c("x" = "Both {.arg ens_color} and {.arg ens_name} should
                     be set to a non NULL value"))
  }
  if (!is.null(facet)) {
    if ((length(facet) != 1) |
        !(facet %in% grep("output_type|value", colnames(forecast_data),
                          value = TRUE, invert = TRUE))) {
      cli::cli_abort(c("x" = "if {.arg facet} is not NULL, the argument should
                       be of lenght 1 and should match one of the task_id column
                       of {.arg forecast_data}"))
    }
  }
  if (!is.null(facet_title)) {
    facet_title_opt <- c("top right", "top left", "bottom right", "bottom left")
    if (!facet_title %in% facet_title_opt) {
      cli::cli_abort(c("x" = "{.arg facet_title} should correspond to one of these
                     possible values: {.val {facet_title_opt}}"))
    }
  }

  # Data process
  if (!is.null(ens_color) & !is.null(ens_name)) {
    ens_df <- forecast_data[which(forecast_data$model_id == ens_name), ]
    all_ens <- plot_prep_data(ens_df, plain_line, plain_type, ribbon)
    plot_df <- forecast_data[which(forecast_data$model_id != ens_name), ]
  } else {
    all_ens <- NULL
    plot_df <- forecast_data
  }
  all_plot <- plot_prep_data(plot_df, plain_line, plain_type, ribbon)

  # Plot
  plot_model <- plotly::plot_ly(height = 1050)
  if (is.null(facet)) {
    df_point <- all_plot$plain_df
    df_ribbon <- all_plot$ribbon_df
    if (!is.null(all_ens)) {
      df_point_ens <- all_ens$plain_df
      df_ribbon_ens <- all_ens$ribbon_df
    }
    plot_model <- plotly_model_plot(plot_model, df_point, df_ribbon, plot_truth,
                                    truth_data)

    # Ensemble color
    if (!is.null(all_ens)) {
      plot_model <- plotly_model_plot(plot_model, df_point_ens, df_ribbon_ens,
                                      plot_truth, FALSE,
                                      line_color = ens_color)
    }
    plot_model <- plotly::layout(
      plot_model, xaxis = list(title = 'Date'), yaxis = list(title = 'Value'))
  } else {
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
    subplot <- lapply(sort(unique(forecast_data[["scenario_id"]])), function(x) {
      df_point <- all_plot$plain_df[which(all_plot$plain_df[[facet]] == x), ]
      df_ribbon <- all_plot$ribbon_df[which(all_plot$ribbon_df[[facet]] == x), ]
      if (!is.null(all_ens)) {
        df_point_ens <- all_ens$plain_df[which(all_ens$plain_df[[facet]] == x), ]
        df_ribbon_ens <- all_ens$ribbon_df[which(all_ens$ribbon_df[[facet]] == x), ]
      }
      if (x == sort(unique(forecast_data[["scenario_id"]]))[1]) {
        plot_model <- plotly_model_plot(
          plot_model, df_point, df_ribbon, plot_truth, truth_data)
      } else {
        plot_model <- plotly_model_plot(
          plot_model, df_point, df_ribbon, plot_truth, truth_data,
          showlegend = FALSE)
      }
      # Ensemble color
      if (!is.null(all_ens)) {
        if (x == sort(unique(forecast_data[["scenario_id"]]))[1]) {
          plot_model <- plotly_model_plot(
            plot_model, df_point_ens, df_ribbon_ens, FALSE, truth_data,
            line_color = ens_color)
        } else {
          plot_model <- plotly_model_plot(
            plot_model, df_point_ens, df_ribbon_ens, FALSE, truth_data,
            line_color = ens_color, showlegend = FALSE)
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
  }

  # Layout
  if (!is.null(title))
    plot_model <- plotly::layout(plot_model, title = title)

  if (isTRUE(plot)) {
    show(plot_model)
    return(plot_model)
  } else {
    invisible(plot_model)
  }

}
