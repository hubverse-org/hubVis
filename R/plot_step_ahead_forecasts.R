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
  ribbon_df <- reshape(
    ribbon_df, timevar = "output_type_id", direction = "wide",
    idvar = colnames(ribbon_df)[!colnames(ribbon_df) %in%
                                  c("output_type_id", "value")])
  ribbon_df$target_date <- as.Date(ribbon_df$target_date)
  colnames(ribbon_df) <- gsub("^value\\.", "" , colnames(ribbon_df))
  return(list("plain_df" = plain_df, "ribbon_df" = ribbon_df))
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
#'@param use_median_as_point a `boolean` for using median quantiles as point
#' forecasts in plot. Default to FALSE.
#'@param plot a `boolean` for showing the plot. Default to TRUE.
#'@param plot_truth a `boolean` for showing the truth data in the plot.
#'  Default to TRUE. Data used in the plot comes from the paremeter `truth_data`
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
#' @importFrom plotly plot_ly add_trace add_lines add_ribbons layout
#' @importFrom cli cli_abort cli_warn
#' @importFrom scales hue_pal
#' @importFrom methods show
#'
#' @export
#'
plot_step_ahead_forecasts <- function(forecast_data, truth_data,
                                      use_median_as_point = FALSE, plot = TRUE,
                                      plot_truth = TRUE,
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
      "x" = "{.arg forecast_data} should contain at least one supported output type.",
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
  plot_model <- plotly::plot_ly(height = 1050, colors = scales::hue_pal()(50))
  if (plot_truth) {
    truth_data$time_idx <- as.Date(truth_data$time_idx)
    plot_model <- plotly::add_trace(
      plot_model, data = truth_data, x = ~time_idx, y = ~value,
      type = "scatter", mode = "lines+markers", line = list(color = "#6e6e6e"),
      hoverinfo = "text", name = "ground truth",
      hovertext = paste("Date: ", truth_data$time_idx, "<br>",
                        "Ground truth: ",
                        format(truth_data$value, big.mark = ","), sep = ""),
      marker = list(color = "#6e6e6e", size = 7))
  }
  if (nrow(all_plot$plain_df) > 0) {
    plot_model <- plotly::add_lines(
      plot_model, data = all_plot$plain_df, x = ~target_date, y = ~value,
      color = ~model_id)
    show_legend = FALSE
  } else {
    show_legend = TRUE
  }

  plot_model <- plotly::add_ribbons(
    plot_model, data = all_plot$ribbon_df, x = ~target_date, ymin = ~min,
    ymax = ~max, color = ~model_id, opacity = 0.25, line = list(width = 0),
    showlegend = show_legend)

  # Ensemble color
  if (!is.null(all_ens)) {
    if (nrow(all_ens$plain_df) > 0) {
      plot_model <- plotly::add_lines(
        plot_model, data = all_ens$plain_df, x = ~target_date, y = ~value,
        line = list(color = ens_color), color = ~model_id)
      show_legend = FALSE
    } else {
      show_legend = TRUE
    }
    plot_model <- plotly::add_ribbons(
      plot_model, data = all_ens$ribbon_df, x = ~target_date, ymin = ~min,
      ymax = ~max, opacity = 0.25, fillcolor = ens_color,
      line = list(width = 0, color = ens_color), showlegend = show_legend)
  }

  # Layout
  plot_model <- plotly::layout(
    plot_model, xaxis = list(title = 'Date'), yaxis = list(title = 'Value'))
  if (!is.null(title))
    plot_model <- plotly::layout(plot_model, title = title)

  if (isTRUE(plot)) {
    show(plot_model)
  } else {
    invisible(plot_model)
  }

}
