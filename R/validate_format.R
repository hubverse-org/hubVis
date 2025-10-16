#' Validate model output table format
#'
#' Validate model output table is a `model_out_tbl` data frame with the expected
#' content (column names and output_type)
#'
#' @param model_out_tbl object to validate
#' @param col_names character vector, if not set to NULL (default), will
#'  validate all the value(s) are contained the column names of
#'  `model_out_tbl`
#' @param valid_type character vector, expected `output_type`.
#' `model_out_tbl` should contain at least one of the input type.
#'
#' @noRd
mdl_out_validation <- function(model_out_tbl, col_names = NULL,
                               valid_types = c("median", "quantile",
                                               "sample")) {
  if (!is.data.frame(model_out_tbl)) {
    cli::cli_abort(c("x" = "{.arg model_out_tbl} must be a `data.frame`."))
  }
  if (isFALSE("model_out_tbl" %in% class(model_out_tbl))) {
    cli::cli_warn(c("!" = "{.arg model_out_tbl} must be a `model_out_tbl`.
                    Class applied by default"))
    model_out_tbl <- hubUtils::as_model_out_tbl(model_out_tbl,
                                                remove_empty = TRUE)
  }

  if (!is.null(col_names)) {
    model_output_col <- colnames(model_out_tbl)
    if (!all(col_names %in% model_output_col)) {
      cli::cli_abort(c("x" = "{.arg model_out_tbl} did not have all required
                       columns {.val {col_names}}"))
    }
  }

  model_output_type <- unique(model_out_tbl$output_type)
  if (!any(valid_types %in% model_output_type)) {
    cli::cli_abort(c(
      "x" = "{.arg model_out_tbl} should contain at least one supported
      output type.",
      "i" = "Supported output types: {.val {valid_types}}."
    ))
  }
}

#' Validate target data format
#'
#' Validate target data is a data frame with the expected
#' content (column names)
#'
#' @param target_data object to validate
#' @param col_names character vector, if not set to NULL (default), will
#'  validate all the value(s) are contained the column names of
#'  `target_data`
#'
#' @noRd
target_validation <- function(target_data, col_names = NULL) {
  if (!is.data.frame(target_data)) {
    cli::cli_abort(c("x" = "{.arg target_data} must be a `data.frame`."))
  }
  if (!is.null(col_names)) {
    target_data_col <- colnames(target_data)
    if (!all(col_names %in% target_data_col)) {
      cli::cli_abort(c("x" = "{.arg target_data} did not have all required
                     columns {.val {col_names}}"))
    }
  }

}

#' Validate parameter `intervals` format
#'
#' Validate the format and content of the `intervals` parameter,
#' returns `intervals` in the expected format if necessary
#'
#' @param model_out_tbl a `model_out_tbl` object, containing all the
#'  required columns including a column containing date information and a
#'  column `value`.
#' @param intervals a vector of `numeric` values indicating which central
#'  prediction interval levels to plot. `NULL` means no interval levels.
#' @param list_intervals named list of accepted intervals accepted
#' @param max_model_model_id numeric, if model_out_tbl contains more than
#'  this number of unique `"model_id"`, the intervals will be reduced to
#'  only one value (the maximal)
#'
#' @noRd
interval_validation <- function(model_out_tbl, intervals, list_intervals,
                                max_model_id = 5) {
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
  if (length(unique(model_out_tbl[["model_id"]])) > max_model_id &&
        length(intervals) > 1) {
    intervals <- max(intervals)[1]
    cli::cli_warn(c("!" = "{.arg model_out_tbl} contains more than
                    {.val {max_model_id}} models, the plot will be reduced to
                    show only one interval (the maximum interval value):
                    {.val {intervals}}"))
  }
  return(intervals)
}

#' Validate `"ensembles"` parameters format
#'
#' Validate `ens_color`, `ens_name` format: both should be set to `NULL` or to
#' a non `NULL` value.
#'
#'@param ens_color a `character` string of a color name, if not NULL, will be
#' use as color for the model name associated with the parameter `ens_name`
#' (both parameter need to be provided)
#'@param ens_name a `character` string of a model name, if not NULL, will be
#' use to change the color for the model name, associated with the parameter
#' `ens_color`(both parameter need to be provided)
#'
#' @noRd
ensemble_validation <- function(ens_color, ens_name) {
  if (is.null(ens_color) + is.null(ens_name) == 1) {
    cli::cli_abort(c("x" = "Both {.arg ens_color} and {.arg ens_name} should
                     be set to a non NULL value"))
  }
}

#' Validate `"output_type_id"` values
#'
#' Validate model output table contains the expected `"output_type_id"` values.
#'
#' @param model_out_tbl a `model_out_tbl` object, containing all the
#'  required columns including a column containing date information and a
#'  column `value`.
#' @param exp_value numeric vector, expected value required in
#' `model_out_tbl` in `"output_type_id"` column
#'
#' @noRd
output_type_validation <- function(model_out_tbl, quant_value, plain_line,
                                   intervals, use_median_as_point) {

  mod_out_type <- unique(model_out_tbl$output_type)
  mod_out_type_id <- unique(model_out_tbl$output_type_id)

  # Median line
  out_type_med <- NULL
  if (!is.null(plain_line)) {
    if (is.na(plain_line)) {
      out_type_med <- "median"
    } else if (plain_line == 0.5) {
      out_type_med <- purrr::map_vec(c("quantile", "sample"), ~ .x %in%
                                       mod_out_type)
      out_type_med <- c("quantile", "sample")[[grep(TRUE, out_type_med)[1]]]
      if (out_type_med == "quantile" && !(0.5 %in% mod_out_type_id)) {
        if ("sample" %in% mod_out_type) {
          out_type_med <- "sample"
        } else {
          cli::cli_abort(c("!" = "{.arg model_output_tbl} is missing the expected
                         output_type_id value {.val 0.5} or {.val median}
                         output_type to plot the median."))
        }
      }
    }
  }

  # Ribbon or spaghetti plot
  if (!is.null(quant_value)) {
    if ("quantile" %in% mod_out_type) {
      out_type_plot <- "quantile"
      if (!all(quant_value %in% mod_out_type_id)) {
        if ("sample" %in% mod_out_type) {
          cli::cli_warn(c("x" = "{.arg model_output_tbl} did not have the
                          expected output_type_id value {.val {quant_value}}.
                          {.val sample} output_type will be used to calculate
                          the quantiles."))
          out_type_plot <- "sample"
        } else {
          cli::cli_abort(c("x" = "{.arg model_output_tbl} did not have the
                         expected output_type_id value {.val {quant_value}}"))
        }
      }
    } else if ("sample" %in% mod_out_type) {
      out_type_plot <- "sample"
    } else {
      cli::cli_abort(c("x" = "{.arg model_out_tbl} did not have the
                     expected output_type {.val sample} or {.val quantile}."))
    }
  } else {
    out_type_plot <- "sample"
    if (!"sample" %in% mod_out_type) {
      cli::cli_warn(c("!" = "{.arg plot_set_ahead_model_output()} was expecting
                          {.val sample} output_type due to {.arg intervals} set
                          to {.arg NULL}. {.arg model_out_tbl} is missing the
                          output_type {.val sample}. No intervals or samples
                          will be plotted."))
      out_type_plot <- NULL
    }
  }

  all_out_type <- unique(c(out_type_plot, out_type_med))
  if (is.null(intervals)) intervals <- "NULL"
  if (!all(mod_out_type %in% all_out_type) && !is.null(all_out_type)) {
    cli::cli_warn(c("!" = "{.arg plot_set_ahead_model_output()} was expecting
                          {.val {all_out_type}} output_type due to
                          {.arg intervals} set to {.val {intervals}} and
                          {.arg use_median_as_point} set to
                          {.val {use_median_as_point}}.
                          Additional output_type will be removed."))
    model_out_tbl <-
      dplyr::filter(model_out_tbl,
                    .data[["output_type"]] %in% all_out_type)
  }

  if (all(all_out_type %in% c("median", "quantile"))) {
    if (!"numeric" %in% class(model_out_tbl$output_type_id)) {
      model_out_tbl$output_type_id <-
        as.numeric(model_out_tbl$output_type_id)
      cli::cli_warn(c("!" = "{.arg output_type_id} column must be a numeric.
                    Converting to numeric."))
    }
  }
  model_out_tbl
}

#' Validate `"facet"` parameters format
#'
#' Validate `facet` is a column name of `model_out_tbl` (if not `NULL`) and
#' `facet_title` is one of: "top right", "top left" (default), "bottom right",
#' "bottom left" (if not `NULL`) .
#' If `interactive`, validate that `facet_nrow` is not greater than the number
#' of expected facet. If so a warning will be return a `facet_nrow` will be
#' restricted to the maximum number of possible facets.
#'
#' @param model_out_tbl a `model_out_tbl` object, containing all the
#'  required columns including a column containing date information and a
#'  column `value`.
#' @param facet a unique value corresponding to a task_id variable name
#' (interpretable as facet option for ggplot)
#' @param interactive a `boolean` to output an "interactive" version of the
#'  plot (using Plotly) or a "static" plot (using ggplot2). By default, `TRUE`
#'  (interactive plot)
#' @param facet_nrow a numeric, number of rows in the layout.
#' @param facet_title a `string`, position of each subplot tile (value
#'  associated with the `facet` parameter). "top right", "top left" (default),
#'  "bottom right", "bottom left" are the possible values, `NULL` to remove the
#'  title. For interactive plot only.
#'
#' @noRd
facet_validation <- function(model_out_tbl, facet = NULL,
                             interactive = TRUE, facet_nrow = NULL,
                             facet_title = "top left") {
  if (!is.null(facet)) {
    if ((length(facet) != 1) ||
          !all(facet %in%
                 setdiff(colnames(model_out_tbl),
                         hubUtils::std_colnames[names(hubUtils::std_colnames) !=
                                                  "model_id"]))) {
      cli::cli_abort(c("x" = "if {.arg facet} is not NULL, the argument should
                       be of length 1 and should match one of the task_id columns
                       of {.arg model_out_tbl}"))
    }
    facet_max <- length(unique(model_out_tbl[[facet]]))
    if ((interactive) && !is.null(facet_nrow)) {
      if (facet_nrow > facet_max) {
        cli::cli_warn(c("!" = "{.arg facet_nrow} should be less or equal to the
                    number of unique {.arg facet} values. By default, the
                    parameter will be set to {.val {facet_max}}"))
        facet_nrow <- facet_max
      }
    }
  }
  if (!is.null(facet_title)) {
    facet_title_opt <- c("top right", "top left", "bottom right", "bottom left")
    if (!facet_title %in% facet_title_opt) {
      cli::cli_abort(c("x" = "{.arg facet_title} should correspond to one of
                       these possible values: {.val {facet_title_opt}}"))
    }
  }
  return(facet_nrow)
}

#' Validate `"top_layer"` parameters format
#'
#' Validate `top_layer` is one of: "model_output", "target"
#'
#' @param top_layer character vector, where the first element indicates the top
#'  layer of the resulting plot. Possible options are `"model_output"` (default)
#'  and `"target"`
#'
#' @noRd
layer_validation <- function(top_layer) {
  if (!any(top_layer %in% c("model_output", "target"))) {
    cli::cli_abort(c("x" = "{.arg top_layer} should correspond to one of
                       these possible values: {.val model_output},
                     {.val target}"))
  }
}
