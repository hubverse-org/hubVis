# Imput format validation

mdl_out_validation <- function(model_output_data, col_names = NULL,
                               valid_types = c("median", "quantile")) {
  if (!is.data.frame(model_output_data)) {
    cli::cli_abort(c("x" = "{.arg model_output_data} must be a `data.frame`."))
  }
  if (isFALSE("model_out_tbl" %in% class(model_output_data))) {
    cli::cli_warn(c("!" = "{.arg model_output_data} must be a `model_out_tbl`.
                    Class applied by default"))
    model_output_data <- hubUtils::as_model_out_tbl(model_output_data,
                                                    remove_empty = TRUE)
  }

  if (!is.null(col_names)) {
    model_output_col <- colnames(model_output_data)
    if (!all(col_names %in% model_output_col)) {
      cli::cli_abort(c("x" = "{.arg model_output_data} did not have all required
                       columns {.val {exp_f_col}}"))
    }
  }

  model_output_type <- unique(model_output_data$output_type)
  if (!any(valid_types %in% model_output_type)) {
    cli::cli_abort(c(
      "x" = "{.arg model_output_data} should contain at least one supported
      output type.",
      "i" = "Supported output types: {.val {valid_types}}."
    ))
  }
}


target_validation <- function(target_data, col_names = NULL) {
  if (!is.data.frame(target_data)) {
    cli::cli_abort(c("x" = "{.arg target_data} must be a `data.frame`."))
  }
  if (!is.null(col_names)) {
    target_data_col <- colnames(target_data)
    if (!all(col_names %in% target_data_col)) {
      cli::cli_abort(c("x" = "{.arg target_data} did not have all required
                     columns {.val {exp_td_col}}"))
    }
  }

}


interval_validation <- function(model_output_data, intervals, list_intervals,
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
  if (length(unique(model_output_data[["model_id"]])) > max_model_id &&
        length(intervals) > 1) {
    intervals <- max(intervals)[1]
    cli::cli_warn(c("!" = "{.arg model_output_data} contains 6 or more models,
                      the plot will be reduced to show only one interval (the
                      maximum interval value): {.val {intervals}}"))
  }
  return(intervals)
}

ensemble_validation <- function(ens_color, ens_name) {
  if (is.null(ens_color) + is.null(ens_name) == 1) {
    cli::cli_abort(c("x" = "Both {.arg ens_color} and {.arg ens_name} should
                     be set to a non NULL value"))
  }
}


output_type_validation <- function(model_output_data, exp_value) {
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
  return(model_output_data)
}

facet_validation <- function(model_output_data, facet = NULL,
                             interactive = TRUE, facet_nrow = NULL,
                             facet_title = "top left") {
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
    facet_max <- length(unique(model_output_data[[facet]]))
    if ((interactive) && !is.null(facet_nrow)) {
      if (facet_nrow > facet_max) {
        cli::cli_warn(c("!" = "{.arg facet_nrow} should be less or equal to the
                    number of unique {.arg facet} value. By default, the
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


layer_validation <- function(top_layer) {
  if (!any(top_layer %in% c("model_output", "target"))) {
    cli::cli_abort(c("x" = "{.arg top_layer} should correspond to one of
                       these possible values: {.val model_output},
                     {.val target}"))
  }
}
