# Data Preparation
library(hubExamples)

## Model output - Scenario
projection_data <-
  dplyr::mutate(scenario_outputs,
                target_date = as.Date(origin_date) + (horizon * 7) - 1)
projection_data_a_us <-
  dplyr::filter(projection_data, scenario_id == "A-2021-03-05",
                location == "US")

## Target data
target_data_us <-
  dplyr::filter(scenario_target_ts, location == "US",
                date < min(projection_data$target_date) + 21,
                date > "2020-10-01")

## Model output - Forecast
forecast_data <- dplyr::filter(forecast_outputs, location == "48")
target_data_48 <- dplyr::filter(forecast_target_ts, location == "48")


# Test input information
test_that("Input parameters", {
  # Data preparation
  d_proj <- rbind(projection_data_a_us,
                  dplyr::mutate(projection_data_a_us,
                                model_id = paste0(model_id, "_2")))
  d_proj <- hubUtils::as_model_out_tbl(d_proj)

  # Model output format
  ## List
  expect_error(plot_step_ahead_model_output(as.list(projection_data_a_us),
                                            target_data_us),
               "`model_out_tbl` must be a `data.frame`.")
  ##  Data Frame
  df_test <- as.data.frame(projection_data_a_us)
  expect_warning(plot_step_ahead_model_output(df_test, target_data_us,
                                              show_plot = FALSE),
                 "must be a `model_out_tbl`. Class applied by default")

  # Model output format - model_out_tbl
  projection_data_a_us <- hubUtils::as_model_out_tbl(projection_data_a_us)

  # Model output type
  forecast_quantile <-
    dplyr::filter(forecast_data, output_type == "quantile") |>
    dplyr::mutate(output_type_id = as.numeric(output_type_id))
  err_mess <- paste0("did not have the expected output_type_id value 0.975, ",
                     "0.025, 0.9, 0.1, 0.75, and 0.25")
  expect_error(plot_step_ahead_model_output(forecast_quantile, target_data_us,
                                            x_col_name = "target_end_date"),
               err_mess)

  expect_warning(plot_step_ahead_model_output(forecast_quantile, target_data_us,
                                              x_col_name = "target_end_date",
                                              intervals = NULL))
  expect_no_error(plot_step_ahead_model_output(forecast_quantile, target_data_us,
                                               x_col_name = "target_end_date",
                                               intervals = NULL,
                                               use_median_as_point = TRUE) |>
                    suppressWarnings())
  expect_warning(plot_step_ahead_model_output(
    dplyr::filter(forecast_data, output_type_id != 0.5,
                  output_type != "median"), target_data_us,
    use_median_as_point = TRUE, x_col_name = "target_end_date"
    ))

  expect_message(plot_step_ahead_model_output(
    dplyr::filter(forecast_data, output_type == "sample"), target_data_us,
    use_median_as_point = TRUE, x_col_name = "target_end_date", intervals = 0.5
  ))

  forecast_median <- dplyr::filter(forecast_data, output_type == "median")
  expect_no_error(plot_step_ahead_model_output(forecast_median, target_data_us,
                                               x_col_name = "target_end_date",
                                               intervals = NULL))
  expect_warning(plot_step_ahead_model_output(forecast_median, target_data_us,
                                              x_col_name = "target_end_date",
                                              intervals = NULL,
                                              use_median_as_point = TRUE))
  expect_error(plot_step_ahead_model_output(forecast_median, target_data_us,
                                            x_col_name = "target_end_date",
                                            intervals = 0.9),
               '`model_out_tbl` did not have the expected output_type "sample"')

  # Output type format - character
  df_test <- dplyr::mutate(projection_data_a_us,
                           output_type_id = as.character(output_type_id))
  expect_warning(plot_step_ahead_model_output(df_test, target_data_us,
                                              show_plot = FALSE),
                 "column must be a numeric. Converting to numeric.")

  # Task id columns age group - NA
  df_test <- dplyr::mutate(projection_data_a_us, age_group = NA)
  expect_warning(plot_step_ahead_model_output(df_test, target_data_us,
                                              show_plot = FALSE),
                 "contains some empty columns: age_group.")

  # Parameter link to column names
  ## X Axis
  expect_error(plot_step_ahead_model_output(projection_data_a_us,
                                            target_data_us, show_plot = FALSE,
                                            x_col_name = "date"),
               "did not have all required columns")
  ## Fill by
  expect_error(plot_step_ahead_model_output(projection_data_a_us,
                                            target_data_us,
                                            show_plot = FALSE,
                                            fill_by = "model_name"),
               "did not have all required columns ")
  ## Group
  expect_error(plot_step_ahead_model_output(projection_data_a_us,
                                            target_data_us, show_plot = FALSE,
                                            group = "forecast_date"),
               "did not have all required columns ")
  ## Output type ID
  df_test <- dplyr::rename(projection_data_a_us, type_id = output_type_id)
  expect_error(plot_step_ahead_model_output(df_test, target_data_us,
                                            show_plot = FALSE),
               "did not have all required columns")

  # Column format & content
  ## Output type ID - unexpected value
  df_test <- dplyr::filter(projection_data_a_us, output_type_id == 0.5)
  expect_error(plot_step_ahead_model_output(df_test, target_data_us,
                                            show_plot = FALSE),
               "did not have the expected output_type_id value")
  ## Value - character
  df_test <- dplyr::mutate(projection_data_a_us, value = as.character(value))
  expect_error(plot_step_ahead_model_output(df_test, target_data_us,
                                            show_plot = FALSE),
               "non-numeric argument to mathematical function")

  # Output type
  df_test <- dplyr::filter(projection_data_a_us, output_type_id == 0.5)
  df_test$output_type <- "median"
  df_test$output_type_id <- NA
  df_test <- rbind(df_test, projection_data_a_us)
  ## Add Median
  expect_no_error(plot_test <-
                    plot_step_ahead_model_output(df_test, target_data_us,
                                                 use_median_as_point = TRUE))
  ## Add Mean
  df_test <- dplyr::mutate(projection_data_a_us,
                           output_type = gsub("median", "mean", output_type))
  expect_no_error(plot_step_ahead_model_output(df_test, target_data_us,
                                               show_plot = FALSE))
  ## Add CDF
  df_test <- dplyr::mutate(projection_data_a_us, output_type = "cdf")
  expect_error(plot_step_ahead_model_output(df_test, target_data_us,
                                            show_plot = FALSE),
               "should contain at least one supported output type")

  # Target Data
  ## List
  expect_error(plot_step_ahead_model_output(projection_data_a_us,
                                            as.list(target_data_us),
                                            show_plot = FALSE),
               "`target_data` must be a `data.frame`.")
  ## Column Name
  expect_error(plot_step_ahead_model_output(projection_data_a_us,
                                            target_data_us, show_plot = FALSE,
                                            x_target_col_name = "time_idx"),
               "`target_data` did not have all required columns")

  # Intervals & Median value
  ## Interval parameter
  expect_warning(plot_step_ahead_model_output(projection_data_a_us,
                                              target_data_us, show_plot = FALSE,
                                              intervals = c(0.6, 0.75)),
                 "should correspond to one or multiple of these possible value")
  ## Number of unique model_id
  expect_warning(plot_step_ahead_model_output(d_proj, target_data_us,
                                              show_plot = FALSE),
                 "the plot will be reduced to show only one interval")
  ## 50% Interval
  expect_no_error(plot_step_ahead_model_output(d_proj, target_data_us,
                                               show_plot = FALSE,
                                               intervals = 0.5))
  # Median
  df_test <- dplyr::filter(projection_data_a_us, output_type_id != 0.5)
  expect_error(plot_step_ahead_model_output(df_test, target_data_us,
                                            show_plot = FALSE,
                                            use_median_as_point = TRUE),
               " is missing the expected output_type_id value")
  expect_no_error(plot_step_ahead_model_output(df_test, target_data_us,
                                               show_plot = FALSE))

  # Parameter input
  ## Parameters `ens_name` & `ens_color`
  expect_error(plot_step_ahead_model_output(projection_data_a_us,
                                            target_data_us, show_plot = FALSE,
                                            ens_color = "black"),
               "Both `ens_color` and `ens_name` should be set to a non NULL ")
  ## Facet
  ### Column Name
  expect_error(plot_step_ahead_model_output(projection_data_a_us,
                                            target_data_us, show_plot = FALSE,
                                            facet = "value"),
               " be of length 1 and should match one of the task_id columns")
  ### Facet row number
  df_test <- hubUtils::as_model_out_tbl(projection_data)
  expect_warning(plot_step_ahead_model_output(df_test, target_data_us,
                                              facet = "scenario_id",
                                              show_plot = FALSE,
                                              facet_nrow = 5),
                 "should be less or equal to the number of unique `facet` ")
  ### Parameter length
  expect_error(plot_step_ahead_model_output(projection_data_a_us,
                                            target_data_us, show_plot = FALSE,
                                            facet = c("target", "scenario_id")),
               "if `facet` is not NULL, the argument should be of length 1 ")
  ### Title location
  expect_error(plot_step_ahead_model_output(projection_data_a_us,
                                            target_data_us, show_plot = FALSE,
                                            facet_title = "center"),
               "should correspond to one of these possible values: ")
  ### Top Layer
  expect_error(plot_step_ahead_model_output(projection_data_a_us,
                                            target_data_us, show_plot = FALSE,
                                            top_layer = "model"),
               "should correspond to one of these possible values: ")
})
