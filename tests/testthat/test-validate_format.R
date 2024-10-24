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


# Test input information
test_that("Input parameters", {
  # Data preparation
  d_proj <- rbind(projection_data_a_us,
                  dplyr::mutate(projection_data_a_us,
                                model_id = paste0(model_id, "_2")))
  d_proj <- hubUtils::as_model_out_tbl(d_proj)

  # Model output format
  ## List
  expect_snapshot(plot_step_ahead_model_output(as.list(projection_data_a_us),
                                               target_data_us), error = TRUE)
  ##  Data Frame
  df_test <- as.data.frame(projection_data_a_us)
  expect_snapshot(plot_step_ahead_model_output(df_test, target_data_us,
                                               show_plot = FALSE))

  # Model output format - model_out_tbl
  projection_data_a_us <- hubUtils::as_model_out_tbl(projection_data_a_us)

  # Output type format - character
  df_test <- dplyr::mutate(projection_data_a_us,
                           output_type_id = as.character(output_type_id))
  expect_snapshot(plot_step_ahead_model_output(df_test, target_data_us,
                                               show_plot = FALSE))

  # Task id columns age group - NA
  df_test <- dplyr::mutate(projection_data_a_us, age_group = NA)
  expect_snapshot(plot_step_ahead_model_output(df_test, target_data_us,
                                               show_plot = FALSE))

  # Parameter link to column names
  ## X Axis
  expect_snapshot(plot_step_ahead_model_output(projection_data_a_us,
                                               target_data_us,
                                               show_plot = FALSE,
                                               x_col_name = "date"),
                  error = TRUE)
  ## Fill by
  expect_snapshot(plot_step_ahead_model_output(projection_data_a_us,
                                               target_data_us,
                                               show_plot = FALSE,
                                               fill_by = "model_name"),
                  error = TRUE)
  ## Group
  expect_snapshot(plot_step_ahead_model_output(projection_data_a_us,
                                               target_data_us,
                                               show_plot = FALSE,
                                               group = "forecast_date"),
                  error = TRUE)
  ## Output type ID
  df_test <- dplyr::rename(projection_data_a_us, type_id = output_type_id)
  expect_snapshot(plot_step_ahead_model_output(df_test, target_data_us,
                                               show_plot = FALSE), error = TRUE)

  # Column format & content
  ## Output type ID - unexpected value
  df_test <- dplyr::filter(projection_data_a_us, output_type_id == 0.5)
  expect_snapshot(plot_step_ahead_model_output(df_test, target_data_us,
                                               show_plot = FALSE),
                  error = TRUE)
  ## Value - character
  df_test <- dplyr::mutate(projection_data_a_us, value = as.character(value))
  expect_snapshot(plot_step_ahead_model_output(df_test, target_data_us,
                                               show_plot = FALSE),
                  error = TRUE)

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
  expect_snapshot(plot_step_ahead_model_output(df_test, target_data_us,
                                               show_plot = FALSE),
                  error = TRUE)

  # Target Data
  ## List
  expect_snapshot(plot_step_ahead_model_output(projection_data_a_us,
                                               as.list(target_data_us),
                                               show_plot = FALSE),
                  error = TRUE)
  ## Column Name
  expect_snapshot(plot_step_ahead_model_output(projection_data_a_us,
                                               target_data_us,
                                               show_plot = FALSE,
                                               x_target_col_name = "time_idx"),
                  error = TRUE)

  # Intervals & Median value
  ## Interval parameter
  expect_snapshot(plot_step_ahead_model_output(projection_data_a_us,
                                               target_data_us,
                                               show_plot = FALSE,
                                               intervals = c(0.6, 0.75)))
  ## Number of unique model_id
  expect_snapshot(plot_step_ahead_model_output(d_proj, target_data_us,
                                               show_plot = FALSE))
  ## 50% Interval
  expect_no_error(plot_step_ahead_model_output(d_proj, target_data_us,
                                               show_plot = FALSE,
                                               intervals = 0.5))
  # Median
  df_test <- dplyr::filter(projection_data_a_us, output_type_id != 0.5)
  expect_snapshot(plot_step_ahead_model_output(df_test, target_data_us,
                                               show_plot = FALSE,
                                               use_median_as_point = TRUE),
                  error = TRUE)
  expect_no_error(plot_step_ahead_model_output(df_test, target_data_us,
                                               show_plot = FALSE))

  # Parameter input
  ## Parameters `ens_name` & `ens_color`
  expect_snapshot(plot_step_ahead_model_output(projection_data_a_us,
                                               target_data_us,
                                               show_plot = FALSE,
                                               ens_color = "black"),
                  error = TRUE)
  ## Facet
  ### Column Name
  expect_snapshot(plot_step_ahead_model_output(projection_data_a_us,
                                               target_data_us,
                                               show_plot = FALSE,
                                               facet = "value"),
                  error = TRUE)
  ### Facet row number
  df_test <- hubUtils::as_model_out_tbl(projection_data)
  expect_snapshot(plot_step_ahead_model_output(df_test, target_data_us,
                                               facet = "scenario_id",
                                               show_plot = FALSE,
                                               facet_nrow = 5))
  ### Parameter length
  expect_snapshot(plot_step_ahead_model_output(projection_data_a_us,
                                               target_data_us,
                                               show_plot = FALSE,
                                               facet = c("target",
                                                         "scenario_id")),
                  error = TRUE)
  ### Title location
  expect_snapshot(plot_step_ahead_model_output(projection_data_a_us,
                                               target_data_us,
                                               show_plot = FALSE,
                                               facet_title = "center"),
                  error = TRUE)
  ### Top Layer
  expect_snapshot(plot_step_ahead_model_output(projection_data_a_us,
                                               target_data_us,
                                               show_plot = FALSE,
                                               top_layer = "model"),
                  error = TRUE)
})
