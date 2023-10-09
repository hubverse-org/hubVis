# Data Preparation
projection_path <- system.file("example_round1.csv", package = "hubVis")
projection_data0 <- read.csv(projection_path)
projection_data <- dplyr::mutate(
  projection_data0, target_date = as.Date(origin_date) + (horizon * 7) - 1)
projection_data_A_us <- dplyr::filter(projection_data,
                                      scenario_id == "A-2021-03-05",
                                      location == "US")

truth_path <- system.file("truth_data.csv", package = "hubVis")
truth_data <- read.csv(truth_path)
truth_data_us <- dplyr::filter(truth_data, location == "US",
                               time_idx < min(projection_data$target_date) + 21,
                               time_idx > "2020-10-01")
# Test input information
testthat::test_that("Input parameters", {

  # model_output_data format
  testthat::expect_error(plot_step_ahead_model_output(
    as.list(projection_data_A_us), truth_data_us))
  testthat::expect_warning(plot_step_ahead_model_output(projection_data_A_us,
                                                        truth_data_us))
  projection_data_A_us <- hubUtils::as_model_out_tbl(projection_data_A_us)

  # Column
  testthat::expect_error(plot_step_ahead_model_output(
    projection_data_A_us, truth_data_us, x_col_name = "date"))
  testthat::expect_error(plot_step_ahead_model_output(
    projection_data_A_us, truth_data_us, fill_by = "model_name"))
  testthat::expect_error(plot_step_ahead_model_output(
    dplyr::rename(projection_data_A_us, type_id = output_type_id),
    truth_data_us))

  # Column format
  testthat::expect_error(plot_step_ahead_model_output(
    dplyr::mutate(projection_data_A_us,
                  target_date = gsub("-", "_", target_date)), truth_data_us))
  testthat::expect_error(plot_step_ahead_model_output(
    dplyr::mutate(projection_data_A_us,
                  value = as.character(value)), truth_data_us))

  # Output type
  testthat::expect_no_error(plot_step_ahead_model_output(
    dplyr::mutate(projection_data_A_us,
                  output_type = gsub("median", "mean", output_type)),
    truth_data_us))
  testthat::expect_error(plot_step_ahead_model_output(
    dplyr::mutate(projection_data_A_us, output_type = "cdf"),
    truth_data_us))

  # Truth Data
  testthat::expect_error(plot_step_ahead_model_output(projection_data_A_us,
                                                      as.list(truth_data_us)))
  testthat::expect_error(plot_step_ahead_model_output(
    projection_data_A_us, truth_data_us, x_truth_col_name = "date"))

  # Intervals & Median value
  testthat::expect_warning(plot_step_ahead_model_output(
    projection_data_A_us, truth_data_us, intervals = c(0.6, 0.75)))

  d_proj = rbind(projection_data_A_us,
                 dplyr::mutate(projection_data_A_us,
                               model_id = paste0(model_id, "_2")))

  testthat::expect_warning(plot_step_ahead_model_output(
    d_proj, truth_data_us))
  testthat::expect_no_error(plot_step_ahead_model_output(
    d_proj, truth_data_us, intervals = 0.5))
  testthat::expect_error(plot_step_ahead_model_output(
    dplyr::filter(projection_data_A_us, output_type_id != 0.5),
    truth_data_us, use_median_as_point = TRUE))
  testthat::expect_no_error(plot_step_ahead_model_output(
    dplyr::filter(projection_data_A_us, output_type_id != 0.5),
    truth_data_us))

  # Parameter input
  testthat::expect_error(plot_step_ahead_model_output(
    projection_data_A_us, truth_data_us, ens_color = "black"))
  testthat::expect_error(plot_step_ahead_model_output(
    projection_data_A_us, truth_data_us, facet = "value"))
  testthat::expect_error(plot_step_ahead_model_output(
    projection_data_A_us, truth_data_us, facet = c("target", "scenario_id")))
  testthat::expect_error(plot_step_ahead_model_output(
    projection_data_A_us, truth_data_us, facet_title = "center"))
  testthat::expect_error(plot_step_ahead_model_output(
    projection_data_A_us, truth_data_us, top_layer = "model"))

  # Palette/Color input
  testthat::expect_warning(plot_step_ahead_model_output(
    projection_data_A_us, truth_data_us, one_color = "set2", pal_color = NULL))
  testthat::expect_warning(plot_step_ahead_model_output(
    projection_data_A_us, truth_data_us, pal_color = "blue"))
})
