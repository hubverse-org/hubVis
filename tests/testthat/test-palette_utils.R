# Data Preparation
library(hubExamples)

## Model output - Scenario
projection_data <-
  dplyr::mutate(
    scenario_outputs,
    target_date = as.Date(origin_date) + (horizon * 7) - 1
  )
projection_data_a_us <-
  dplyr::filter(
    projection_data,
    scenario_id == "A-2021-03-05",
    location == "US"
  )

## Target data
target_data_us <-
  dplyr::filter(
    scenario_target_ts,
    location == "US",
    date < min(projection_data$target_date) + 21,
    date > "2020-10-01"
  )


# Test input information
test_that("Palette Input parameters", {
  ## Palette/Color input
  ### One color
  expect_warning(
    plot_step_ahead_model_output(
      projection_data_a_us,
      target_data_us,
      show_plot = FALSE,
      pal_color = NULL,
      one_color = "set2"
    ),
    "is not one of the accepted color name"
  )
  ### Pal color
  expect_warning(
    plot_step_ahead_model_output(
      projection_data_a_us,
      target_data_us,
      show_plot = FALSE,
      pal_color = "blue"
    ),
    "is not one of the accepted palette name"
  )
})
