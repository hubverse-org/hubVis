# Data Preparation
projection_path <- system.file("example_round1.csv", package = "hubVis")
projection_data0 <- read.csv(projection_path)
projection_data <- dplyr::mutate(
  projection_data0, target_date = as.Date(origin_date) + (horizon * 7) - 1)
projection_data_A_us <- dplyr::filter(projection_data,
                                      scenario_id == "A-2021-03-05",
                                      location == "US")

d_proj = rbind(projection_data_A_us,
               dplyr::mutate(projection_data_A_us,
                             model_id = paste0(model_id, "_2")))
d_proj = hubUtils::as_model_out_tbl(d_proj)

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

# Update data
projection_data_A_us <- hubUtils::as_model_out_tbl(projection_data_A_us)
projection_data <- hubUtils::as_model_out_tbl(projection_data)

# Test output
testthat::test_that("Output", {

  # Class
  testthat::expect_s3_class(
    plot_step_ahead_model_output(projection_data_A_us, truth_data_us,
                                 interactive = FALSE),
    "ggplot")
  testthat::expect_s3_class(
    plot_step_ahead_model_output(projection_data_A_us, truth_data_us,
                                 interactive = TRUE),
    "plotly")

  # Invisible
  testthat::expect_invisible(plot_step_ahead_model_output(
    projection_data, truth_data_us, show_plot = FALSE))

  # Layout (static)
  plot_test <- plot_step_ahead_model_output(
    projection_data_A_us, truth_data_us, interactive = FALSE, title = "My Plot",
    facet = "scenario_id", use_median_as_point = TRUE)
  testthat::expect_equal(plot_test$labels$title, "My Plot")
  testthat::expect_equal(names(plot_test$facet$params$facets), "scenario_id")
  testthat::expect_equal(
    unique(tail(plot_test$layers, 1)[[1]]$data$output_type), "median")

  plot_test <- plot_step_ahead_model_output(
    projection_data, truth_data_us, interactive = FALSE, facet = "scenario_id",
    top_layer = "truth", facet_scales = "free_x")
  testthat::expect_in(names(tail(plot_test$layers, 1)[[1]]$data),
                      c("time_idx", "location", "value", "target"))
  testthat::expect_true(plot_test$facet$params$free$x)
  testthat::expect_false(plot_test$facet$params$free$y)

  plot_test <- plot_step_ahead_model_output(
    dplyr::filter(projection_data_A_us, model_id != "HUBuni-simexamp"),
    truth_data_us, interactive = FALSE, ens_color = "black",
    ens_name = "hub-ensemble", use_median_as_point = TRUE, show_legend = FALSE)
  testthat::expect_equal(plot_test$guides$fill, "none")
  testthat::expect_equal(plot_test$guides$colour, "none")
  testthat::expect_equal(tail(plot_test$layers, 1)[[1]]$geom$default_aes$colour,
                         "black")

  # Layout (interactive)
  projection_data <- dplyr::filter(projection_data, output_type == "quantile")
  plot_test <- plot_step_ahead_model_output(
    projection_data, truth_data_us, facet = "scenario_id",
    use_median_as_point = TRUE, intervals = NULL, plot_truth = FALSE,
    ens_color = "black", ens_name = "hub-ensemble", facet_scales = "free_x",
    pal_color = "Set1")
  testthat::expect_length(plot_test$x$data,
                          length(unique(projection_data$model_id)) *
                            length(unique(projection_data$scenario_id)))
  testthat::expect_equal(
    unique(unlist(purrr::map(plot_test$x$data[purrr::map(
      plot_test$x$data, "name") == "hub-ensemble"], "line"))["color"]), "black")
  testthat::expect_true(plot_test$x$subplot)
  testthat::expect_gt(length(unique(purrr::map(plot_test$x$data, "xaxis"))), 1)
  testthat::expect_equal(length(unique(purrr::map(plot_test$x$data, "yaxis"))),
                         1)
  testthat::expect_equal(
    unique(unlist(purrr::map(plot_test$x$attrs, "colors"))), "Set1")

  plot_test <- plot_step_ahead_model_output(
    projection_data, dplyr::mutate(truth_data_us, scenario_id = "A-2021-03-05"),
    facet = "scenario_id", plot_truth = TRUE, facet_scales = "free",
    show_legend = FALSE, use_median_as_point = TRUE)
  testthat::expect_false(plot_test$x$layoutAttrs[[1]]$showlegend)
  testthat::expect_gt(length(unique(purrr::map(plot_test$x$data, "yaxis"))), 1)
  testthat::expect_gt(length(unique(purrr::map(plot_test$x$data, "xaxis"))), 1)
  testthat::expect_equal(
    length(purrr::discard(purrr::map(plot_test$x$data[
      purrr::map(plot_test$x$data, "name") == "ground truth"], "x"), is.null)),
    1
  )

  plot_test <- plot_step_ahead_model_output(
    projection_data, truth_data_us, top_layer = "truth",
    facet = "model_id", fill_by = "scenario_id", facet_title = "bottom right")
  testthat::expect_equal(tail(plot_test$x$data, 1)[[1]]$name, "ground truth")
  testthat::expect_equal(
    unique(unlist(purrr::map(plot_test$x$layout$annotations, "xanchor"))),
    "right")
  testthat::expect_equal(
    unique(unlist(purrr::map(plot_test$x$layout$annotations, "yanchor"))),
    "bottom")
  testthat::expect_in(
    unlist(purrr::map(plot_test$x$layout$annotations, "text")),
    unique(projection_data$model_id)
  )
  testthat::expect_in(
    unique(unlist(purrr::map(plot_test$x$attrs, "color")))  ,
    unique(projection_data$scenario_id)
  )

  plot_test <- plot_step_ahead_model_output(
    projection_data, truth_data_us, facet = "model_id", title = "My Plot",
    facet_scales = "free_y", facet_nrow = 3, fill_transparency = 0.75,
    use_median_as_point = TRUE)
  testthat::expect_gt(length(unique(purrr::map(plot_test$x$data, "yaxis"))), 1)
  testthat::expect_equal(length(unique(purrr::map(plot_test$x$data, "xaxis"))),
                         1)
  testthat::expect_equal(plot_test$x$layoutAttrs[[2]]$title, "My Plot")
  testthat::expect_in(
    unlist(purrr::map(plot_test$x$layout$annotations, "text")),
    unique(projection_data$model_id)
  )
  testthat::expect_in(
    unique(unlist(purrr::map(plot_test$x$attrs, "color")))  ,
    unique(projection_data$model_id)
  )
  testthat::expect_equal(tail(plot_test$x$data, 1)[[1]]$opacity, 0.75)

  plot_test <- plot_step_ahead_model_output(
    projection_data, truth_data_us, pal_color = NULL, one_color = "orange",
    ens_color = "black", ens_name = "hub-ensemble", intervals = NULL,
    use_median_as_point = TRUE, facet = "scenario_id")
  testthat::expect_equal(
    unique(unlist(purrr::map(plot_test$x$data[purrr::map(
      plot_test$x$data, "name") == "hub-ensemble"], "line"))["color"]), "black")
  testthat::expect_equal(
    unique(unlist(purrr::map(plot_test$x$data[purrr::map(
      plot_test$x$data, "name") == "HUBuni-simexamp"], "line"))["color"]),
    "rgba(255,165,0,1)")

  plot_test <- plot_step_ahead_model_output(
    projection_data_A_us, truth_data_us, intervals = 0.9, ens_color = "black",
    ens_name = "hub-ensemble", facet = "model_id")
  testthat::expect_equal(
    strsplit(strsplit(tail(purrr::map(
      plot_test$x$attrs, "hovertext"), 1)[[1]], "<br>")[[1]][2],
      "Intervals")[[1]][1], "90%")

  })
