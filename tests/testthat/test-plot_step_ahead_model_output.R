# Data Preparation
projection_path <- system.file("example_round1.csv", package = "hubVis")
projection_data0 <- read.csv(projection_path, stringsAsFactors = FALSE)
projection_data <- dplyr::mutate(
  projection_data0, target_date = as.Date(origin_date) + (horizon * 7) - 1)
projection_data_A_us <- dplyr::filter(projection_data,
                                      scenario_id == "A-2021-03-05",
                                      location == "US")

d_proj <- rbind(projection_data_A_us,
               dplyr::mutate(projection_data_A_us,
                             model_id = paste0(model_id, "_2")))
d_proj <- hubUtils::as_model_out_tbl(d_proj)

target_path <- system.file("target_data.csv", package = "hubVis")
target_data <- read.csv(target_path, stringsAsFactors = FALSE)
target_data_us <- dplyr::filter(
  target_data, location == "US",
  time_idx < min(projection_data$target_date) + 21, time_idx > "2020-10-01")
# Test input information
test_that("Input parameters", {

  # model_output_data format
  expect_error(plot_step_ahead_model_output(
    as.list(projection_data_A_us), target_data_us))
  expect_warning(plot_step_ahead_model_output(projection_data_A_us,
                                                        target_data_us))
  projection_data_A_us <- hubUtils::as_model_out_tbl(projection_data_A_us)
  projection_data_A_us_w <-  dplyr::mutate(
    projection_data_A_us, output_type_id = as.character(output_type_id))
  expect_warning(plot_step_ahead_model_output(projection_data_A_us_w,
                                              target_data_us))


  # Column
  expect_error(plot_step_ahead_model_output(
    projection_data_A_us, target_data_us, x_col_name = "date"))
  expect_error(plot_step_ahead_model_output(
    projection_data_A_us, target_data_us, fill_by = "model_name"))
  expect_error(plot_step_ahead_model_output(
    dplyr::rename(projection_data_A_us, type_id = output_type_id),
    target_data_us))

  # Column format
  expect_error(plot_step_ahead_model_output(
    dplyr::mutate(projection_data_A_us,
                  target_date = gsub("-", "_", target_date)), target_data_us))
  expect_error(plot_step_ahead_model_output(
    dplyr::mutate(projection_data_A_us,
                  value = as.character(value)), target_data_us))

  # Output type
  expect_no_error(plot_step_ahead_model_output(
    dplyr::mutate(projection_data_A_us,
                  output_type = gsub("median", "mean", output_type)),
    target_data_us))
  expect_error(plot_step_ahead_model_output(
    dplyr::mutate(projection_data_A_us, output_type = "cdf"),
    target_data_us))

  # target Data
  expect_error(plot_step_ahead_model_output(projection_data_A_us,
                                                      as.list(target_data_us)))
  expect_error(plot_step_ahead_model_output(
    projection_data_A_us, target_data_us, x_target_col_name = "date"))

  # Intervals & Median value
  expect_warning(plot_step_ahead_model_output(
    projection_data_A_us, target_data_us, intervals = c(0.6, 0.75)))
  expect_warning(plot_step_ahead_model_output(
    d_proj, target_data_us))
  expect_no_error(plot_step_ahead_model_output(
    d_proj, target_data_us, intervals = 0.5))
  expect_error(plot_step_ahead_model_output(
    dplyr::filter(projection_data_A_us, output_type_id != 0.5),
    target_data_us, use_median_as_point = TRUE))
  expect_no_error(plot_step_ahead_model_output(
    dplyr::filter(projection_data_A_us, output_type_id != 0.5),
    target_data_us))

  # Parameter input
  expect_error(plot_step_ahead_model_output(
    projection_data_A_us, target_data_us, ens_color = "black"))
  expect_error(plot_step_ahead_model_output(
    projection_data_A_us, target_data_us, facet = "value"))
  testthat::expect_warning(plot_step_ahead_model_output(
    hubUtils::as_model_out_tbl(projection_data), target_data_us,
    facet = "scenario_id", facet_nrow = 5))
  expect_error(plot_step_ahead_model_output(
    projection_data_A_us, target_data_us, facet = c("target", "scenario_id")))
  expect_error(plot_step_ahead_model_output(
    projection_data_A_us, target_data_us, facet_title = "center"))
  expect_error(plot_step_ahead_model_output(
    projection_data_A_us, target_data_us, top_layer = "model"))

  # Palette/Color input
  expect_warning(plot_step_ahead_model_output(
    projection_data_A_us, target_data_us, one_color = "set2", pal_color = NULL))
  expect_warning(plot_step_ahead_model_output(
    projection_data_A_us, target_data_us, pal_color = "blue"))
})

# Update data
projection_data_A_us <- hubUtils::as_model_out_tbl(projection_data_A_us)
projection_data <- hubUtils::as_model_out_tbl(projection_data)

# Test output
test_that("Output", {

  # Class
  expect_s3_class(
    plot_step_ahead_model_output(projection_data_A_us, target_data_us,
                                 interactive = FALSE),
    "ggplot")
  expect_s3_class(
    plot_step_ahead_model_output(projection_data_A_us, target_data_us,
                                 interactive = TRUE),
    "plotly")

  # Invisible
  expect_invisible(plot_step_ahead_model_output(
    projection_data, target_data_us, show_plot = FALSE))

  # Layout (static)
  plot_test <- plot_step_ahead_model_output(
    projection_data_A_us, target_data_us, interactive = FALSE, title = "My Plot",
    facet = "scenario_id", use_median_as_point = TRUE)
  expect_equal(plot_test$labels$title, "My Plot")
  expect_equal(names(plot_test$facet$params$facets), "scenario_id")
  expect_equal(
    unique(tail(plot_test$layers, 1)[[1]]$data$output_type), "median")

  plot_test <- plot_step_ahead_model_output(
    projection_data, target_data_us, interactive = FALSE, facet = "scenario_id",
    top_layer = "target", facet_scales = "free_x")
  expect_in(names(tail(plot_test$layers, 1)[[1]]$data),
                      c("time_idx", "location", "value", "target"))
  expect_true(plot_test$facet$params$free$x)
  expect_false(plot_test$facet$params$free$y)

  plot_test <- plot_step_ahead_model_output(
    dplyr::filter(projection_data_A_us, model_id != "HUBuni-simexamp"),
    target_data_us, interactive = FALSE, ens_color = "black",
    ens_name = "hub-ensemble", use_median_as_point = TRUE, show_legend = FALSE)
  expect_equal(plot_test$guides$fill, "none")
  expect_equal(plot_test$guides$colour, "none")
  expect_equal(tail(plot_test$layers, 1)[[1]]$geom$default_aes$colour,
                         "black")

  # Layout (interactive)
  projection_data <- dplyr::filter(projection_data, output_type == "quantile")
  plot_test <- plot_step_ahead_model_output(
    projection_data, target_data_us, facet = "scenario_id",
    use_median_as_point = TRUE, intervals = NULL, plot_target = FALSE,
    ens_color = "black", ens_name = "hub-ensemble", facet_scales = "free_x",
    pal_color = "Set1")
  expect_length(plot_test$x$data,
                          length(unique(projection_data$model_id)) *
                            length(unique(projection_data$scenario_id)))
  expect_equal(
    unique(unlist(purrr::map(plot_test$x$data[purrr::map(
      plot_test$x$data, "name") == "hub-ensemble"], "line"))["color"]), "black")
  expect_true(plot_test$x$subplot)
  expect_gt(length(unique(purrr::map(plot_test$x$data, "xaxis"))), 1)
  expect_equal(length(unique(purrr::map(plot_test$x$data, "yaxis"))),
                         1)
  expect_equal(
    unique(unlist(purrr::map(plot_test$x$attrs, "colors"))), "Set1")

  plot_test <- plot_step_ahead_model_output(
    projection_data, dplyr::mutate(target_data_us, scenario_id = "A-2021-03-05"),
    facet = "scenario_id", plot_target = TRUE, facet_scales = "free",
    show_legend = FALSE, use_median_as_point = TRUE)
  expect_false(plot_test$x$layoutAttrs[[1]]$showlegend)
  expect_gt(length(unique(purrr::map(plot_test$x$data, "yaxis"))), 1)
  expect_gt(length(unique(purrr::map(plot_test$x$data, "xaxis"))), 1)
  expect_equal(
    length(purrr::discard(purrr::map(plot_test$x$data[
      purrr::map(plot_test$x$data, "name") == "target"], "x"), is.null)),
    1
  )

  plot_test <- plot_step_ahead_model_output(
    projection_data, target_data_us, top_layer = "target",
    facet = "model_id", fill_by = "scenario_id", facet_title = "bottom right")
  expect_equal(tail(plot_test$x$data, 1)[[1]]$name, "target")
  expect_equal(
    unique(unlist(purrr::map(plot_test$x$layout$annotations, "xanchor"))),
    "right")
  expect_equal(
    unique(unlist(purrr::map(plot_test$x$layout$annotations, "yanchor"))),
    "bottom")
  expect_in(
    unlist(purrr::map(plot_test$x$layout$annotations, "text")),
    unique(projection_data$model_id)
  )
  expect_in(
    unique(unlist(purrr::map(plot_test$x$attrs, "color")))  ,
    unique(projection_data$scenario_id)
  )

  plot_test <- plot_step_ahead_model_output(
    projection_data, target_data_us, facet = "model_id", title = "My Plot",
    facet_scales = "free_y", facet_nrow = 3, fill_transparency = 0.75,
    use_median_as_point = TRUE)
  expect_gt(length(unique(purrr::map(plot_test$x$data, "yaxis"))), 1)
  expect_equal(length(unique(purrr::map(plot_test$x$data, "xaxis"))),
                         1)
  expect_equal(plot_test$x$layoutAttrs[[2]]$title, "My Plot")
  expect_in(
    unlist(purrr::map(plot_test$x$layout$annotations, "text")),
    unique(projection_data$model_id)
  )
  expect_in(
    unique(unlist(purrr::map(plot_test$x$attrs, "color")))  ,
    unique(projection_data$model_id)
  )
  expect_equal(tail(plot_test$x$data, 1)[[1]]$opacity, 0.75)

  plot_test <- plot_step_ahead_model_output(
    projection_data, target_data_us, pal_color = NULL, one_color = "orange",
    ens_color = "black", ens_name = "hub-ensemble", intervals = NULL,
    use_median_as_point = TRUE, facet = "scenario_id")
  expect_equal(
    unique(unlist(purrr::map(plot_test$x$data[purrr::map(
      plot_test$x$data, "name") == "hub-ensemble"], "line"))["color"]), "black")
  expect_equal(
    unique(unlist(purrr::map(plot_test$x$data[purrr::map(
      plot_test$x$data, "name") == "HUBuni-simexamp"], "line"))["color"]),
    "rgba(255,165,0,1)")

  plot_test <- plot_step_ahead_model_output(
    projection_data_A_us, target_data_us, intervals = 0.9, ens_color = "black",
    ens_name = "hub-ensemble", facet = "model_id")
  expect_equal(
    strsplit(strsplit(tail(purrr::map(
      plot_test$x$attrs, "hovertext"), 1)[[1]], "<br>")[[1]][2],
      "Intervals")[[1]][1], "90%")

  })
