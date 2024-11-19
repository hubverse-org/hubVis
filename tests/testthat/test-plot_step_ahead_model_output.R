# Data Preparation
library(hubExamples)

## Model output - Scenario
projection_data <-
  dplyr::mutate(scenario_outputs,
                target_date = as.Date(origin_date) + (horizon * 7) - 1)
projection_data_a_us <-
  dplyr::filter(projection_data, scenario_id == "A-2021-03-05",
                location == "US")
projection_data <- hubUtils::as_model_out_tbl(projection_data)
projection_data_a_us <- hubUtils::as_model_out_tbl(projection_data_a_us)

## Target data
target_data_us <-
  dplyr::filter(scenario_target_ts, location == "US",
                date < min(projection_data$target_date) + 21,
                date > "2020-10-01")
proj_data_q <- dplyr::filter(projection_data, output_type == "quantile")
test_date <- unique(projection_data_a_us$target_date)[1:8]
static_proj <- dplyr::filter(projection_data_a_us, target_date %in% test_date)
static_proj <-
  dplyr::mutate(static_proj,
                forecast_date = ifelse(target_date <= test_date[4],
                                       as.character(test_date[1]),
                                       as.character(test_date[5])))

# Test output
test_that("Output", {

  # Class
  ## GGPLOT
  expect_s3_class(plot_test <-
                    plot_step_ahead_model_output(projection_data_a_us,
                                                 target_data_us,
                                                 interactive = FALSE), "ggplot")
  ## Plotly
  expect_s3_class(plot_test <- plot_step_ahead_model_output(projection_data_a_us,
                                                            target_data_us,
                                                            interactive = TRUE),
                  "plotly")

  # Invisible
  expect_invisible(plot_step_ahead_model_output(projection_data, target_data_us,
                                                show_plot = FALSE))

  # Layout (static)
  ## Static plot with facets, median as line, and title
  plot_test <-
    plot_step_ahead_model_output(projection_data_a_us, target_data_us,
                                 interactive = FALSE, title = "My Plot",
                                 facet = "scenario_id",
                                 use_median_as_point = TRUE)
  expect_equal(plot_test$labels$title, "My Plot")
  expect_equal(names(plot_test$facet$params$facets), "scenario_id")
  expect_equal(unique(tail(plot_test$layers, 1)[[1]]$data$output_type_id), 0.5)
  ## Static plot with target data on top and free x axis (only)
  plot_test <-
    plot_step_ahead_model_output(projection_data, target_data_us,
                                 interactive = FALSE, facet = "scenario_id",
                                 top_layer = "target", facet_scales = "free_x")
  expect_in(names(tail(plot_test$layers, 1)[[1]]$data),
            c("date", "location", "observation", "target"))
  expect_true(plot_test$facet$params$free$x)
  expect_false(plot_test$facet$params$free$y)
  ## Static plot with 2 models and 1 Ensemble specific format, and no legend.
  ## Traces grouped by forecast date
  plot_test <-
    plot_step_ahead_model_output(dplyr::filter(static_proj,
                                               model_id != "hubcomp_examp"),
                                 target_data_us,
                                 use_median_as_point = TRUE,
                                 ens_color = "black", interactive = FALSE,
                                 ens_name = "hub-ensemble", show_legend = FALSE,
                                 group = "forecast_date")
  expect_equal(plot_test$guides$guides$fill, "none")
  expect_equal(plot_test$guides$guides$colour, "none")
  expect_equal(tail(plot_test$layers, 1)[[1]]$geom$default_aes$colour, "black")

  # Layout (interactive)
  ## Interactive plot with traces grouped by forecast date
  plot_test <-
    plot_step_ahead_model_output(static_proj, target_data_us,
                                 group = "forecast_date")
  attr_test <- purrr::map(c(3:length(plot_test$x$visdat)),
                          function(x) attributes(plot_test$x$visdat[[x]]()))
  attr_test <- purrr::map(attr_test, "groups")
  expect_false(any(purrr::map_lgl(attr_test, is.null)))
  expect_true(length(unique(attr_test)) == 1)
  expect_equal(unique(attr_test)[[1]]$forecast_date,
               c("2021-03-13", "2021-04-10"))

  ## Interactive plot with Ensemble specific format, median, limited number of
  ## model input and traces grouped by forecast date
  plot_test <-
    plot_step_ahead_model_output(dplyr::filter(static_proj,
                                               model_id != "hubcomp_examp"),
                                 target_data_us,
                                 ens_color = "black", ens_name = "hub-ensemble",
                                 use_median_as_point = TRUE,
                                 group = "forecast_date")
  attr_test <- purrr::map(c(3:length(plot_test$x$visdat)),
                          function(x) attributes(plot_test$x$visdat[[x]]()))
  attr_test <- purrr::map(attr_test, "groups")
  expect_false(any(purrr::map_lgl(attr_test, is.null)))
  expect_true(length(unique(attr_test)) == 1)
  expect_equal(unique(attr_test)[[1]]$forecast_date,
               c("2021-03-13", "2021-04-10"))

  ## Interactive plot with Ensemble specific format, facets, no ribbons, free
  ## x axis and a specific palette
  plot_test <-
    plot_step_ahead_model_output(proj_data_q, target_data_us,
                                 facet = "scenario_id", facet_scales = "free_x",
                                 use_median_as_point = TRUE, intervals = NULL,
                                 plot_target = FALSE, ens_color = "black",
                                 ens_name = "hub-ensemble", pal_color = "Set1")
  expect_length(plot_test$x$data,
                length(unique(proj_data_q$model_id)) *
                  length(unique(proj_data_q$scenario_id)))
  test_value <-
    purrr::map(plot_test$x$data[as.character(unlist(purrr::map(plot_test$x$data,
                                                               "name"))) ==
                                  "hub-ensemble"], "line")
  expect_equal(unique(unlist(test_value)["color"]), "black")
  expect_true(plot_test$x$subplot)
  expect_gt(length(unique(purrr::map(plot_test$x$data, "xaxis"))), 1)
  expect_equal(length(unique(purrr::map(plot_test$x$data, "yaxis"))), 1)
  expect_equal(unique(unlist(purrr::map(plot_test$x$attrs, "colors"))), "Set1")
  ## Interactive plot with facets (only target data available for facet A),
  ## median line, free x and y axis, a specific palette, and no legend
  plot_test <-
    plot_step_ahead_model_output(proj_data_q,
                                 dplyr::mutate(target_data_us,
                                               scenario_id = "A-2021-03-05"),
                                 facet = "scenario_id", plot_target = TRUE,
                                 facet_scales = "free", show_legend = FALSE,
                                 use_median_as_point = TRUE)
  expect_false(plot_test$x$layoutAttrs[[1]]$showlegend)
  expect_gt(length(unique(purrr::map(plot_test$x$data, "yaxis"))), 1)
  expect_gt(length(unique(purrr::map(plot_test$x$data, "xaxis"))), 1)
  test_value <-
    purrr::map(plot_test$x$data[purrr::map(plot_test$x$data,
                                           "name") == "target"], "x")
  expect_equal(length(purrr::discard(test_value, is.null)), 1)
  ## Interactive plot with facets, target data on top, color by scenario,
  ## sub title on the bottom right
  plot_test <-
    plot_step_ahead_model_output(proj_data_q, target_data_us,
                                 top_layer = "target", facet = "model_id",
                                 fill_by = "scenario_id",
                                 facet_title = "bottom right")
  expect_equal(tail(plot_test$x$data, 1)[[1]]$name, "target")
  expect_equal(unique(unlist(purrr::map(plot_test$x$layout$annotations,
                                        "xanchor"))), "right")
  expect_equal(unique(unlist(purrr::map(plot_test$x$layout$annotations,
                                        "yanchor"))), "bottom")
  expect_in(unlist(purrr::map(plot_test$x$layout$annotations, "text")),
            unique(proj_data_q$model_id))
  expect_in(unique(unlist(purrr::map(purrr::map(plot_test$x$attrs, "color"),
                                     levels))),
            unique(proj_data_q$scenario_id))
  ## Interactive plot with facets (only target data available for facet A),
  ## median line, tile, free y axis, limit number of row, and a specific
  ## opacity
  plot_test <-
    plot_step_ahead_model_output(proj_data_q, target_data_us,
                                 facet = "model_id", title = "My Plot",
                                 facet_scales = "free_y", facet_nrow = 3,
                                 fill_transparency = 0.75,
                                 use_median_as_point = TRUE)
  expect_gt(length(unique(purrr::map(plot_test$x$data, "yaxis"))), 1)
  expect_equal(length(unique(purrr::map(plot_test$x$data, "xaxis"))), 1)
  expect_equal(plot_test$x$layoutAttrs[[2]]$title, "My Plot")
  expect_in(unlist(purrr::map(plot_test$x$layout$annotations, "text")),
            unique(proj_data_q$model_id))
  expect_in(unique(unlist(purrr::map(purrr::map(plot_test$x$attrs, "color"),
                                     levels))),
            unique(proj_data_q$model_id))
  expect_equal(tail(plot_test$x$data, 1)[[1]]$opacity, 0.75)
  ## Interactive plot with facets, no ribbons, ensemble specific format,
  ## median line, a specific one color.
  plot_test <-
    plot_step_ahead_model_output(proj_data_q, target_data_us,
                                 pal_color = NULL, one_color = "orange",
                                 ens_color = "black", ens_name = "hub-ensemble",
                                 intervals = NULL, use_median_as_point = TRUE,
                                 facet = "scenario_id")
  test_value <-
    purrr::map(plot_test$x$data[purrr::map(purrr::map(plot_test$x$data,
                                           "name"), levels) %in%
                                  "hub-ensemble"], "line")
  expect_equal(unique(unlist(test_value)["color"]), "black")
  test_value <-
    purrr::map(plot_test$x$data[purrr::map(purrr::map(plot_test$x$data,"name"),
                                           as.character) == "HUBuni-simexamp"],
               "line")
  test_value <- unique(unlist(test_value)["color"])
  expect_equal(test_value, "rgba(255,165,0,1)")
  ## Interactive plot with facets, no median line, ensemble specific format,
  ## a specific ribbon interval (information in the hover text)
  plot_test <- plot_step_ahead_model_output(projection_data_a_us,
                                            target_data_us,
                                            intervals = 0.9,
                                            ens_color = "black",
                                            ens_name = "hub-ensemble",
                                            facet = "model_id")
  expect_equal(strsplit(strsplit(tail(purrr::map(plot_test$x$attrs,
                                                 "hovertext"), 1)[[1]],
                                 "<br>")[[1]][2], "Intervals")[[1]][1], "90% ")

  ## Interactive plot with 3 model, 1 model did not submit one scenario
  test_data <- dplyr::filter(projection_data,
                             !(model_id == "HUBuni-simexamp" &
                                 scenario_id == "A-2021-03-05"))
  plot_test <-
    plot_step_ahead_model_output(test_data,
                                 target_data_us,
                                 use_median_as_point = TRUE,
                                 facet = "scenario_id")
  leg_sel <- !unlist(purrr::map(purrr::map(plot_test$x$data, "showlegend"),
                                isFALSE))
  legend <- unique(unlist(purrr::map(purrr::map(plot_test$x$data,
                                                "name")[leg_sel],
                                     as.character)))
  expect_equal(legend,
               c("target" ,"hub-ensemble", "hubcomp_examp", "HUBuni-simexamp"))

  test_data <- dplyr::filter(projection_data,
                             !(model_id == "HUBuni-simexamp" &
                                 scenario_id %in% c("A-2021-03-05",
                                                    "B-2021-03-05")),
                             !(model_id == "hubcomp_examp" &
                                 scenario_id %in% c("A-2021-03-05",
                                                    "C-2021-03-05",
                                                    "D-2021-03-05")))
  plot_test <-
    plot_step_ahead_model_output(test_data,
                                 target_data_us,
                                 use_median_as_point = TRUE,
                                 facet = "scenario_id")
  leg_sel <- !unlist(purrr::map(purrr::map(plot_test$x$data, "showlegend"),
                                isFALSE))
  legend <- unique(unlist(purrr::map(purrr::map(plot_test$x$data,
                                                "name")[leg_sel],
                                     as.character)))
  expect_equal(legend,
               c("target" ,"hub-ensemble", "hubcomp_examp", "HUBuni-simexamp"))

})

test_that("ggplot output file", {
  # Layout (static)
  plot_test <-
    plot_step_ahead_model_output(projection_data_a_us, target_data_us,
                                 interactive = FALSE)
  vdiffr::expect_doppelganger("Basis Static", plot_test)

  plot_test <-
    plot_step_ahead_model_output(projection_data_a_us, target_data_us,
                                 interactive = FALSE, title = "My Plot",
                                 facet = "scenario_id",
                                 use_median_as_point = TRUE)
  vdiffr::expect_doppelganger("Basis Static Median", plot_test)

  plot_test <-
    plot_step_ahead_model_output(projection_data, target_data_us,
                                 interactive = FALSE, facet = "scenario_id",
                                 top_layer = "target", facet_scales = "free_x")
  vdiffr::expect_doppelganger("Facet Static", plot_test)

  plot_test <-
    plot_step_ahead_model_output(static_proj,
                                 target_data_us, interactive = FALSE,
                                 ens_color = "black", ens_name = "hub-ensemble",
                                 use_median_as_point = TRUE,
                                 show_legend = FALSE, group = "forecast_date")
  vdiffr::expect_doppelganger("Ensemble Group Static", plot_test)

  plot_test <- plot_step_ahead_model_output(projection_data_a_us,
                                            target_data_us,
                                            intervals = 0.9,
                                            ens_color = "black",
                                            ens_name = "hub-ensemble",
                                            facet = "model_id",
                                            interactive = FALSE)
  vdiffr::expect_doppelganger("Ensemble Model Facet Static", plot_test)


  ## Static plot with 3 model, 1 model did not submit one scenario
  test_data <- dplyr::filter(projection_data,
                             !(model_id == "HUBuni-simexamp" &
                                 scenario_id == "A-2021-03-05"))
  plot_test <-
    plot_step_ahead_model_output(test_data,
                                 target_data_us,
                                 use_median_as_point = TRUE,
                                 interactive = FALSE, facet = "scenario_id")
  vdiffr::expect_doppelganger("Missing Value Facet Static", plot_test)

})
