#' Create palette associated with the data
#'
#' The function creates a palette with a color associated with each trace.
#'
#'@param model_out_tbl a `model_out_tbl` object, containing all the required
#' columns including a column containing date information (`x_col_name`
#' parameter) and a column `value`.
#'@param fill_by name of a column for specifying colors and legend in plot.
#' The `pal_color` parameter can be use to change the palette.
#' Default to `model_id`.
#'@param pal_color a `character` string for specifying the palette color in the
#' plot. Please refer to [RColorBrewer::display.brewer.all()]. If `NULL`,
#' only `one_color` parameter will be used for all models. Default to `"Set2"`
#'@param one_color a `character` string for specifying the color in the
#' plot if `pal_color` is set to `NULL`. Please refer
#' to [colors()] for accepted color names. Default to `"blue"`
#'@param ens_color a `character` string of a color name, if not NULL, will be
#' use as color for the model name associated with the parameter `ens_name`
#' (both parameter need to be provided)
#'@param ens_name a `character` string of a model name, if not NULL, will be
#' use to change the color for the model name, associated with the parameter
#' `ens_color`(both parameter need to be provided)
#'@param plot_target a `boolean` for showing the target data in the plot.
#'  Default to TRUE. Data used in the plot comes from the parameter
#'  `target_data`
#'
#'@noRd
make_palette <- function(
  model_out_tbl,
  fill_by = "model_id",
  pal_color = "Set2",
  one_color = "blue",
  ens_color = NULL,
  ens_name = NULL,
  plot_target = TRUE
) {
  fill_by_vect <- unique(model_out_tbl[[fill_by]])
  if (!is.null(pal_color)) {
    if (!pal_color %in% row.names(RColorBrewer::brewer.pal.info)) {
      cli::cli_warn(c(
        "!" = "{.arg pal_color} is not one of the accepted palette
                       name, accepted values are:
                      {.val {row.names(RColorBrewer::brewer.pal.info)}}.
                      {.val Set2} used by default."
      ))
      pal_color <- "Set2"
    }
    if (length(fill_by_vect) < 3) {
      n_pal <- 3
    } else {
      n_pal <- length(fill_by_vect)
    }
    pal_value <- RColorBrewer::brewer.pal(n_pal, pal_color)
  } else {
    if (!one_color %in% colors()) {
      cli::cli_warn(c(
        "!" = "{.arg one_color} is not one of the accepted color
                       name, accepted values are: {.val {colors()}}.
                      {.val blue} used by default."
      ))
      one_color <- "blue"
    }
    pal_color <- one_color
    pal_value <- rep(pal_color, length(fill_by_vect))
  }
  names(pal_value) <- fill_by_vect
  if (!is.null(ens_color) && !is.null(ens_name)) {
    pal_value[ens_name] <- grDevices::rgb(
      grDevices::col2rgb(ens_color)[1],
      grDevices::col2rgb(ens_color)[2],
      grDevices::col2rgb(ens_color)[3],
      maxColorValue = 255
    )
  }
  if (plot_target) {
    pal_value <- c(pal_value, "Target Data" = "#6e6e6e")
  }
  list("value" = pal_value, "color" = pal_color)
}
