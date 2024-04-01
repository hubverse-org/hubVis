
make_palette <- function(model_output_data, fill_by = "model_id",
                         pal_color = "Set2", one_color = "blue",
                         ens_color = NULL, ens_name = NULL,
                         plot_target = TRUE) {
  fill_by_vect <- unique(model_output_data[[fill_by]])
  if (!is.null(pal_color)) {
    if (!pal_color %in% row.names(RColorBrewer::brewer.pal.info)) {
      cli::cli_warn(c("!" = "{.arg pal_color} is not one of the accepted palette
                       name, accepted values are:
                      {.val {row.names(RColorBrewer::brewer.pal.info)}}.
                      {.val Set2} used by default."))
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
      cli::cli_warn(c("!" = "{.arg one_color} is not one of the accepted color
                       name, accepted values are: {.val {colors()}}.
                      {.val blue} used by default."))
      one_color <- "blue"
    }
    pal_color <- one_color
    pal_value <- rep(pal_color, length(fill_by_vect))
  }
  names(pal_value) <- fill_by_vect
  if (!is.null(ens_color) && !is.null(ens_name))
    pal_value[ens_name] <- grDevices::rgb(grDevices::col2rgb(ens_color)[1],
                                          grDevices::col2rgb(ens_color)[2],
                                          grDevices::col2rgb(ens_color)[3])
  if (plot_target) {
    pal_value <- c(pal_value, "Target Data" = "#6e6e6e")
  }
  return(list("value" = pal_value, "color" = pal_color))
}
