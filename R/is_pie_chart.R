#' test a ggplot object to understand if it is a pie chart
#' @param plot_object the ggplot object being analysed
#' @param n_of_layers number of layers within the ggplot object
#' @description to understand if aplot is a pie chart this functions look for the presence of a \code{geom_bar} layer and a \code{coord_polar} layer.
#' @return TRUE if the plot is a pie chart and FALSE in the opposite case
#' @export
is_pie_chart <- function(plot_object, n_of_layers){

  coordinates_class <-   coordinates_lister(plot_object)

  # if we do not found any geom_bar we can be sure it is not a pie chart

test_for_geom_bar(plot_object,n_of_layers)& !is.na(match("CoordPolar",coordinates_class))

  }

