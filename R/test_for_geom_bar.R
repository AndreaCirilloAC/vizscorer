#' look for geom_bar layer on a plot
#' @param plot_object the ggplot object being analysed
#' @param n_of_layers number of layers within the ggplot object
#' @return TRUE if a geom_bar is found within ggplot object layers, FALSE in the opposite case.

test_for_geom_bar <- function(plot_object,n_of_layers){
  !is.na(match("GeomBar",geoms_lister(plot_object,n_of_layers)))
}
