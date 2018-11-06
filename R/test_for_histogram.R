#' analyse a ggplot object to understand if it is an histogram
#' @param plot_object the ggplot object being analysed
#' @param n_of_layers number of layers within the ggplot object
#' @description this function checks two conditions to understand if the provided ggplot object represent an histogram:
#' - presence of a geom_bar layer
#' - presence of bindwidth parameter among parameters of that layer
#' @return TRUE in case the plot is an histogram, FALSE in the opposite case
#' @export
test_for_histogram <- function(plot_object,n_of_layers){
  bar_index <- match("GeomBar", geoms_lister(plot_object,n_of_layers))
  bar_stat_params <- if(!is.na(bar_index )){plot_object$layers[[bar_index]]$stat_params %>% names()}
  test_histogram <- !is.na(bar_index) & !is.na(match("binwidth",bar_stat_params))
  return(test_histogram)
}
