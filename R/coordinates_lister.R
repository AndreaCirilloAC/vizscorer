#' list coordinates layers in a ggplot object
#' @param plot_object the ggplot object being analysed
#' @return a vector storing all coordinates found within layers of a ggplot object
#' @export
coordinates_lister <- function(plot_object){
  plot_object$coordinates %>% class() %>% as.vector()
}
