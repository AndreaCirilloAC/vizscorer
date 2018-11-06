#' look for overshooting in case of a plot for with too few data
#' @param plot_object the ggplot object being analysed
#' @param data_threshold a custom threshold to define too many data
#' @description this function checks for the presence of too few data to deserve a plot. Based on data visualization literature it is assumed that twenty data points can be considered a significant threshold
#' @return TRUE in case too few data points are observed, FALSE in the opposite site.
#' @export
too_few_data <- function(plot_object,data_threshold) {
  if (plot_object$data %>% nrow() < data_threshold) { FALSE}else{TRUE}
}
