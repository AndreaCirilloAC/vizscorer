#' test a ggplot object to understand if it represents a barplot with filled bars
#' @param plot_object the ggplot object being analysed
#' @param n_of_layers number of layers within the ggplot object
#' @description this function tests a ggplot object to understand if it represents a barplot with filled bars.
#' Based on data visualization literature this kind of plots tend to reduce the **data to ink ratio** meaning the amount of data showed related to amount of ink employed.
#' Lower values of this ratio are related to non-effective plots.
#' A control is performed to exclude we are simply looking at a pie chart based on an intermediate \code{geom_bar} layer
#' @references The Visual Display of Quantitative Information, E.R.Tufte, GRaphics Press . Cheshire, Connecticut
#' @return returns TRUE when the barplot is composed of filled bars and FALSE in the opposite case.
#' @export
filled_barplot <- function(plot_object,n_of_layers){

is_geom_bar <- test_for_geom_bar(plot_object,n_of_layers)

mappings_lister(plot_object,n_of_layers) %>%
    select(aes) %>%
    pull() %>%
    as.vector()-> aes_vector

fill_aes_is_mapped <- !is.na(match("fill",aes_vector))

a_pie_chart <- is_pie_chart(plot_object, n_of_layers)

if(is_geom_bar &
   fill_aes_is_mapped == FALSE &
   a_pie_chart==FALSE){ # we control for being a pie chart
  TRUE
}else{if(is_geom_bar == FALSE){FALSE}#NA}
  else{FALSE}}
}
