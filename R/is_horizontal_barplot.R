#' a simple function to test for the presence of too many dimension for a 2D plotting space
#' @param plot_object the ggplot object being analysed
#' @param n_of_layers number of layers within the ggplot object
#' @return TRUE in case the plot object is a bar plot and it is horizontally layed, FALSE in the opposite case.

is_horizontal_barplot <- function(plot_object,n_of_layers){
  is_bar_plot <- test_for_geom_bar(plot_object,n_of_layers)
  is_flipped <- !is.na(match("CoordFlip" ,coordinates_lister(plot_object)))
  if(!is_bar_plot){return(FALSE)}else{
  return(is_bar_plot & is_flipped)}
}
