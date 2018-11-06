#' compare the actual bins width against the Freedman Diaconis rule
#' @param plot_object the ggplot object being analysed
#' @param n_of_layers number of layers within the ggplot object
#' @param default_n_of_bins the default number of bins set by ggplot2. employed if no custom bin was set from the user within the ggplot object
#' @description this function computes the optimal bins width based on the Freedman Diaconis rule and compares it against the actual bin width set within the ggplot object.
#' @return an object storing the general test result, the optimal binwidth based on Freedman Diaconis and the distance of the actual binwidth from this measure.
#' TRUE is produced as a result if the actual width is not optimal.
#' @export
histogram_bins_tester <- function(plot_object, n_of_layers, default_n_of_bins){
# first of all I look for a GeomBar layer and a binwidth parameter. the occurence of
  # both means we are "looking" at an histogram ( there is no separate geom)
 test_histogram <- test_for_histogram(plot_object,n_of_layers)
# if the plot actually seems to be an histogram I retrieve di x variable and compute on it
  ## the optimal number of bins based on the Freedman Diaconis rule

if(test_histogram){
variable_vector <- aes_puller(plot_object,n_of_layers,required_aes = "x")
optimal_bw       <-  2 * (IQR(variable_vector, na.rm = TRUE) / length(variable_vector)^(1/3))
bar_index <- match("GeomBar",geoms_lister(plot_object,n_of_layers))
bar_stat_params <- plot_object[[bar_index]]$stat_params
actual_bw_index  <- match("binwidth",bar_stat_params)
actual_bw          <- plot_object$layers[[bar_index]]$stat_params[[actual_bw_index]]

if (is.null(actual_bw)){
  actual_bw <- diff(range(variable_vector, na.rm = TRUE))/default_n_of_bins
  } # we handle here the common case of the user not changing the default setting for bins size

# after retrieving or computing the actual binwidth I compute the distance from the optimum

distance_from_optimum <- optimal_bw - actual_bw
if(distance_from_optimum !=0){
optimization_data <- list("test" = TRUE,"optimal_bw" = optimal_bw, "distance_from_optimum" = distance_from_optimum)
return(optimization_data)
}else{return(list("test"=FALSE,"optimal_bw" =optimal_bw,"distance_from_optimum"=distance_from_optimum))}
  }else{list(FALSE,NA,100)}

}
