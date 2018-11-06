#' a simple function to test for the presence of too many dimension for a 2D plotting space
#' @param plot_object the ggplot object being analysed
#' @param n_of_layers number of layers within the ggplot object
#' @description this function checks for the presence of both point or line and the mapping of some variable on the \code{size} aestetich.
#' This check is based on data visualization principle requesting that the number of dimension depicted should not be higher thant the number of dimension of the data.
#' @references The Visual Display of Quantitative Information, E.R.Tufte, GRaphics Press . Cheshire, Connecticut
#' @return TRUE if the plot contains too many dimension, FALSE in the opposite case.
#' @export
too_many_dimensions <- function(plot_object,n_of_layers){
# we check here if there is a combination of geom_point/ geom_line and mapping = size

not_point_or_line <- match(c("GeomLine", "GeomPoint"),geoms_lister(plot_object,n_of_layers)) %>%
  sum(na.rm = TRUE) ==0
not_mapped_on_size <- match("size",
                        mappings_lister(plot_object,n_of_layers)$aes ) %>% is.na()
return(!not_point_or_line & !not_mapped_on_size)

}
