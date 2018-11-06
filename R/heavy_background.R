#'look for heavy background on a ggplot plot
#' @param plot_object the ggplot object being analysed
#' @description this function tests a ggplot object to understand if it represents a plot with a background neither transparent nor white.
#' Based on data visualization literature this kind of plots tend to reduce the **data to ink ratio** meaning the amount of data showed related to amount of ink employed.
#' Lower values of this ratio are related to non-effective plots.
#' @references The Visual Display of Quantitative Information, E.R.Tufte, GRaphics Press . Cheshire, Connecticut
#' @return returns TRUE if the plot is depicted on an heavy background, FALSE if the background is white or transparent
heavy_background <- function(plot_object){
  # we check here if the default background is being used AND
  # if the default background is still the grey one

  default_theme <- theme_get()
  default_is_grey <- pmatch("grey",default_theme$panel.background$fill)
  background_fill <- plot_object$theme$panel.background$fill
  if ((length(as.character(plot_object$theme)) == 0 | is.null(background_fill))  & !is.na(default_is_grey)) {
    TRUE # the default theme is being used and this produces to a grey background
  }else{
    if (background_fill %in% c(rgb(1,1,1),"#FFFFFF", "white", "transparent")) { # if the background is not white or transparent we say it is heavy, tertium non datur
      FALSE}else{
        TRUE}
  }
}
