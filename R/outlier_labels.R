#' check for the presence of outliers not labelled within a ggplot object
#' @param plot_object the ggplot object being analysed
#' @param n_of_layers number of layers within the ggplot object
#' @param p_build an object obtained running \code{ggplot_build} on the ggplot object
#' @param aes_db a data frame storing plot object aestetichs and variable mapped against them
#' @description for x and y variable the Tukey outlier rule is applied to evaluate the presence of outliers.
#' If any outlier is identified one more check is performed to retrieve among plot metadata the presence of some annotation related to that data.
#' @return a list storing a TRUE in case there are no outliers or the detected outliers are labelled and FALSE in the other cases.
#' @export
outlier_labels <- function(plot_object,n_of_layers,p_build,aes_db){
  #TRUE as final result means either you don't have outliers or the ones you have are labeled. the relevant result here is
  # FALSE since it discriminates between a good and a bad plot.

  #draw_data
  raw_x <- aes_puller(plot_object,n_of_layers, "x")
  if(mode(raw_x) == "list"){x_vector <- raw_x[,1] %>% pull}else{x_vector <- as.numeric(raw_x)}
  if(!is.na(match("..density..",aes_db$variable) )){y_vector <- c(NA)}else{
  raw_y <- aes_puller(plot_object,n_of_layers, "y")
  if(mode(raw_y) == "list"){y_vector <- raw_y[,1] %>% pull}else{y_vector <- as.numeric(raw_y)}
  }
############ x
if(is.na(x_vector)|is.character(x_vector)){x_outliers_are_labelled <- NA}else{
  # look for outliers
  x_outliers <- boxplot.stats(x_vector )$out


  if (length((x_outliers))>0){ #we preliminary check for outliers presence, to avoid unnecessary computations
    # look for geom_text
    text_index <- match("GeomText",geoms_lister(plot_object, n_of_layers ))
    # if text is found I look at data to see if we are looking the outliers are labeled
    if(is.na(text_index)){x_outliers_are_labelled <- FALSE}else if(text_index > 0){

      text_x <- p_build$data[[text_index]]$x


      x_outliers_are_labelled <- if(length(x_outliers)==0){TRUE}else if(x_outliers%in% text_x){TRUE}else{FALSE}

      }}else{x_outliers_are_labelled <-  TRUE}}

########### y

  if(is.na(y_vector)|is.character(x_vector)){y_outliers_are_labelled <- NA}else{

  # look for outliers

  y_outliers <- boxplot.stats(y_vector)$out

  if (length(y_outliers)>0){ #we preliminary check for outliers presence, to avoid unnecessary computations
  # look for geom_text
    text_index <- match("GeomText",geoms_lister(plot_object, n_of_layers ))
    # if text is found we look at data to see if in the plot we are looking at the outliers are labeled
   if(is.na(text_index)){y_outliers_are_labelled <- FALSE}else if(text_index > 0){

     text_y <- p_build$data[[text_index]]$y

     y_outliers_are_labelled <- if(length(y_outliers)==0){TRUE}else if(y_outliers%in% text_y){TRUE}else{FALSE}

   }}else{y_outliers_are_labelled <-  TRUE}}
###############
     general_result <- if(is.na(x_outliers_are_labelled) & is.na(y_outliers_are_labelled)){
       TRUE} else{if(prod(x_outliers_are_labelled,y_outliers_are_labelled, na.rm = TRUE)== TRUE){TRUE}else{FALSE}}

     return(general_result)
}
