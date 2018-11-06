#' study the relationship among variables to find out if a geom_smooth is needed within the plot
#' @param plot_object the ggplot object being analysed
#' @param n_of_layers number of layers within the ggplot object
#' @param correlation_threshold a judgmentally specified threshold to test for the need of a trend line on the plot
#' @param aes_db a data frame storing plot object aestetichs and variable mapped against them
#' @description \code{does_it_need_geom_smooth} computes distance correlation among x and y aestetich to evaluate it against a judgmental threshold.
#' if this threshold is breached and no \code{geom_smooth} is observed among ggplot objects layers a postive outcome is produced for the test.
#' @return a list storing in two separate vectors the final result of the test and the observed distance correlation.
#' the test outcome is TRUE in case a trend line is deemed necessary.
#' @export
does_it_need_geom_smooth <- function(plot_object, n_of_layers,correlation_threshold,aes_db){

  # first we check for correlations ( of any type) among points
  raw_x <- aes_puller(plot_object,n_of_layers, "x")
  if(mode(raw_x) == "list"){x_vector <- raw_x[,1] %>% pull}else{x_vector <- raw_x}

  #look for density plot currently not handled
  if(!is.na(match("..density..",aes_db$variable))){y_vector <- c()}else{
  raw_y <- aes_puller(plot_object,n_of_layers, "y")
  if(mode(raw_y) == "list"){y_vector <- raw_y[,1] %>% pull}else{y_vector <- raw_y}
  }
  not_handled <- c("factor","character")
  #check on variabe type
  if(class(x_vector) %in% not_handled | class(y_vector) %in% not_handled | !is.na(match("..density..",aes_db$variable))){return(list(NA,0,NA))
    } else if(!is.na(x_vector) & !is.na(y_vector)){
    distance_correlation <- dcor(x_vector,y_vector)
  }else{distance_correlation <- 2}

  # if the level of correlation is relevant we look for the presence of geom_smooth
  if(abs(distance_correlation)>correlation_threshold & distance_correlation != 2){
    geoms_vector <- geoms_lister(plot_object, n_of_layers)
    if(!is.na(match("GeomSmooth",geoms_vector))){return(list(FALSE,distance_correlation))}else{
      return(list(TRUE,distance_correlation)) # if we have a relevant correlation and no geom smooth we suggest to add one
    }

  }else if(distance_correlation == 2){return(list(NA,0))}
  else{
    return(list(FALSE,distance_correlation))
  }
}
