#' checking for the presence of overplotting within a ggplot object.
#' @param plot_object the ggplot object being analysed
#' @param n_of_layers number of layers within the ggplot object
#' @param overplotting_floor a custom threshold of distance among points to detect overplotting
#' @param aes_db a data frame storing plot object aestetichs and variable mapped against them
#' @return a list containing an overall test result with TRUE in presence of overplotting and the median distance observed between points
#' @export
cozy_plot <- function(plot_object, n_of_layers, overplotting_floor,aes_db){

if (is.na(match("y",aes_db$aes))){ #if there is no y we are probably "looking" at an histogram, for which no overplotting check is provided
  return(list(NA,100))}else{
  # we look here for overplotting. To do this we compute the median euclidean distance as a measure
    # of plot density
    raw_x <- aes_puller(plot_object,n_of_layers, "x")
    if(mode(raw_x) == "list"){x_vector <- raw_x[,1] %>% pull}else{x_vector <- raw_x}
    if(!is.na(match("..density..",aes_db$variable) )){y_vector <- c()}else{
    raw_y <- aes_puller(plot_object,n_of_layers, "y")
    if(mode(raw_y) == "list"){y_vector <- raw_y[,1] %>% pull}else{y_vector <- raw_y}
    }
    not_handled <- c("factor","character","Date")
    #check on variabe type
    if(class(x_vector) %in% not_handled | class(y_vector) %in% not_handled|!is.na(match("..density..",aes_db$variable)) ){return(list(NA,100))}else{

  median_distance <- median(daisy(data.frame(x_vector,y_vector)))

  test_result <- median_distance < overplotting_floor

  return(list("test_result" = test_result,
              "median_distance" = median_distance))
    }
  }

}
