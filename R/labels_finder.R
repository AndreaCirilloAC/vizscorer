#' look for a specific label within a ggplot object
#' @param plot_object the ggplot object being analysed
#' @param searched_label, among title, subtitle and caption
#' @return TRUE if the searched label was found, FALSE in the opposite case
labels_finder <- function(plot_object,searched_label){
  labels_vector <- plot_object$labels %>% names() %>% as.vector()
  label_found <- if(is.na(match(searched_label,labels_vector))){FALSE}else{TRUE}

  }
