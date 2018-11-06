#' look for special charaters in a plot labels
#' @param plot_object the ggplot object being analysed
#' @description this function match the labels of the given ggplot object against a set of predefined special characters
#' @return a list storing test result and the list of special characters found within labels.
#' @export
labels_reader <- function(plot_object){
  labels_text_vector <- plot_object$labels %>% unlist() %>% as.vector()
  special_character_vector <- c("&","/","!","?","^","@","#","$","¶","§", ":)", ".")

  match_vector <- pmatch(special_character_vector,labels_text_vector)

  special_characters_indexes <- match_vector[!is.na(match_vector)]

  there_are_special_characters <- if(length(special_characters_indexes) == 0){FALSE}else{TRUE}

  labels_with_special_characters <- labels_text_vector[special_characters_indexes]
  return(list(there_are_special_characters, labels_with_special_characters))
}
