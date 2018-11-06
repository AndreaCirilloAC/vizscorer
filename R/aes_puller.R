#' retrieve a vector storing values of a specified aestetich
#' @param plot_object the ggplot object being analysed
#' @param n_of_layers number of layers within the ggplot object
#' @param required_aes the ggplot object aestetich required
#' @return a vector containing values of the variable the required specified aestetich is mapped to
aes_puller <- function(plot_object,n_of_layers, required_aes){

  aes_db     <- mappings_lister(plot_object , n_of_layers )
  variable_name   <- aes_db %>% filter(aes == required_aes) %>% select("variable") %>% pull()
  col_index <- match(variable_name,colnames(plot_object$data))
  if(length(variable_name) == 0 ){return(NA)}else{
  variable_vector <- plot_object$data[,col_index]

  return(variable_vector)}
}
