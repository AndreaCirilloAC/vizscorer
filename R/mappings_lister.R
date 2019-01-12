#' storing aestetich variables in a dataframe
#' @param plot_object the ggplot object being analysed
#' @param n_of_layers number of layers within the ggplot object
#' @return a data frane storing for each aestetich the mapped variable
#' @export
mappings_lister <- function(plot_object,n_of_layers){

  #aes_db_raw just contains aes from within the ggplot() call, i.e. from the "zero" layer
  aes_db_raw <- data.frame("aes" = plot_object$mapping %>% unlist() %>% names(),
                                     "variable" = plot_object$mapping %>% unlist() %>% as.character(),stringsAsFactors = FALSE )

  #we then add aes from other layers
  for(i in 1:n_of_layers){
    aes_names_partial <- plot_object$layers[[i]]$mapping %>% unlist() %>% names()
    aes_values_partial_raw <- plot_object$layers[[i]]$mapping %>% unlist() %>% as.character()
    #removing ~ character to ensure proper matching with variable names
    aes_values_partial <- gsub("~","",x = aes_values_partial_raw)
    rbind(aes_db_raw,
          data.frame("aes" = aes_names_partial,
                     "variable" = aes_values_partial,
                     stringsAsFactors = FALSE)) %>%
      filter(variable != "x", variable != "y") -> aes_db_intermediate # we check for aestetichs inheritance
    }
  #in the end I add the mapping set into ggplot()
  aes_db_intermediate %>%
    mutate(variable = gsub("~","",variable)) -> aes_db
 return(aes_db)

}
