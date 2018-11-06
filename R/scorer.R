#'scoring the effectivenes of ggplot object applying a gradient boosting model
#'@description \code{scorer} function handles the running of a gradient boosting pre-trained model on the metadata of a ggplot object.
#'As a result a 0 to 1 score is produced, measuring the effectiveness of the related plot based on a set of rules drawn from data visualization theory.
#'@param plot_metadata a data frame storing ggplot object metadata, resulting from a previous call of \code{metadata_reader} function.
#'@return a numeric value
#' @export
scorer <- function(plot_metadata = NULL){

  if(is.null(plot_metadata)){stop("you must provide a valid set of plot metadata to obtain a score")}else{

    # take as input a data frame with one row, coming from metadata_reader

    # predict plot score employing the gradient boosting fitted on the classification db

    scoring_db_raw <- data.frame()
    colnames_vector <- c(
                          'pie_chart',
                          'number_of_layers',
                          'number_of_dimensions',
                          'width_of_bins',
                          'flipped_barplot',
                          'need_for_a_smooth',
                          'sufficient_number_of_data',
                          'overplotting',
                          'use_of_heavy_background',
                          'filled_barplot',
                          'presence_of_title',
                          'presence_of_subtitle',
                          'presence_of_caption',
                          'special_characters_in_label',
                          'outliers_not_labelled')

    plot_metadata <- plot_metadata %>% as.data.frame()

    result <- plot_metadata$test %>% as.character()
    row <- data.frame(t(c(result)))
    colnames(row) <- colnames_vector
    scoring_db_raw <- rbind(scoring_db_raw,row)
    colnames(scoring_db_raw) <- colnames_vector

    scoring_db_raw %>%
      mutate(pie_chart = as.logical(pie_chart),
             number_of_dimensions = as.logical(number_of_dimensions),
             number_of_layers = as.numeric(as.character(number_of_layers)),
             width_of_bins = as.numeric(as.character(width_of_bins)),
             flipped_barplot = as.logical(flipped_barplot),
             need_for_a_smooth = as.numeric(as.character(need_for_a_smooth)),
             sufficient_number_of_data = as.logical(sufficient_number_of_data),
             overplotting =as.numeric(as.character(overplotting)),
             use_of_heavy_background = as.logical(use_of_heavy_background),
             filled_barplot= as.logical(filled_barplot),
             presence_of_title = as.logical(presence_of_title),
             presence_of_subtitle = as.logical(presence_of_subtitle),
             presence_of_caption = as.logical(presence_of_caption),
             special_characters_in_label = as.logical(special_characters_in_label),
             outliers_not_labelled = as.logical(outliers_not_labelled))-> scoring_db

    predict(gbm_fit,newdata = scoring_db, probability = TRUE)
    probabilities <- extractProb(list(gbm = gbm_fit),unkX = scoring_db )
    #return as output the probability of being a good plot, i.e. the final score

    return(probabilities$good)

  }
}
