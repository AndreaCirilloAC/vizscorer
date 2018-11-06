#' draw a data frame containing relevant metadata of a ggplot object
#' @param plot_object the ggplot object being analysed
#' @description \code{metadata_reader} function performs a number of test on the provided ggplot object to draw some intelligence about it.
#' It tries to derive attributes ofthe plot relate to four data visualization theory areas:
#' - the adequateness of labelling
#' - the data density
#' - the data to ink ratio
#' - the readability of the plot
#' The results of performed tests are stored in a structued list object, so to be provided as an input to a gradient boosting model.
#' See \code{scorer} function documentation for more info about this subsequent step.
#' @return a list storing results and additional data related to each test performed.
metadata_reader <- function(plot_object = NULL){

  # preliminary chech on arguments of the function and others

  if(is.null(plot_object)){
    stop("you must provide a valid ggplot2 object", call = FALSE)
  }
  #blocking conditions:
  # data must e provided within ggplot() call
  if(is.null(plot_object$data[1])){
    stop("current version only checks on data provided within ggplot call")
  }
  n_of_layers <-  plot_object$layers %>% length()
  # no overplotting check for histograms ( and generally where no y is provided)
    if(test_for_histogram(plot_object,n_of_layers) == TRUE){
      message("no check for overplotting on histogram is currently provided")
    }
  #limitations
  aes_db <- mappings_lister(plot_object,n_of_layers)

  raw_x <- aes_puller(plot_object,n_of_layers, "x")

  if(mode(raw_x) == "list"){x_vector <- raw_x[,1] %>% pull}else{x_vector <- as.numeric(raw_x)}


  if(!is.na(match("..density..",aes_db$variable))){y_vector <- c()}else{
  raw_y <- aes_puller(plot_object,n_of_layers, "y")
  if(mode(raw_y) == "list"){y_vector <- raw_y[,1] %>% pull}else{y_vector <- as.numeric(raw_y)}
  }
  not_handled <- c("factor","character")
  available_and_not_handled <- intersect( x = c(unique(c(class(x_vector),class(y_vector)))),y = not_handled)
  #check on variabe type
  if(class(x_vector) %in% not_handled | class(y_vector) %in% not_handled){
    message(paste("you have variables of class ",
                  paste(unique(c(class(x_vector),class(y_vector))),collapse = " and "),
                  ". Not all checks are implemented for ", paste(available_and_not_handled, collapse = " and ")))}




area_categories <- c("readability of the plot",
                     "data density",
                     "data to ink ratio",
                     "adequateness of labeling")

default_n_of_bins     <- 30 # the default ggplot setting for the number of bins
data_threshold        <- 20 # following tufte we set 20 as threshold to suggest the useR to avoid graphs
overplotting_floor    <- if(!is.na(match("GeomLine",geoms_lister(plot_object ,n_of_layers)))){
  16
} else{1} # judgmental, based on experience
layers_tresholds     <- 5   # judgmental, based on experience
correlation_threshold <- .4  # judgmental, based on experience
bins_distance         <- 6   # judgmental, based on experience
p_build               <- ggplot_build(plot_object)

# perform checks for each of the area and principle and assign a score



# READABILITY OF THE PLOT

## is it a pie chart?

pie_results <- list(           area_label = area_categories[1],
                               topic_label = "pie_chart",
                               test = as.logical(is_pie_chart(plot_object, n_of_layers)),
                               ispositive = as.logical(!is_pie_chart(plot_object, n_of_layers)),
                               additional_data = list())

## are there too many layers?

layers_results <- list(
                               area_label = area_categories[1],
                               topic_label = "number_of_layers",
                               test = n_of_layers,# here the number of layers
                               ispositive = !(n_of_layers > layers_tresholds ),
                               additional_data = n_of_layers)

## is the user showing more dimensions than the plot would allow to?

dimension_results <- list(
                               area_label = area_categories[1],
                               topic_label = "number_of_dimensions",
                               test = too_many_dimensions(plot_object,n_of_layers),
                               ispositive = !too_many_dimensions(plot_object,n_of_layers),
                               additional_data = list())

## in case of geom_histogram study the optimal number of bins, employing the Freedman Diaconis rule

bins_results <- list(
                                  area_label = area_categories[1],
                                  topic_label = "number_of_bins",
                                  test = histogram_bins_tester(plot_object,n_of_layers,default_n_of_bins)[[3]], #here the distance from the optimal number of bins
                                  ispositive = !(histogram_bins_tester(plot_object,n_of_layers,default_n_of_bins)[[3]]>bins_distance),
                                  additional_data = histogram_bins_tester(plot_object,n_of_layers,default_n_of_bins)[2:3] )
## let's check if we are looking at a bar plot and if yes we check if it is flipped or not

flipped_bar_results <- list(
  are_label = area_categories[1],
  topic_label = "flipped_barplot",
  test = is_horizontal_barplot(plot_object,n_of_layers),# TRUE here means we are looking at an horizontal barplot, which is good
  ispositive = is_horizontal_barplot(plot_object,n_of_layers),
  additional_data = list()
)

# we look here for correlation between x and y and for the presence of geom_smooth lines.

geom_smooth_results <- list(
                              area_label = area_categories[1],
                              topic_label = "need_for_a_smooth",
                              test = does_it_need_geom_smooth(plot_object,n_of_layers,correlation_threshold,aes_db)[[2]],#we place here the distance correlation to better apply machine learning technique
                              ispositive = !(does_it_need_geom_smooth(plot_object,n_of_layers,correlation_threshold,aes_db)[[2]]>correlation_threshold),
                              additional_data = does_it_need_geom_smooth(plot_object,n_of_layers,correlation_threshold,aes_db)[[2]]

)

# DATA DENSITY

## we check here for the user wasting his time developing a graph for less than 20 points to show

n_data_results <-  list(
                                area_label      = area_categories[2],
                                topic_label     = "sufficient_number_of_data",
                                test            = too_few_data(plot_object, data_threshold), #TRUE here means you have enough data
                                ispositive      = too_few_data(plot_object, data_threshold),
                                additional_data = list ())

## check here for overplotting

overplotting_results <- list(
                               area_label = area_categories[2],
                               topic_label = "overplotting",
                               test       = cozy_plot(plot_object, n_of_layers,overplotting_floor,aes_db)[[2]], # the median distance here as a measure of overplotting
                               ispositive = !(cozy_plot(plot_object, n_of_layers,overplotting_floor,aes_db)[[2]]<overplotting_floor),# if distance is lower than overplotting floor we have a cozy plot
                               additional_data = cozy_plot(plot_object, n_of_layers,overplotting_floor,aes_db)[[2]])

# DATA TO INK RATIO

## check for the use of default theme and by that way the grey background

background_results <- list(
                               area_label = area_categories[3],
                               topic_label = "use_of_heavy_background",
                               test = heavy_background(plot_object),
                               ispositive = !heavy_background(plot_object),
                               additional_data = list()) #TRUE here means we are looking at an heavy background, either being one set from the user or the default one ( if still grey)
## check for bar map with full, non white filling not mapped to any aes

filled_barplot_results <- list(
                               area_label = area_categories[3],
                               topic_label = "filled_barplot",
                               test = filled_barplot(plot_object,n_of_layers),
                               ispositive = !filled_barplot(plot_object,n_of_layers),
                               additional_data = list()) #TRUE here means we are looking at an ink-wasting barplot filled with one meaningless colour


# ADEQUATENESS OF LABELLING

##check for presence of descriptive labels: title, subtitle and caption

title_results <- list(
                               area_label = area_categories[4],
                               topic_label = "presence_of_title",
                               test = labels_finder(plot_object,"title"), # TRUE here means we found the label
                               ispositive = labels_finder(plot_object,"title"),
                               additional_data = list())

subtitle_results <- list(
                               area_label = area_categories[4],
                               topic_label = "presence_of_subtitle",
                               test = labels_finder(plot_object,"subtitle"), # TRUE here means we found the label
                               ispositive = labels_finder(plot_object,"subtitle"),
                               additional_data = list())

caption_results <- list(
                               area_label = area_categories[4],
                               topic_label = "presence_of_caption",
                               test = labels_finder(plot_object,"caption"), # TRUE here means we found the label
                               ispositive = labels_finder(plot_object,"caption"),
                               additional_data = list())

## loook for special characters within labels

special_characters_results <- list(
                               area_label = area_categories[4],
                               topic_label = "special_characters_in_label",
                               test = labels_reader(plot_object)[[1]],#TRUE here means we found special character
                               ispositive = !(labels_reader(plot_object)[[1]]),
                               additional_data = labels_reader(plot_object)[2])
## a polished check : we try to understand if there are outliers in data and if they are in some way labeled within the plot

outliers_results <- list(
                               area_label = area_categories[4],
                               topic_label = "outliers_not_labelled",
                               test = outlier_labels(plot_object,n_of_layers,p_build,aes_db),#TRUE here means we have outliers not labelled
                               ispositive = !(outlier_labels(plot_object,n_of_layers,p_build,aes_db)),
                               additional_data = list())

check_results <- rbind(pie_results,
                       layers_results,
                       dimension_results,
                       bins_results,
                       flipped_bar_results,
                       geom_smooth_results,
                       n_data_results,
                       overplotting_results,
                       background_results,
                       filled_barplot_results,
                       title_results,
                       subtitle_results,
                       caption_results,
                       special_characters_results,
                       outliers_results)

return(check_results)}
