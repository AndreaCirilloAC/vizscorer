#' produce a customised report analysing the submitted ggplot object
#' @param plot_object the ggplot object being analysed
#' @description call to \code{scorer_bot} function produces the sequential calling of all function included within
#' the \code{vizscorer} package, with the purpose to produce a customised report about the submitted ggplot object. The function handles call to \code{metadata_reader} function
#' to draw metadata from the plot object in a format ready to be submitted to the scorer algorithm. within the next step those metadata are submitted to the \code{scorer} function which apply a gradient boosting pre-trained model to them.
#' a final score from 0 to 1 is computed as a probability to be a good plot.
#' To provide more relevant information to the user a set of advices are retrieved from a built-in knowledge base, selecting only those relevant to suboptimal areas of the plot, observed from metadata.
#' Finally a revealjs deck of slides is programmatically created leveraging a quasi-real natural language generation algorithm.
#' @return a reveljs deck of slide containing an evaluation of the effectiveness of ggplot objects and suggestion about how to improve it.

scorer_bot <- function(plot_object = NULL){
 plot_metadata_raw <- metadata_reader(plot_object) %>% as.data.frame()
 plot_metadata <- data.frame(area_label = unlist(plot_metadata_raw$area_label),
                             topic_label = unlist(plot_metadata_raw$topic_label),
                             test        = unlist(plot_metadata_raw$test),
                             ispositive = unlist(plot_metadata_raw$ispositive))

  score <- scorer(plot_metadata)
  score_db <- data.frame("good"=score,"count"=1)
  comparison_path <- system.file("extdata", "comparison_db.csv", package = "vizscrorer")
  comparison_db <- read.csv(comparison_path, sep = ",", stringsAsFactors = FALSE) %>%
    select(good,model)

  comparison_db %>%
    ggplot(aes(x = good))+
    geom_histogram(binwidth = 0.01,colour = "grey80",fill ="grey80")+
    geom_point(data =score_db,
               aes(x=good,y=count),
               inherit.aes = FALSE,colour = "red",size=3)+
    theme_minimal()+
    xlab("score")+
    labs(title="comparison among training plots and your plot")+
    annotate("segment", x = score, xend = 0.4, y = 1, yend = 5,colour ="grey50")+
  annotate(geom ="text",
           label = "you are here",
           x = 0.4,y=5.1,colour="grey20",size= 7) -> score_plot

  score_map <- data.frame(lower = c(0,0.11,0.41,0.71),
                          upper = c(0.10,0.40,0.70,1),
                          judgment = c("poorly", "improvable","good!", "great!"))

  score_map$score_match <- mapply(between,
                                  x = rep(score,nrow(score_map)),
                                  left = score_map$lower,
                                  right = score_map$upper,
                                  SIMPLIFY = TRUE)
  score_map %>%
    filter(score_match == TRUE) %>%
    select(judgment) %>% pull() %>%
    as.character() -> judgment

  # from metadata, leveraging area_label and ispositive attribute, we highlight which area of dataviz
  # shows problem for the plot
  # we will highlight those area to our user
  # brief test for histograms to filter out test note relevant for non-histogram plots
  is_histogram <- test_for_histogram(plot_object,plot_object$layers %>% length())

  plot_metadata %>%
  {if (is_histogram == FALSE) filter(., topic_label != "number_of_bins" & topic_label != "flipped_barplot" ) else filter(.,topic_label !="stop")}  %>%
    select(area_label,ispositive) %>%
    group_by(area_label,ispositive) %>%
    count() %>%
    spread(key = ispositive, value = n) %>%
    mutate(positive_ratio = `TRUE`/sum(`TRUE`,`FALSE`, na.rm = TRUE)) %>%
    mutate(area_ratio     =round(positive_ratio,2)) %>%
    rename(passed = `TRUE`,failed = `FALSE`) %>%
    mutate(total = sum(passed , failed,na.rm = TRUE)) -> positive_ratio_db

  positive_ratio_db %>%
    select(area_label,failed,passed,area_ratio) -> positive_ratio_show

  positive_ratio_show[is.na(positive_ratio_show)] <- 0

  positive_ratio_db %>%
    filter(positive_ratio == min(.$positive_ratio)) %>%
    select(area_label) %>%
    pull() %>%
    as.character() -> worst_area # the worst area was the one in which the plot obtained ther worst positive rate,
  #i.e. number of positive results in test given the overall number of test for that area.

  # we then select each specific test showing problems, so to give specific advice for each of this elements.
  plot_metadata %>%
  {if (is_histogram == FALSE) filter(., topic_label != "number_of_bins" & topic_label != "flipped_barplot" ) else filter(.,topic_label !="stop")}  %>%
    select(topic_label,ispositive) %>%
    filter(ispositive == FALSE) -> errors_db

  # merge errors_db with a db storing a suggestion for each possible error
  advices_path <- system.file("extdata", "plot_advices.csv", package = "vizscrorer")
  advices_db <- read.csv(advices_path, sep = ";", stringsAsFactors = FALSE)

  errors_db %>%
    left_join(.,advices_db,by = "topic_label") -> teaching_db

n_of_errors <- nrow(errors_db)
report_path <- system.file("extdata", "report.Rmd", package = "vizscrorer")
file.copy(report_path, to = "plot_report.Rmd",overwrite = TRUE)
write(" ", file = "plot_report.Rmd", append = TRUE)
for (i in 1:n_of_errors){


  write("---", file = "plot_report.Rmd", append = TRUE)
  write(" ", file = "plot_report.Rmd", append = TRUE)
  write(paste0("__",teaching_db[i,4],"__"), file = "plot_report.Rmd", append = TRUE)
  write("  " , file = "plot_report.Rmd", append = TRUE)
  write(paste0(teaching_db[i,3],"<span class=blinking-cursor>_</span>",sep = ""),file = "plot_report.Rmd", append = TRUE) #<- il problema è in questa aggiunta
  write("  " , file = "plot_report.Rmd", append = TRUE)

}

 write("---", file = "plot_report.Rmd", append = TRUE)
 write("Hope you will find this useful and will help you improve your plot", file = "plot_report.Rmd", append = TRUE)
 write("  " , file = "plot_report.Rmd", append = TRUE)
 write("Cheers,", file = "plot_report.Rmd", append = TRUE)
 write("  " , file = "plot_report.Rmd", append = TRUE)
 write("_your dataviz bot advisor_", file = "plot_report.Rmd", append = TRUE)
 write("  " , file = "plot_report.Rmd", append = TRUE)

  rmarkdown::render("plot_report.Rmd",  revealjs_presentation())
  system("open plot_report.html")
}


