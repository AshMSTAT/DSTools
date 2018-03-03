#' @title Top Data by Count (returns new data set with only top values of choosen group)
#'
#' @param  dataset -- Data Frame to evaluate
#' @param  group -- Grouping that you wish to use on the data frame
#' @param  n --  Is the number of top items in the group you want to display
#'
#' @import dplyr
#' @import ggplot2
#' @import lazyeval
#'
#' @export

top_data <- function(dataset, group, n){

  filter_criteria <- interp(~y != x, .values=list(y = as.name(group), x = ""))
  TopN <- dataset %>%
    filter_(filter_criteria)%>%
    group_by_at(vars(one_of(group))) %>%
    summarise(count = n()) %>%
    arrange(-count) %>%
    head(n)

  TopData <- dataset %>% filter(dataset[[group]] %in% TopN[[group]])


  return(TopData)

}



