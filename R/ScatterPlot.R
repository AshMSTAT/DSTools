#' @title Scatter Plot Generator
#'
#' @param  dataset -- Data Frame to evaluate
#' @param  col_name -- Column in data (the variable) that you wish to look at.
#' @param  value --  Is value in the column ( the variable) you wish to create a density plot
#' @param  rank --  the parameter that you wish to use for the qq plot
#'
#' @export

require("dplyr")
require("ggplot2")
require("lazyeval")


scatter_plot <- function(dataset, group, value, plot){

  #filter the data by the set by the goup (variable) that is equal to value choosen
  filter_criteria <- interp(~y == x, .values=list(y = as.name(group), x = value))
  dataset1 <- dataset %>% filter_(filter_criteria)


  #remove blank entries from the dataset
  filter_criteria <- interp(~y != x, .values=list(y = as.name(plot), x = ""))
  dataset1 <- dataset1 %>% filter_(filter_criteria)


  #select the values to be plotted
  filter_criteria <- interp(~y, .values=list(y = as.name(plot)))
  dataset1 <- dataset1 %>% select_(filter_criteria)

  plot(dataset1[[plot]], main = plot)

}
