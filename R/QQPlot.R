#' @title QQ Plot Generator
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


qq_plots <- function(dataset, group, value, plot){

  filter_criteria <- interp(~y != x, .values=list(y = as.name(group), x = ""))
  dataset1 <- dataset %>% filter_(filter_criteria)
  filter_criteria <- interp(~y == x, .values=list(y = as.name(group), x = value))
  dataset1 <- dataset1 %>% filter_(filter_criteria)
  filter_criteria <- interp(~y, .values=list(y = as.name(plot)))
  dataset1 <- dataset1 %>% select_(filter_criteria)

  qqnorm(dataset1[[plot]], main = plot)
  qqline(dataset1[[plot]])
}



