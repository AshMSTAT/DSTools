#' @title Bar Plot Generator
#'
#' @param  dataset -- Data Frame to evaluate
#' @param  col_name -- Column in data (the variable) that you wish to look at.
#' @param  value --  Is value in the column ( the variable) you wish to create a density plot
#' @param  rank --  the parameter that you wish to use for the density plot
#'
#' @export



bar_plot <- function(dataset, col_name, value = "", rank){

  require("dplyr")
  require("ggplot2")
  require("lazyeval")


  filter_criteria <- interp(~y == x, .values=list(y = as.name(col_name), x = value))
  dataset1 <- dataset %>% filter_(filter_criteria)

  #remove blank entries from the dataset
  filter_criteria <- interp(~y != x, .values=list(y = as.name(rank), x = ""))
  dataset1 <- dataset1 %>% filter_(filter_criteria)

  p <-  (ggplot(dataset1, aes_string(x = rank, fill = col_name)) +
           geom_bar())

  return(p)

}
