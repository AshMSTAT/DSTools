#' @title Scatter Plot Generator
#'
#' @param  dataset -- Data Frame to evaluate
#' @param  group -- Column in data (the variable) that you wish to look at.
#' @param  value --  Is value in the column ( the variable) you wish to create a density plot
#' @param  plot --  the parameter that you wish to use for the qq plot
#'
#' @import dplyr
#' @import ggplot2
#' @import lazyeval
#'
#' @export
#'

scatter_plot <- function(dataset, group, value, plot, ylimit){

  #filter the data by the set by the goup (variable) that is equal to value choosen
  filter_criteria <- interp(~y == x, .values=list(y = as.name(group), x = value))
  dataset1 <- dataset %>% filter_(filter_criteria)


  #remove blank entries from the dataset
  filter_criteria <- interp(~y != x, .values=list(y = as.name(plot), x = ""))
  dataset1 <- dataset1 %>% filter_(filter_criteria)
  filter_criteria <- interp(~y <= x, .values=list(y = as.name(y), x = as.numeric(ylimit)))
  dataset <- dataset %>% filter_(filter_criteria)

  n <- length(dataset1[[plot]])
  p <-  ggplot(dataset1, aes_string(y = plot, x = seq(1,n), fill = group)) +
                geom_point()
  return(p)

}



