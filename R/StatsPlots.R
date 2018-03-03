#' @title Statistic Graphs Comparison
#'
#' @param  dataset --  Data Frame to evaluate
#' @param  group --    Column in data (the variable) that you wish to look at.
#' @param  value --    Is value in the column ( the variable) you wish to create a plot for
#' @param  x --     The control or input variable
#' @param  y --     The result or output variable that you wish to use for plotting
#' @param  Bandwith -- width of smothting in kernal density estimation
#'
#' @import dplyr
#' @import ggplot2
#' @import lazyeval
#'
#' @export


stats_plot <- function(dataset, group, value, x, y, ylimit, bandwidth=1){

require(gridExtra)

  g1 <- jitter_plot(dataset, group, value, x, y, ylimit)
  g2 <- bar_plot(dataset, group, value, x, ylimit)
  g3 <- density_plot(dataset, group, value, x, ylimit, bandwidth)
  g4 <- qq_plot(dataset, group, value, x, ylimit)

  grid.arrange(g1, g2, g3, g4, ncol=2)

}
