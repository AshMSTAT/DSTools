#' @title Statistic Graphs Comparison
#'
#' @param  dataset --  Data Frame to evaluate
#' @param  group --    Column in data (the variable) that you wish to look at.
#' @param  value --    Is value in the column ( the variable) you wish to create a plot for
#' @param  plot --     The parameter that you wish to use for plotting
#' @param  Bandwith -- width of smothting in kernal density estimation
#'
#' @export


graph_stats <- function(dataset, group, value, plot, bandwidth=1){

  require(gridExtra)


  g1 <- scatter_plot(dataset, group, value, plot)
  g2 <- bar_plot(dataset, group, value, plot)
  g3 <- density_plot(dataset, group, value, plot, bandwidth)
  g4 <- qq_plot(dataset, group, value, plot)

  grid.arrange(g1, g2, g3, g4, ncol=2)

}

graph_stats(wines,"variety", "Merlot", "points")
