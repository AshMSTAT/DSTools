#' @title Density Plot Generator
#'
#' @param  dataset -- Data Frame to evaluate
#' @param  col_name -- Column in data (the variable) that you wish to look at.
#' @param  value --  Is value in the column ( the variable) you wish to create a density plot
#' @param  rank --  the parameter that you wish to use for the density plot
#' @param  Bandwith -- width of smothting in kernal density estimation
#'
#' @export



density_plot <- function(dataset, col_name, rank, value = "" , bandwidth){

  require("dplyr")
  require("ggplot2")
  require("lazyeval")


  filter_criteria <- interp(~y == x, .values=list(y = as.name(col_name), x = value))
  dataset <- dataset %>% filter_(filter_criteria)

  p <-  (ggplot(dataset, aes_string(x = rank, fill = col_name)) +
          geom_density(bw=bandwidth))

  return(p)

}











