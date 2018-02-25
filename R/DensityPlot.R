#' @title Density Plot Generator
#'
#' @param  dataset -- Data Frame to evaluate
#' @param  col_name -- Column in data that you wish to look at.
#' @param  value --  Is value in the column you wish to look at
#' @param  rank --  the parameter that you wish to use for the density plot
#'
#' @export


density_plot <- function(dataset, col_name, rank, value = "" ){

  require("dplyr")
  require("lazyeval")
  filter_criteria <- interp(~y == x, .values=list(y = as.name(col_name), x = value))
  dataset <- dataset %>% filter_(filter_criteria)

  p <-  (ggplot(dataset, aes_string(x = rank, fill = col_name)) +
          geom_density(bw=1))
  return(p)

}











