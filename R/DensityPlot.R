#' @title Density Plot Generator
#'
#' @param  dataset -- Data Frame to evaluate
#' @param  group -- Column in data (the variable) that you wish to look at.
#' @param  value --  Is value in the column ( the variable) you wish to create a density plot
#' @param  plot --  the parameter that you wish to use for the density plot
#' @param  Bandwith -- width of smothting in kernal density estimation
#'
#' @import dplyr
#' @import ggplot2
#' @import lazyeval
#'
#' @export

density_plot <- function(dataset, group, value, plot, bandwidth=1){


  filter_criteria <- interp(~y == x, .values=list(y = as.name(group), x = value))
  dataset1 <- dataset %>% filter_(filter_criteria)



  #remove blank entries from the dataset
  filter_criteria <- interp(~y != x, .values=list(y = as.name(plot), x = ""))
  dataset1 <- dataset1 %>% filter_(filter_criteria)


  p <-  ggplot(dataset1, aes_string(x = plot, fill = group)) +
          geom_density(bw=bandwidth)

  return(p)

}


 density_plot(wines, "variety", "Merlot","price")









