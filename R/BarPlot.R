#' @title Bar Plot Generator
#'
#' @param  dataset -- Data Frame to evaluate
#' @param  group -- Column in data (the variable) that you wish to look at.
#' @param  value --  Is value in the column ( the variable) you wish to create a density plot
#' @param  plot --  the parameter that you wish to use for the density plot
#'
#' @import dplyr
#' @import ggplot2
#' @import lazyeval
#'
#' @export

bar_plot <- function(dataset, group, value, y, ylimit){



  #filter the data by the set by the goup (variable) that is equal to value choosen
  if(value == ""){

  }else{
    #filter the data by the set by the goup (variable) that is equal to value choosen
    filter_criteria <- interp(~y == x, .values=list(y = as.name(group), x = value))
    dataset <- dataset %>% filter_(filter_criteria)

  }



  #remove blank entries from the dataset
  filter_criteria <- interp(~y != x, .values=list(y = as.name(y), x = ""))
  dataset <- dataset %>% filter_(filter_criteria)
  filter_criteria <- interp(~y <= x, .values=list(y = as.name(y), x = as.numeric(ylimit)))
  dataset <- dataset %>% filter_(filter_criteria)

  p <-  (ggplot(dataset, aes_string(x = y, fill = group)) +
           geom_bar())
   return(p)

}

