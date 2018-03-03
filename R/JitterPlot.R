#' @title Jittered Scatter Plot Generator
#'
#' @param  dataset -- Data Frame to evaluate
#' @param  group --    Column in data (the variable) that you wish to look at.
#' @param  value --    Is value in the column ( the variable) you wish to create a plot for
#' @param  x --     The control or input variable
#' @param  y --     The result or output variable that you wish to use for plotting
#' @param  ylimit -- limit on the y value
#'
#' @export

jitter_plot <- function(dataset, group, value="", x, y, ylimit){


  require(dplyr)
  require(ggplot2)
  require(lazyeval)

  #filter the data by the set by the goup (variable) that is equal to value choosen
    if(value == ""){
    plot_title <- paste("Scatter Plot -", x,"- vs -", y, "- for -", group, ".")
  }else{
    #filter the data by the set by the goup (variable) that is equal to value choosen
    filter_criteria <- interp(~y == x, .values=list(y = as.name(group), x = value))
    dataset <- dataset %>% filter_(filter_criteria)
    plot_title <- paste("Scatter Plot of -", x, "- vs -", y, "- for -", group, "-", value)
  }

  #remove blank entries from the dataset
  filter_criteria <- interp(~y != x, .values=list(y = as.name(x), x = ""))
  dataset <- dataset %>% filter_(filter_criteria)
  filter_criteria <- interp(~y != x, .values=list(y = as.name(y), x = ""))
  dataset <- dataset %>% filter_(filter_criteria)
  filter_criteria <- interp(~y <= x, .values=list(y = as.name(y), x = as.numeric(ylimit)))
  dataset <- dataset %>% filter_(filter_criteria)

  p <-  (ggplot(dataset, aes_string(x = x, y = y, color = group)) +
           geom_jitter()) +
           geom_smooth(method = "loess", span = 1, se = FALSE)+
           ggtitle(c(plot_title))

  return(p)

}



