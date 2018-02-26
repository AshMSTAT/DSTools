#' @title QQ Plot Generator
#'
#' @param  dataset -- Data Frame to evaluate
#' @param  group -- Column in data (the variable) that you wish to look at.
#' @param  value --  Is value in the column ( the variable) you wish to create a density plot
#' @param  plot --  the parameter that you wish to use for the qq plot
#'
#' @export

qq_plot <- function(dataset, group, value = "", plot){

  require("dplyr")
  require("ggplot2")
  require("lazyeval")

  if(value == ""){
      plot_title <- paste("QQ Plot of   -", plot, "-   for  -", group, "-")
    }else{
      #filter the data by the set by the goup (variable) that is equal to value choosen
      filter_criteria <- interp(~y == x, .values=list(y = as.name(group), x = value))
      dataset <- dataset %>% filter_(filter_criteria)
      plot_title <- paste("QQ Plot of   -", plot, "-   for  -", group, "-", value)
    }

  #remove blank entries from the dataset
  filter_criteria <- interp(~y != x, .values=list(y = as.name(plot), x = ""))
  dataset <- dataset %>% filter_(filter_criteria)

  # create qqline
  y <- quantile(dataset[[plot]], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]


  p <-  ggplot(dataset, aes_string(sample = plot)) +
           stat_qq() +
           geom_abline(slope = slope, intercept = int) +
           ggtitle(c(plot_title))

  return(p)
}



