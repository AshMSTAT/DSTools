#' @title Top Data by Count (returns new data set with only top values of choosen group)
#'
#' @param  dataset -- Data Frame to evaluate
#' @param  group -- Grouping that you wish to use on the data frame
#' @param  n --  Is the number of top items in the group you want to display
#'
#' @import dplyr
#' @import ggplot2
#' @import lazyeval
#'
#' @export

scatter_fuacet_plot <- function(dataset, group, x, y, n){

  filter_criteria <- interp(~y != x, .values=list(y = as.name(group), x = ""))

  TopN <- dataset %>%
    filter_(filter_criteria)%>%
    group_by_at(vars(one_of(group))) %>%
    summarise(count = n()) %>%
    arrange(-count) %>%
    head(n)

  TopData <- dataset %>% filter(dataset[[group]] %in% TopN[[group]])
  plot_title <- paste("Plot of -", y, "- vs -", x, "- for -", group)

    p <- ggplot(TopData, aes_string(x=x, y=y, color = group)) +
      geom_point(position = 'jitter', alpha =.50)+
      facet_wrap(~TopData[[group]], ncol = 3) +
      ylim(c(0,500))+
      theme(legend.position = "none") +
      geom_smooth(method = "loess", span = 1, se = FALSE, color ='maroon')+
      ggtitle(plot_title)


 return(p)
}

