#' @title Top Data by Count (returns new data set with only top values of choosen group)
#'
#' @param  dataset -- Data Frame to evaluate
#' @param  group -- Grouping that you wish to use on the data frame
#' @param  n --  Is the number of top items in the group you want to display
#'
#' @import dplyr
#' @import ggplot2
#' @import lazyeval"
#'
#' @export

scatter_fuacet_plot <- function(dataset, group, x, n, limit, bw=1){

  #remove rows where the "y" value or "x" value is blank
  filter_criteria <- interp(~y != x, .values=list(y = as.name(x), x = ""))
  dataset <- dataset %>% filter_(filter_criteria)
  filter_criteria <- interp(~y != x, .values=list(y = as.name(group), x = ""))
  dataset <- dataset %>% filter_(filter_criteria)

  #count up instances of each group
  sorted_dataset <- dataset %>%
            filter_(filter_criteria)%>%
            group_by_at(vars(one_of(group))) %>%
            summarise(count = n())

  #remove any groups that do not have more than 50 instances
  filter_criteria <- interp(~y > x, .values=list(y = as.name("count"), x = as.integer(limit)))
  sorted_dataset <- sorted_dataset %>% filter_(filter_criteria)

  dataset <- dataset %>% filter(dataset[[group]] %in% sorted_dataset[[group]])

  #sort and order by user selection
  TopN <- dataset %>%
    group_by_at(vars(one_of(group))) %>%
    summarise_at(.vars = vars(x),.funs = c(mean="mean")) %>%
    arrange(-mean)%>%
    head(n)

  TopData <- dataset %>% filter(dataset[[group]] %in% TopN[[group]])
  plot_title <- paste("Histogram of -", x, "- for -", group)

    p <- ggplot(TopData, aes_string(x=x, fill = group)) +
                geom_histogram(binwidth = bw) +
                facet_wrap(~TopData[[group]], ncol = 3) +
                theme(legend.position = "none") +
                ggtitle(plot_title)
 return(p)
}

