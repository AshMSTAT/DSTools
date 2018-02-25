#' @title Top Grouping by Count
#'
#' @param  dataset -- Data Frame to evaluate
#' @param  group -- Grouping that you wish to use on the data frame
#' @param  n --  Is the number of top items in the group you want to display
#'
#' @export


top_group <- function(dataset, group, n){

  require("dplyr")
  require("knitr")
  require("lazyeval")


  filter_criteria <- interp(~y != x, .values=list(y = as.name(group), x = ""))
  TopGroup <- dataset %>%
                filter_(filter_criteria)%>%
                group_by_at(vars(one_of(group))) %>%
                summarise(count = n()) %>%
                arrange(-count) %>%
                head(n)

  p <- knitr::kable(TopGroup[ , ], caption = "By number of reviews.")
  return(p)
}






