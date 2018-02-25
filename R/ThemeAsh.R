#' @title Custom Theme  -- theme_ash
#'
#'
#' @export






theme_ash <- function () {
  theme_dark(base_size=12, base_family="Avenir") %+replace%
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="gray96", colour=NA),
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA)
    )
}
