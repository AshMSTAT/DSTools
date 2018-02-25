#' @title Custom Theme  -- theme_ash  (Uses smaller font)
#'
#'
#' @export






theme_ash <- function () {
  theme_dark(base_size=10) %+replace%
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="gray96", colour=NA),
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA)
    )
}
