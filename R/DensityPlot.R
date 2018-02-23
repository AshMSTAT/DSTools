#' @title Density Plot for Snow Fall Data
#'
#' @param  Samples Specify how many samples
#'
#' @export









library(tidyverse)
library(knitr)
library(ggplot2)
library(dplyr)
library(xtable)




wine_data <- read.csv("https://raw.githubusercontent.com/JackStat/PracticalDataScience/master/data/winemag-data-130k-v2.csv")
wines <- as.tibble(select(wine_data, -X))
list(wines)


Top6Tasters <- wines %>%filter(taster_name != "")%>% group_by(taster_name) %>% summarise(count = n(), AverageScore = round(mean(points),2)) %>% arrange(-count) %>% head(6)
Top6<- wines %>% filter(taster_name %in% Top6Tasters$taster_name & !is.na(price))

knitr::kable(Top6Tasters[ , ],caption = "Top Tasters by number of reviews .")

sd = sd(Top6$points, na.rm = True)
ggplot(Top6, aes(x=points, color = taster_name)) +
  geom_freqpoly(binwidth = 1)+
  facet_wrap(~taster_name, ncol = 3) +
  theme_grey() +
  theme(legend.position = "none") +
  ggtitle("Top 6 Taste Testers")



