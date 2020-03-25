library(shiny)
library(shinyWidgets)
library(tidyverse)
library(scales)
# library(plotly)
library(data.table)
library(lubridate)
library(ggrepel)
library(xml2)
#library(rsconnect)
library(bit64)
#library(gganimate)
#library(gifski)
#library(png)
#library(maps)
library(RCurl)
library(httr)
library(ggalluvial)
theme_set(theme_minimal())
#library(tidylog)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggthemes)
library(yarrr)
library(scales)
library(ggrepel)


#install.packages("gtrendsR")
library(gtrendsR)

list = countries %>%
  filter (sub_code =="")%>%
  select (country_code,name)%>%
  distinct() %>%
  .[1:217,]

France = gtrends(c("chomage"), geo = c("FR"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]] %>%
  select (date,hits, geo)

France %>% ggplot (aes (date, as.numeric (hits))) + geom_line() + geom_point() + scale_y_log10()


English = gtrends(c("unemployment"), geo = c("AU", "US","GB","CA"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]]%>%
  select (date,hits, geo)

Italy = gtrends(c("disoccupazione"), geo = c("IT"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]] %>%
  select (date,hits, geo)

Austria = gtrends(c("melden "), geo = c("AT"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]] %>%
  select (date,hits, geo)

Belgium = gtrends(c("werkloosheid "), geo = c("BE"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]] %>%
  select (date,hits, geo)

Czech = gtrends(c("nezaměstnanost"), geo = c("CZ"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]] %>%
  select (date,hits, geo)

Denmark = gtrends(c("arbejdsløshed"), geo = c("DK"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]] %>%
  select (date,hits, geo)

Iran = gtrends(c("بیکاری"), geo = c("IR"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]] %>%
  select (date,hits, geo)


Japan = gtrends(c("失業"), geo = c("JP"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]] %>%
  select (date,hits, geo)

Netherlands = gtrends(c("werkloosheid"), geo = c("NL"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]] %>%
  select (date,hits, geo)

Norway = gtrends(c("arbeidsledig"), geo = c("NO"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]] %>%
  select (date,hits, geo)

#Norway %>% ggplot (aes (date, as.numeric (hits))) + geom_line() + geom_point() + scale_y_log10() + geom_smooth()


Korea = gtrends(c("실업"), geo = c("KR"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]] %>%
  select (date,hits, geo)

Spain = gtrends(c("desempleo"), geo = c("ES"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]] %>%
  select (date,hits, geo)

switz = gtrends(c("chomage"), geo = c("CH"), gprop = "web", time = "2020-02-20 2020-03-19")[[1]] %>%
  select (date,hits, geo)






desempleo

#technische werkloosheid

dt <- English %>%
rbind(France) %>% rbind (Italy) %>% rbind (Austria) %>% rbind (Belgium) %>% rbind (Netherlands) %>% rbind (Norway) %>% rbind (Korea) %>%
  rbind (Spain) %>% rbind (switz) %>%
#  rbind (Czech) %>% 
  rbind (Denmark) %>% rbind (Iran) %>% rbind (Japan) %>%
  mutate (hits = as.numeric(hits)) %>%
  mutate (date = as.Date(date, format = "%Y-%m-%d")) %>%
  left_join(list, by = c("geo" = "country_code"))

write.csv (dt, "google trends 20-02.csv")

means =   dt %>% 
  group_by(geo)%>%
  filter(date < as.Date("2020-02-28", format = "%Y-%m-%d")) %>%
  summarise (feb.avg = mean (hits, na.rm = TRUE)) %>%
  ungroup() 

dt %>%
#  left_join(means) %>%
 # filter (date >= as.Date("2020-03-01", format = "%Y-%m-%d"))%>%
 # mutate (norm.hits = hits / feb.avg) %>%
ggplot (aes (date, hits, group = name)) + geom_smooth(se = FALSE)+ geom_point() + scale_y_log10() + facet_wrap(~name, scales = "free") +
  ggsave ("Fig 1 - Figure 1.png", height = 16, width = 24, units = "cm", dpi = 300)













France = gtrends(c("chomage"), geo = c("FR"), gprop = "web", time = "2020-02-01 2020-03-19")
France [[1]]

France$interest_over_time
France$interest_by_country
France$interest_by_dma
France$related_topics
France$related_queries
