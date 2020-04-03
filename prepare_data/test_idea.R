# # test 
# 
# dt %>% 
#   left_join(wb_dt, by = c("country_region" = "Country")) %>% 
#   head()
# 
# 
# 
# 
# wb_dt %>%
#   filter (Date == max (Date, na.rm = TRUE))%>%
#   select (Country, death_cases, pop_age_65,population,confirmed_cases,gdp_capita ) %>%
#   distinct()%>%
#   filter (death_cases > 0) %>%
#   filter (Country != "San Marino") %>%
#   mutate (death_cases_rate = death_cases / population * 10000000) %>%
#   mutate (pop_age_65 = pop_age_65 / 100)%>%
#   ggplot (aes(pop_age_65, death_cases_rate)) + geom_point() +
#   scale_x_continuous(labels = percent) + 
#   scale_y_log10(labels = comma) +
#   geom_text_repel(aes (label = Country), size = 2) +
#   geom_smooth(method = "lm") +
#   xlab ("\n% Population above 65")+
#   ylab ("COVID-19 Deaths per 10 M population\n")
# 
# 
# wb_dt %>%
#   filter (Date == max (Date, na.rm = TRUE))%>%
#   select (Country, death_cases, pop_age_65,population,confirmed_cases,gdp_capita, income,import_goods, nurse, physicians) %>%
#   distinct()%>%
#   filter (death_cases > 0) %>%
#   filter (Country != "San Marino") %>%
#   mutate (death_cases_rate = death_cases / population * 10000000) %>%
#   mutate (pop_age_65 = pop_age_65 / 100)%>%
#   ggplot (aes(physicians, death_cases_rate)) + 
#   geom_point() +
#   scale_x_continuous(labels = comma) + 
#   scale_y_log10(labels = comma) +
#   geom_text(aes (label = Country), size = 2) +
#   geom_smooth(method = "lm") +
#   xlab ("\n% Population above 65")+
#   ylab ("COVID-19 Deaths per 10 M population\n")




plt_dt <- wb_dt %>%
  filter (Date == max(Date, na.rm = TRUE)) %>%
  # distinct (Country, death_cases, pop_age_65,population,confirmed_cases,gdp_capita, income,import_goods, nurse, physicians,
  #           ) %>%
  filter (death_cases > 0) %>%
  filter (Country != "San Marino") %>%
  mutate (death_cases_rate = death_cases / population) %>%
  mutate (pop_age_65 = pop_age_65 / 100) %>%
  select(Country, population, death_cases_rate, 
         urban_pop, life_expectancy, pop_age_65, 
         nurse, physicians, hospital_bed, 
         unemployment, gdp_capita,v_employment_male, v_employment_female) %>% 
  mutate_at(vars(nurse, physicians, hospital_bed), funs(./population)) %>% 
  select(-population) %>% 
  pivot_longer(cols = c("urban_pop", "life_expectancy", "pop_age_65", 
                        "nurse", "physicians", "hospital_bed", 
                        "unemployment", "gdp_capita","v_employment_male","v_employment_female"), 
               names_to = "Plot.title", values_to = "value") %>% 
  group_by(`Plot.title`) %>% 
  mutate(value = value/max(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(death_cases_rate = death_cases_rate/max(death_cases_rate, na.rm = TRUE)) %>% 
  as.data.table()

# plt_dt %>% 
#   ggplot(aes(x = Plot.title, y = value, color = Country, group = Country)) +
#   geom_line(alpha = 0.5, aes(size = death_cases_rate)) +
#   # geom_col(data = filter(monash_end, Area %in% c("Research", "Education")), 
#   #          alpha = 1, width = 0.6) +
#   coord_polar() +
#   #theme_void()  +
#   scale_x_discrete(labels = wrap_format(20))+ # wrap text for x axis labels
#   scale_alpha_manual(values = c(.5, 1)) +
#   geom_hline(yintercept = 1:8, colour = "white") +
#   # geom_vline(xintercept = seq(1.5, 20.5, 1), colour = "white") +
#   geom_hline(yintercept = 8, colour = "grey80", linetype = "dashed") +
#   # geom_vline(xintercept = c(4.5, 0.5), colour = "grey80", linetype = "dashed")+
#   labs(caption = "Monash University Performance based on most recent data available.")+
#   #geom_vline(xintercept = seq(1, 20.5, 1), colour = "grey90", linetype = "dashed") +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.text.x = element_text(size = 12, hjust = 2),
#         # legend.position = "none",
#         panel.background = element_rect(fill = "white", colour = "white"),
#         plot.caption = element_text(size = 14, hjust = 0, face = "italic"),
#         legend.text = element_text(size = 13, colour = "grey40"),
#         legend.title = element_blank())




# Library
library(fmsb)



plt_radar_dt <- plt_dt %>% 
  filter( Country %in% c("Italy", "Australia", "China", "US", "Iran", "South Korea")) %>% 
  group_by(Country) %>% 
  pivot_wider(names_from = "Plot.title", values_from = "value") %>% 
  ungroup() %>% 
  column_to_rownames(var = "Country") %>% 
  rbind(rep(1,5), rep(0,5), .)


radarchart(plt_radar_dt,
           # #custom polygon
           pcol=country_color , #pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           # 
           #custom the grid
           cglcol="grey80", cglty=1, axislabcol="grey80", caxislabels=seq(0,1,5), 
           cglwd=0.8#,
           # 
           # #custom labels
           # vlcex=0.8 
           )
legend(x=1.4, y=1, legend = rownames(plt_radar_dt[-c(1,2),]), bty = "n", pch=20 , col=country_color , text.col = "grey", cex=1.2, pt.cex=3)




# # Create data: note in High school for Jonathan:
# data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
# colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )
# 
# # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
# data <- rbind(rep(20,10) , rep(0,10) , data)
# 
# # Check your data, it has to look like this!
# # head(data)
# 
# # The default radar chart 
# radarchart(data)
# 
# 
# radarchart( data  , axistype=1 , 
#             
#             #custom polygon
#             pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
#             
#             #custom the grid
#             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
#             
#             #custom labels
#             vlcex=0.8 
# )



## dt %>% write_csv(str_c("/Volumes/GoogleDrive/My Drive/google_transparency/data/","covid19_", Sys.Date(), ".csv"))



### find correlation 

library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")  ##To use the rquery.cormat function, you can source it


head(wb_dt)


cordt <- wb_dt %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  select_if(is.numeric) %>% 
  select(-c(death_cases, recovered_cases, active_cases)) 

#rquery.cormat(select_if(wb_dt, is.numeric))
res <- cor(cordt, use = "pairwise", method = "pearson")
round(res,2)


library(Hmisc)

res2 <- rcorr(as.matrix(cordt))
res2
  

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(res2$r, res2$P) %>% 
  filter(column == "confirmed_cases", !is.na(cor)) %>% 
  arrange(desc(cor))



library(corrr)
cordt %>% 
  correlate() %>% 
  focus(confirmed_cases)




# 
# select_if(wb_dt, is.numeric) %>% 
#   correlate() %>% 
#   rearrange() %>% 
#   shave()

data <- radar_dt %>% 
  filter(Country == "Italy") %>% 
  mutate(id = 1:n())

label_data <- data

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=value+10, label=Plot.title, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p





data <- data.frame(
  id=seq(1,60),
  individual=paste( "Mister ", seq(1,60), sep=""),
  value=sample( seq(10,100), 60, replace=T)
)

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- data

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


p <- ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p












