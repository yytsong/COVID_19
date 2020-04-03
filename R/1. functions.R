# prepare data

# c <- c("Italy", "China")
# #c("China", "Australia", "US", "Russia", "UK", "South Korea", "Italy", "Spain")
# #dt$country_region %>% unique()
# p <- dt %>% filter(country_region %in% c) %>% distinct(province_state) %>% pull()
# m <- "Cumulative Cases"
# a <- "Confirmed Cases"
# # l <- "country_region"
# g <- c("China", "Australia")
# xc <- "Cumulative Cases"
# xm <-  "Confirmed Cases"
# xa <-  "Per Capita (10 Million)"
# yc <- "New Cases"
# ym <-  "Recovered Cases"
# ya <-  "Per Capita (10 Million)"
# r <- "Recovery Percentage"
# min_case <- 100


# # fomratting functions for scales
# si_num <- function (x) {
#   
#   if (!is.na(x)) {
#     
#     if (x < 0){ 
#       sign <-  "-"
#       x <- abs(x)
#     }else{
#       sign <-  ""
#       x <- x
#     }
#     
#     
#     if (x >= 1e9) { 
#       chrs <- strsplit(format(x, scientific=12), split="")[[1]];
#       len <- chrs[seq(1,length(chrs)-9)] %>% length();
#       rem <- chrs[seq(1,length(chrs)-8)];
#       rem <- append(rem, ".", after = len) %>% append("B");
#     }
#     
#     
#     else if (x >= 1e6) { 
#       chrs <- strsplit(format(x, scientific=12), split="")[[1]];
#       len <- chrs[seq(1,length(chrs)-6)] %>% length();
#       rem <- chrs[seq(1,length(chrs)-5)]
#       rem <- append(rem, ".", after = len) %>% append("M");
#     }
#     
#     else if (x >= 1e3) { 
#       chrs <- strsplit(format(x, scientific=12), split="")[[1]];
#       len <- chrs[seq(1,length(chrs)-3)] %>% length();
#       rem <- chrs[seq(1,length(chrs)-2)];
#       rem <- append(rem, ".", after = len) %>% append("K");
#     }
#     
#     else {
#       return(x);
#     }
#     
#     return(str_c(sign, paste(rem, sep="", collapse=""), sep = ""));
#   }
#   else return(NA);
# } 
# 
# si_vec <- function(x) {
#   sapply(x, FUN=si_num);
# }


############# World HERE

### filter data for exploratory tab

filtered_data_explore <- function(c, xc, xm, xa, yc, ym, ya, min_case){
  
  
  x <- combination %>% filter(measure == xc, aspect == xm, actual == xa) %>% pull(id)
  y <- combination %>% filter(measure == yc, aspect == ym, actual == ya) %>% pull(id)
  
  
  if(x %in% c(1,2,7,8)){ xm_m <- sym("confirmed_cases")}else if(x %in% c(3,4,9,10)){
    xm_m <- sym("death_cases")}else if(x %in% c(5,6,11,12)){ xm_m <- sym("recovered_cases")}
  
  
  if(y %in% c(1,2,7,8)){ ym_m <- sym("confirmed_cases")}else if(y %in% c(3,4,9,10)){
    ym_m <- sym("death_cases")}else if(y %in% c(5,6,11,12)){ ym_m <- sym("recovered_cases")}
  
  
  df <- dt %>% 
    filter(country_region %in% c, confirmed_cases >= min_case) %>% 
    select(country_region, Population, Date, !!xm_m, !!ym_m) %>% 
    group_by(country_region, Date, Population) %>% 
    summarize(xsum = sum(!!xm_m), ysum = sum(!!ym_m)) %>% 
    ungroup() %>% 
    arrange(country_region, Date) %>% 
    group_by(country_region) %>% 
    mutate(x_new_cases = ifelse(is.na(lag(xsum)), xsum , xsum - lag(xsum)),
           y_new_cases = ifelse(is.na(lag(ysum)), ysum , ysum - lag(ysum))) %>% 
    ungroup() %>% 
    mutate(x_cum_per_capita = xsum/Population*10000000,
           y_cum_per_capita = ysum/Population*10000000,
           x_new_per_capita = x_new_cases/Population*10000000,
           y_new_per_capita = y_new_cases/Population*10000000)
  
  if(x %in% c(1,3,5)){xv <- sym("xsum")}else if(x %in% c(2,4,6)){
    xv <- sym("x_cum_per_capita")}else if(x %in% c(7,9,11)){
      xv <- sym("x_new_cases")}else if(x %in% c(8,10,12)){xv <- sym("x_new_per_capita")}
                  
  if(y %in% c(1,3,5)){yv <- sym("ysum")}else if(y %in% c(2,4,6)){
    yv <- sym("y_cum_per_capita")}else if(y %in% c(7,9,11)){
      yv <- sym("y_new_cases")}else if(y %in% c(8,10,12)){yv <- sym("y_new_per_capita")}         
       
  
  # df <-                                                                                                
    df %>% 
      select(country_region, Date, !!xv, !!yv) %>% 
      rename("xvar" = xv, "yvar" = yv)
  
  
}

plot_explore_country <- function(df, xc, xm, xa, yc, ym, ya, min_case){
  
  
  df_end <- df %>%
    group_by(country_region) %>% 
    filter(Date == last(Date)) %>% 
    ungroup()
  
  
  df %>% 
    ggplot(aes(x = xvar, y = yvar, group = country_region, color = country_region))+
    geom_line()+
    geom_point(size = 0.4)+
    geom_text_repel(data = df_end, aes(label = country_region), size = 5, min.segment.length = 0)+
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          title = element_text(size = 14))+
    scale_colour_manual(values = country_color)+  
    labs (caption = str_c("\n","By: @behrooz_hm @yurisyt (Monash University) / Data: Johns Hopkins University"),
          x = str_c(xm, xc, xa, "Log Scale",sep = " - "),
          title = str_c ("COVID-19 - Since Number of Confirmed Cases >= ",min_case," @ ", latest_date," AEDT", "\n","(XY-axis are logged)"),
          y = str_c(ym, yc, ya,  "Log Scale", sep = " - "))+
    scale_y_log10(labels = si_vec, breaks = scale_breaks)+
    scale_x_log10(labels = si_vec, breaks = scale_breaks)
  
}

plot_world_lvl <- function(lvl){
  
  lvl_new <- sym(lvl)
  
  continent_pop <- dt %>% 
    distinct(country_region, !!lvl_new, Population) %>% 
    group_by(!!lvl_new) %>% 
    summarise(Population = sum(Population))
   
  df <- dt %>% 
    group_by(!!lvl_new, Date) %>% 
    summarize_at(vars(confirmed_cases, death_cases, recovered_cases), funs(sum)) %>% 
    ungroup() %>% 
    left_join(continent_pop, by = lvl) %>% 
    filter(confirmed_cases >0) %>% 
    mutate_at(vars(confirmed_cases, death_cases, recovered_cases), funs(per_capita=./Population*10000000)) %>% 
    group_by(!!lvl_new) %>% 
    mutate(first_day = min(Date), days = Date - first_day) %>%
    ungroup() 
  
  df_end <- df %>% 
    group_by(!!lvl_new) %>% 
    filter(days == max(days))
  
  df %>% 
    ggplot(aes(x = days, y = confirmed_cases_per_capita, group = !!lvl_new, color = !!lvl_new))+
    geom_point(size = 0.4)+
    geom_line()+
    geom_text_repel(data = df_end, aes(label = !!lvl_new), size = 5, min.segment.length = 0)+
    scale_y_log10(labels = si_vec) +
    scale_x_continuous()+
    theme(legend.position = "none")+
    labs(x = "Days since the First Case Reported from each Country", 
         y = str_c("Confirmed Cases Per Capita (10M)"),
         title = str_c ("COVID-19 Cases Per Capita (10M) by ", lvl, " @ ", latest_date," AEDT", "\n","(Y-axis is logged)"),
         caption = str_c("\n","By: @behrooz_hm @yurisyt (Monash University) / Data: Johns Hopkins University"))
    
}


#### filter data for cumulative and per capita

filtered_day_passed_data <- function(c, a, min_case){
  
  if(identical(a, "Confirmed Cases")){
    aspect <- sym("confirmed_cases")
  }else if(identical(a, "Death Cases")){
    aspect <- sym("death_cases")
  }else{
    aspect <- sym("recovered_cases")
  }
  
    # df <- 
      dt %>% 
        filter(country_region %in% c) %>% 
        rename("cumulative_cases" = aspect) %>% 
        group_by(`country_region`, Date) %>% 
        summarise(cumulative_cases = sum(cumulative_cases)) %>% 
        filter(cumulative_cases >= min_case) %>% ### change the minimum dates
        mutate(first_day = min(Date)) %>% 
        pivot_wider(names_from = "Date", values_from = "cumulative_cases") %>% 
        pivot_longer(cols = starts_with("2020-"), names_to = "Date", values_to = "cumulative_cases") %>% 
        arrange(country_region,Date) %>% 
        mutate(Date = as.Date(Date), passed_days = Date - first_day, 
               incident_cases = ifelse(is.na(lag(cumulative_cases)), cumulative_cases , cumulative_cases - lag(cumulative_cases))) %>% 
        ungroup() %>% 
        left_join(pop, by = c("country_region" = "Country")) %>% 
        filter(passed_days >= 0)

    
}

#### key plot functions for country, state, province
plot_day_passed_day <- function(df, m, a, g, min_case){
  
  if(identical(m, "Cumulative Cases")){
    measure_s <- sym("cumulative_cases")
  }else{
    measure_s <- sym("incident_cases")
  }
  
 if(g == "world"){
    group_var <- sym("country_region")
    xtitle <- "Country"
    cs_color <- country_color
 }else if(g == "other"){
   group_var <- sym("state")
   xtitle <- "State/Province"
   cs_color <- state_color
  }
  
  df_new <- df %>% 
    filter(!is.nan(!!measure_s), !!measure_s>=0, !is.na(!!measure_s))
  
  df_end <- df_new %>%
    group_by(!!group_var) %>% 
    filter(Date == last(Date)) %>% 
    ungroup()
  
  num_of_days <- max(df_new$passed_days) +3
  
  df_new %>% 
    ggplot(aes(x = passed_days, y = !!measure_s, group = !!group_var, color = !!group_var))+
    geom_line()+
    geom_point(size = 0.4)+
    geom_text_repel(data = df_end, aes(label = !!group_var), size = 5, min.segment.length = 0)+
    scale_x_continuous(limits = c(0, num_of_days)) +
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          title = element_text(size = 14))+
    scale_colour_manual(values = cs_color)+  
    labs (caption = str_c("\n","By: @behrooz_hm @yurisyt (Monash University) / Data: Johns Hopkins University"),
          x = str_c("Days since ", m, " >= ", min_case, " from each ", xtitle, sep = ""),
          title = str_c ("COVID-19 ", a, " @ ", latest_date," AEDT", "\n","(Y-axis is logged)"),
          y = "Number of Reported Cases - Log Scale")+
    scale_y_log10(labels = si_vec, breaks = scale_breaks)
  
}

plot_per_capita <- function(df, m, a, g, min_case){
  
  if(identical(m, "Cumulative Cases")){
    measure_s <- sym("cumulative_cases")
  }else{
    measure_s <- sym("incident_cases")
  }
  
 if(g[1] == "world"){
    group_var <- sym("country_region")
    xtitle <- "Country"
    cs_color <- country_color
    df <- df %>% rename("population" = "Population")
  }else{
    group_var <- sym("state")
    xtitle <- "State/Province"
    cs_color <- state_color
  }
  
  df_new <- df %>%
    mutate(`cases_per_capita` = !!measure_s/population*10000000) %>%
    filter(!is.nan(`cases_per_capita`), !is.na(`cases_per_capita`), `cases_per_capita`>=0)
  
  df_end <- df_new %>%
    group_by(!!group_var) %>% 
    filter(Date == last(Date)) %>% 
    ungroup()
  
  num_of_days <- max(df_new$passed_days) +3
  
  
  df_new %>% 
    ggplot(aes(x = passed_days, y = `cases_per_capita`, group = !!group_var, color = !!group_var))+
    geom_line()+
    geom_point(size = 0.4)+
    geom_text_repel(data = df_end, aes(label = !!group_var), size = 5, min.segment.length = 0)+
    scale_x_continuous(limits = c(0, num_of_days)) +
    scale_y_log10(labels = si_vec, breaks = scale_breaks) +
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          title = element_text(size = 14))+
    scale_colour_manual(values = cs_color)+  
    labs (
      #subtitle  = str_c("By: @behrooz_hm @yurisyt"),
      caption = str_c("\n","By: @behrooz_hm @yurisyt (Monash University) / Data: Johns Hopkins University"),
      x = str_c("Days since ", m, " >= ", min_case, " from each ", xtitle, sep = ""),
      title = str_c ("Per Capita COVID-19 ", a, " @ ", latest_date," AEDT", "\n","(Y-axis is logged)"),
      y = "Number of Reported Cases Per 10 Million of Population - Log Scale")
  
  
  
}




###### world growth here 

# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

lm_eqn <- function(df){
  m <- lm(next_day ~ per_capita, df);
  # eq <- substitute(italic(next_day) == a + b %.% italic(per_capita)*","~~italic(r)^2~"="~r2, 
  #                  list(a = format(unname(coef(m)[1]), digits = 2),
  #                       b = format(unname(coef(m)[2]), digits = 2),
  #                       r2 = format(summary(m)$r.squared, digits = 3)))
  
  eq <- str_c("Next Day = ",format(unname(coef(m)[1]), digits = 2), " + ", format(unname(coef(m)[2]), digits = 2)," x Total Last Day ~ ",
              "r^2 = ", format(summary(m)$r.squared, digits = 3), " ")
  
  as.character(eq)
 # as.character(as.expression(eq));
}

#p1 <- p + geom_text(x = 25, y = 300, label = lm_eqn(df), parse = TRUE)



plot_new_cases_growth <- function(df,a, min_case){
  
  df_new <- df %>% 
   # filter(!is.nan(!!measure_s), !!measure_s>=0, !is.na(!!measure_s)) %>% 
    mutate(next_day = lag(incident_cases)/Population*10000000,
           per_capita = cumulative_cases/Population*10000000)  %>% 
    filter(!is.na(next_day))
  
  df_end <- df_new %>%
    group_by(country_region) %>% 
    filter(Date == last(Date)) %>% 
    ungroup()
  
  df_new %>% 
    ggplot(aes(x = per_capita, y = next_day, group = country_region, color = country_region))+
    geom_point(size = 0.4)+
    geom_smooth(method = "lm", aes(group = 1), color = "grey50")+
   # geom_text(x = 15, y = 15, label = lm_eqn(df = df_new), parse = TRUE) +
   # geom_abline(slope = 0.75, linetype = "dashed", alpha = 0.5, intercept = -0.1)+
    geom_text_repel(data = df_end, aes(label = country_region), size = 5, min.segment.length = 0)+
    scale_x_log10(labels = si_vec, breaks = scale_breaks) +
    scale_y_log10(labels = si_vec, breaks = scale_breaks) +
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          title = element_text(size = 14))+
    scale_colour_manual(values = country_color)+  
    labs (caption = str_c("\n","By: @behrooz_hm @yurisyt (Monash University) / Data: Johns Hopkins University"),
          x = "Number of Cumulative Cases Per 10 Million Population - Log Scale",
          title = str_c ("COVID-19 ", a, " (since ", min_case ," Cases) Growth Rate @ ", latest_date," AEDT", "\n",lm_eqn(df = df_new), "(XY-axis is logged)"),
          y = "Number of Reported Cases Next Day Per 10 Million Population - Log Scale")
  
}

# lm_df <- df_new %>% filter(next_day > 0)

# fit <- lm(log10(lm_df$next_day) ~ log10(lm_df$per_capita))
# fit <- lm(df_new$next_day ~ df_new$per_capita)

# test <- data.frame(x = seq(from = 20, to = 50, by = 2)) %>% 
#   mutate(y = lag(x)*0.1) %>% 
#   filter(!is.na(y))
# fit <- lm(test$y ~ test$x)
# fit <- lm(log10(test$y) ~ log10(test$x))

plot_new_case_growth_facet <- function(df,a, min_case){
  
  
  df_new <- df %>% 
  #  filter(!is.nan(!!measure_s), !!measure_s>=0, !is.na(!!measure_s)) %>% 
    mutate(next_day = lag(incident_cases)/Population*10000000,
           per_capita = cumulative_cases/Population*10000000)  %>% 
    filter(!is.na(next_day))
  
  df_end <- df_new %>%
    group_by(country_region) %>% 
    filter(Date == last(Date)) %>% 
    ungroup()
  
  
  df_new %>% 
    ggplot(aes(x = per_capita, y = next_day, group = country_region))+
    geom_point(size = 0.4)+
    geom_smooth( color = "grey50")+
   # geom_abline(intercept = -0.2370, slope = 0.7106, linetype = "dashed", alpha = 0.5, col='tomato3') +
##   geom_abline(intercept = 0, slope = 0.81, linetype = "dashed", alpha = 0.5, col='tomato3') +
   # geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], linetype = "dashed", alpha = 0.5, col='tomato3') +
    scale_x_log10(labels = si_vec, breaks = scale_breaks) +
    scale_y_log10(labels = si_vec, breaks = scale_breaks) +
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_text(size = 13),
          title = element_text(size = 14),
          panel.border = element_blank(),  
          # Remove panel grid lines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Remove panel background
          panel.background = element_blank(),
          strip.text.x = element_text(size = 13))+
    #scale_colour_manual(values = country_color)+  
    labs (caption = str_c("\n","By: @behrooz_hm @yurisyt (Monash University) / Data: Johns Hopkins University"),
          x = "Number of Cumulative Cases Per 10 Million Population - Log Scale",
          title = str_c ("COVID-19 ", a, " (since ", min_case, " Cases) Growth Rate @ ", latest_date," AEDT", "\n"#,"(XY-axis is logged)", " Dashed Red Line Represents 10% Daily Growth."
                         ),
          y = "Number of Reported Cases Next Day Per 10 Million Population - Log Scale") +
    facet_wrap(~ country_region, scale = "free")
  
  
}

##### world recovery here 


plot_perc_rec <- function(c, r, min_case){
  
  df <-  dt %>% 
    filter(country_region %in% c, confirmed_cases > min_case) %>% 
    group_by(country_region, Date, Population) %>% 
    summarise_at(vars(confirmed_cases, death_cases, recovered_cases),
                 funs(sum(., na.rm = TRUE))) %>% 
    ungroup() %>% 
    arrange(country_region, Date) %>% 
    mutate(recovery_rate = ifelse(confirmed_cases == 0, 0, recovered_cases/confirmed_cases),
           death_rate = ifelse(confirmed_cases == 0, 0, death_cases/confirmed_cases),
           cases_per_capita = confirmed_cases/Population *10000000) 
  
  
  df_end <- df %>% 
    group_by(country_region) %>% 
    filter(Date == max(Date))
  
  if(r == "Recovery Percentage"){
    ytitle <- str_c("% Recovery")
    yvar <- sym("recovery_rate")
  }else if (r == "Death Percentage"){
    ytitle <- str_c("% Death")
    yvar <- sym("death_rate")
  }
  
  
  df %>% 
    ggplot(aes(x = cases_per_capita, y = !!yvar, group = country_region, color = country_region))+
    geom_line()+
    geom_point(size = 0.4)+
    geom_text_repel(data = df_end, aes(label = country_region), size = 5, min.segment.length = 0)+
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          title = element_text(size = 14))+
    scale_colour_manual(values = country_color)+  
    labs (caption = str_c("\n","By: @behrooz_hm @yurisyt  (Monash University) / Data: Johns Hopkins University"),
          x = str_c("Cases Per Capita Per 10 Million Population - Log Scale"),
          title = str_c ("COVID-19 - Since Number of Confirmed Cases >= ", min_case," @ ", latest_date," AEDT", "\n","(X-axis is logged)"),
          y = ytitle)+
    scale_y_continuous(labels = percent)+
    scale_x_log10(labels = si_vec, breaks = scale_breaks)
}



############# render data for province_state

filtered_state_data <- function(s, a, g, min_case){
  
## s <- c("Alaska","Arizona","Arkansas","California")
## s <- c("Victoria", "Queensland")
### s <- c("Anhui", "Beijing", "Shanghai", "Hubei")

#  s <- c("Beijing", "Victoria")
  
  if(identical(a, "Confirmed Cases")){
    aspect <- sym("confirmed_cases")
  }else if(identical(a, "Death Cases")){
    aspect <- sym("death_cases")
  }else{
    aspect <- sym("recovered_cases")
  }

  
  # df <- 
  detailed_country_dt %>% 
    rename("cumulative_cases" = aspect) %>% 
    filter(country_region %in% g, state %in% s, cumulative_cases >= min_case) %>% 
    group_by(state, Date, population) %>% 
    summarize(cumulative_cases = sum(cumulative_cases)) %>% 
    ungroup() %>% 
    group_by(state) %>% 
    mutate(first_day = min(Date)) %>% 
    pivot_wider(names_from = "Date", values_from = "cumulative_cases") %>% 
    pivot_longer(cols = starts_with("2020-"), names_to = "Date", values_to = "cumulative_cases") %>% 
    arrange(state,Date) %>% 
    mutate(Date = as.Date(Date), passed_days = Date - first_day, 
           incident_cases = ifelse(is.na(lag(cumulative_cases)), cumulative_cases , cumulative_cases - lag(cumulative_cases))) %>% 
    ungroup() %>% 
    filter(passed_days >= 0)
  
  
}




# find max cumulative_cases for top countries


c25 <- rep (c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", #"gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", #"khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", #"yellow4", "yellow3",
  "darkorange4", "brown"
),9)

names(c25) <- country_sorted_by_cases 
country_color <- c25[1:length(country_sorted_by_cases)]

names(c25) <- c(au_state_sorted_by_cases, ch_state_sorted_by_cases, us_state_sorted_by_cases) 
state_color <- c25[1:length(c(au_state_sorted_by_cases, ch_state_sorted_by_cases, us_state_sorted_by_cases))]



# scales of plots

scale_breaks <- c(0,5,10,25,50,100,250,500,1000,2500,5000, 10000,25000,50000,100000,250000)
# reverselog_trans <- function(base = exp(1)) { 
#   trans <- function(x) -log(x, base) 
#   inv <- function(x) base^(-x) 
#   scales::trans_new(paste0("reverselog-", format(base)), trans, inv, 
#                     scales::log_breaks(base = base), domain = c(1e-100, Inf)) 
# }


### world bank plots from here

plot_health_measure <- function(c, m){
  # m <- "hospital_bed"
  
  measure_s <- sym(m)
  
  if(m == "hospital_bed"){
    ytitle <- "Hospital Beds"
  }else if(m == "physicians"){
    ytitle <- "Physicians"
  }else if(m == "specialists"){
    ytitle <- "Specialists"
  }else if(m == "nurse"){
    ytitle <- "Nurse"
  }
  
  
  
  
  df <- wb_dt %>% 
    filter(Country %in% c) %>% 
    mutate(yvar =  !!measure_s / active_cases)
  
  df_end <- df %>% 
    group_by(Country) %>% 
    filter(Date == max(Date)) %>% 
    ungroup()
  
  num_of_days <- max(df$days) +3
  
  df %>% 
    ggplot(aes(x = days, y = yvar, group = Country, color = Country))+
    geom_line()+
    geom_point(size = 0.4)+
    geom_text_repel(data = df_end, aes(label = Country), size = 5, min.segment.length = 0)+
    scale_x_continuous(limits = c(0, num_of_days)) +
    theme(legend.position = "none",
          axis.text = element_text(size = 9),
          axis.title = element_text(size = 10),
          title = element_text(size = 11))+
    scale_colour_manual(values = country_color)+
    scale_y_log10(labels = si_vec)+
    labs (caption = str_c("\n","By: @behrooz_hm @yurisyt (Monash University) / Data: Johns Hopkins University"),
          x = str_c("Days since First Confirmed Case from each Country", sep = ""),
          title = str_c ("COVID-19 ", ytitle, " per Active Case @ ", latest_date," AEDT", "\n","(Y-axis is logged)"),
          y = str_c("Number of ", ytitle, " per Active Case"))
  
}

plot_death_crude <- function(c, m){
  
  if(m == "new"){
    measure_s <- sym("daily_death_rate")
    ytitle <- "Daily Death Cases / Average Daily Crude Deaths"
  }else if(m == "cumulative"){
    measure_s <- sym("annual_death_rate")
    ytitle <- "Cumulative Death Cases / Average Daily Crude Deaths"
  }
  
  df <- wb_dt %>% 
    filter(Country %in% c) %>% 
    mutate(daily_death_cases = ifelse(is.na(lag(death_cases)), death_cases, death_cases - lag(death_cases))) %>% 
    mutate(daily_death_rate =  daily_death_cases / (death_crude/365),
           annual_death_rate = death_cases / (death_crude / 365))
  
  df_end <- df %>% 
    group_by(Country) %>% 
    filter(Date == max(Date)) %>% 
    ungroup()
  
  num_of_days <- max(df$days) +3
  
  df %>% 
    ggplot(aes(x = days, y = !!measure_s, group = Country, color = Country))+
    geom_line()+
    geom_point(size = 0.4)+
    geom_text_repel(data = df_end, aes(label = Country), size = 5, min.segment.length = 0)+
    scale_x_continuous(limits = c(0, num_of_days)) +
    theme(legend.position = "none",
          axis.text = element_text(size = 9),
          axis.title = element_text(size = 10),
          title = element_text(size = 11))+
    scale_colour_manual(values = country_color)+
    scale_y_log10(labels = comma_format(accuracy = .01), breaks = c(0.01, 0.05, 0.1, 0.5, 1, 2,4))+
    #  scale_y_log10(labels = si_vec, reverse = TRUE)+  
    labs (caption = str_c("\n","By: @behrooz_hm @yurisyt (Monash University) / Data: Johns Hopkins University"),
          x = str_c("Days since First Confirmed Case from each Country", sep = ""),
          title = str_c ("COVID-19 ", ytitle, "\n","@ ", latest_date," AEDT", " (Y-axis is logged)"),
          y = ytitle)
  
}


### country profile

plot_cp_day <- function(c, a){
  
  if(identical(a, "Confirmed Cases")){
    aspect <- sym("confirmed_cases")
  }else if(identical(a, "Death Cases")){
    aspect <- sym("death_cases")
  }else{
    aspect <- sym("recovered_cases")
  }
  
  
  earliest_date <- wb_dt %>% 
    filter(Country %in% c, confirmed_cases> 9) 
  
  df <- wb_dt %>% 
    filter(Country %in% c, Date >= min(earliest_date$Date), confirmed_cases > 9
           ) %>% 
    select(Country, `Alpha-3 code`, Date, confirmed_cases:active_cases) %>% 
    rename("cumulative_cases" = aspect) %>% 
    group_by(Country) %>% 
    mutate(incident_cases = ifelse(is.na(lag(cumulative_cases)), cumulative_cases , cumulative_cases - lag(cumulative_cases))) %>% 
    ungroup() %>% 
    select(Country, `Alpha-3 code`, Date, incident_cases)
  

  
  label_dt <- oxford_dt %>% 
    filter(CountryCode %in% unique(df$`Alpha-3 code`), !is.na(vari),
           !is.na(value), value >0) %>% 
    group_by(Country, CountryCode, vari, value) %>% 
    slice(1) %>% 
    mutate(text = str_c(vari, " ", value)) %>% 
    ungroup() %>% 
    group_by(CountryCode, Date) %>% 
    summarise(text = paste(text, collapse = "\n")) %>% 
    ungroup() %>% 
    right_join(df, by = c("CountryCode" = "Alpha-3 code", "Date"))
  
  label_dt %>% 
    ggplot(aes(x = Date, y = incident_cases, group = Country, label = text))+
    geom_col(fill = "grey60", color = "white", alpha = 0.7)+
    geom_text_repel(min.segment.length = 0, size = 4,
                   force = 1, #point.padding = unit(1, 'lines'),
                   vjust = 1, direction = 'y', nudge_y =  1)+
    scale_y_continuous(labels = si_vec) +
    facet_wrap(~Country, ncol = 1, scales = "free_y")+
    labs(x = "", y = str_c("Number of Daily ", a),
         title = str_c("COVID-19 Daily ", a, " @ ", latest_date, " AEDT"),
         caption = str_c("By: @behrooz_hm @yurisyt (Monash University) / Data: Johns Hopkins University & Oxford University"))+
    theme(legend.position = "none",
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 12),
          title = element_text(size = 13))
  

}

plot_death_con <- function(c){
  
 df1 <-  wb_dt %>% 
   rename("COVID19 Death" = "death_cases") %>% 
    filter(Country %in% c, Date == max(Date)) %>% 
    select(Country, `Alpha-3 code`, population, contains("deaths"), Date, `COVID19 Death`) %>%
    pivot_longer(cols = contains("death"), names_to = "Death Condition", values_to = "Value") %>% 
   mutate(value = si_vec(round(Value,0)),
            #format(as.integer(Value), big.mark = ","), 
          `Death Condition` = str_remove(`Death Condition`, pattern = " \\(deaths\\)")) 
 
 rbind(df1 %>% filter(`Death Condition` == "COVID19 Death"),
       df1 %>% group_by(Country) %>% top_n(n = 10, wt = Value) %>% ungroup()) %>% 
   distinct() %>% 
   arrange(Country, desc(Value)) %>% 
   group_by(Country) %>% 
   ggplot(aes(x = fct_reorder(`Death Condition`, Value), y = Value, group = Country,
              fill = factor(ifelse(`Death Condition` == "COVID19 Death", "focused", "other"))))+
   geom_col(color = "white", alpha = 0.7) +
   geom_text(aes(label =value, y = 10,
                 color = factor(ifelse(`Death Condition` == "COVID19 Death", "focused", "other"))),
            hjust = 0
             )+
   scale_y_continuous(labels = si_vec) +
   scale_fill_manual(name = "Focus", values = c("tomato3", "grey60"))+
   scale_color_manual(name = "Focus", values = c("firebrick4", "black"))+
   facet_wrap(~Country, ncol = 2, scales = "free_x")+
   labs(x = "", y = "", #str_c("Number of Weekly Death"),
        title = str_c ("Top 10 Causes of Death and COVID-19 Death Cases" ," @ ", latest_date, " AEDT"),
        caption = str_c("By: @behrooz_hm @yurisyt (Monash University) / Data: Johns Hopkins University & Oxford University"))+
   theme(legend.position = "none",
        # axis.text.x = element_text(angle = 90),
         axis.text = element_text(size = 11),
         axis.text.x = element_blank(),
         axis.title = element_text(size = 12),
         title = element_text(size = 13)) +
   coord_flip()
  
}

plot_cp_radar <- function(c){

    plt_radar_dt <- radar_dt %>%
      group_by(Country) %>%
      pivot_wider(names_from = "Plot.title", values_from = "value") %>%
      ungroup() %>%
      column_to_rownames(var = "Country") %>%
      rbind(rep(1,11), rep(0,11), .) %>%
      .[c(1,2,c),] %>% 
      select(-`\nCOVID19\nDeath Rate`)

    radarchart(plt_radar_dt,
               #custom polygon
               pcol=country_color , #pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,
               
               #custom the grid
               cglcol="grey80", cglty=1, axislabcol="grey80", caxislabels=seq(0,1,0.2),
               cglwd=0.8,
               
               #custom labels
              vlcex=0.8,
              title= str_c("Country Readiness")
    )
    
if(length(c) == 1){}else{
  legend(x=1.4, y=1, legend = rownames(plt_radar_dt[-c(1,2),]), bty = "n", pch=20 , col=country_color , text.col = "grey", cex=1.2, pt.cex=3)
  
}    
    


}




