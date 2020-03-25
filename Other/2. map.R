# try map function


  
  plt <- ggplot()+
    borders("world", colour="gray50", fill="gray90") +
    geom_point(data = map_dt, alpha = 0.5, aes(x = Long, y = Lat, size = incident_cases,
                                  color = country_region)) +
    scale_color_manual(values = country_color, guide = FALSE) +
    scale_size_continuous(name = "Number of Cases", labels = si_vec)+
    theme(legend.position = "bottom")+
    transition_time(Date) +
    labs(x = "Longitude", y = "Latitude", title = "Date: {frame_time}") 
    
  
  animate(plt, nframe = 50, fps = 5, height = 461, width = 644)
  

  # new dt with recovered and deaths
  
  
  color <- 
  
  dt %>% 
    group_by(country_region, Date, Population) %>% 
    summarise(confirmed_cases = sum(confirmed_cases),
              death_cases = sum(death_cases),
              recovered_cases = sum(recovered_cases)) %>% 
    ungroup() %>% 
    pivot_longer(cols = c(confirmed_cases, death_cases, recovered_cases),
                 names_to = "measure", values_to = "value") %>%
    mutate(.,id = group_indices(.,country_region)) %>% 
    ungroup() %>% 
    group_by(id, Date, measure) %>% 
    summarize(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(Day = Date - min(Date)) %>% 
    mutate(Day = as.numeric(Day)) %>% 
    as.data.table() %>% 
    ggplot(aes(x = Day, stratum = id, alluvium = id, fill = measure))+
    geom_stratum(color = "white")+
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          title = element_text(size = 17))+
    labs (caption = str_c("\n","By: @behrooz_hm @yurisyt / Data: Tim Churches (UNSW) & Johns Hopkins University"),
          x = "Days since the First Case Reported from each Province",
          title = str_c ("COVID-19 ", a, " @ ", Sys.time()," AEDT", "\n","(Y-axis is logged)"),
          y = "Number of Reported Cases - Log Scale")+
    scale_y_log10(labels = si_vec, breaks = scale_breaks)
  
  
 df <-  dt %>% 
   filter(country_region %in% c("China", "Australia", "US", "Italy")) %>% 
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
 
 df %>% 
    ggplot(aes(x = cases_per_capita, y = recovery_rate, group = country_region, color = country_region))+
    geom_line()+
    geom_point(size = 0.4)+
    geom_text_repel(data = df_end, aes(label = country_region), size = 4, min.segment.length = 0)+
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 13),
          title = element_text(size = 14))+
    scale_colour_manual(values = country_color)+  
    labs (caption = str_c("\n","By: @behrooz_hm @yurisyt / Data: Tim Churches (UNSW) & Johns Hopkins University"),
          x = str_c("Cases Per Capita Per 10 Million Population - Log Scale"),
          title = str_c ("COVID-19 Cases @ ", Sys.time()," AEDT", "\n","(X-axis is logged)"),
          y = str_c("% Recovery Rate - Log Scale"))+
    scale_y_continuous(labels = percent)+
    scale_x_log10(labels = si_vec, breaks = scale_breaks)
  
  
  
  