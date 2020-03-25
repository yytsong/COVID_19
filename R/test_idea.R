# test 

dt %>% 
  left_join(wb_dt, by = c("country_region" = "Country")) %>% 
  head()
