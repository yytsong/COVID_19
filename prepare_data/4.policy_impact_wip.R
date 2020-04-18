suppressPackageStartupMessages({
  library(tidyverse)
  library(tidycovid19)
  library(lubridate)
  theme_set(theme_minimal())
  
})



# load in country code
country_code <- read_excel("prepare_data/country_code.xlsx") %>% 
  select(Country, `Alpha-3 code`, `Alpha-2 code`)


## source Apple mobility https://www.apple.com/covid19/mobility
# url is required to be udpated when apple website update the data
url <- str_c("https://covid19-static.cdn-apple.com/covid19-mobility-data/2005HotfixDev14/v1/en-us/applemobilitytrends-2020-04-14.csv")
apple_mobility <- read_csv(url) %>% 
  pivot_longer(cols = starts_with("2020-"), names_to = "date", values_to = "value") %>% 
  filter(geo_type == "country/region") %>% 
  select(-geo_type) %>% 
  mutate(region = case_when(
    region == "Czech Republic" ~ "Czechia",
    region == "Republic of Korea" ~ "South Korea",
    region == "United States" ~ "US",
    TRUE ~ region),
    date = as.Date(date)) %>% 
  left_join(country_code, by = c("region" = "Country")) %>% 
  rename("Country" = "region", "measure" = "transportation_type") %>% 
  select(Country, date, measure, value)

## setdiff(apple_mobility$region %>% unique(), country_code$Country)


cc <- c("New Zealand", "Canada", #"UK", #"US", "Italy", 
  "Australia"#, "South Korea", "Germany"
)

## get policy data in

acaps_dt <- fread("data/acaps_policy.csv") %>% 
  select(-country) %>% 
  mutate(date_implemented = as.Date(date_implemented))



label_dt <- acaps_dt %>% 
  mutate(cat = str_sub(category_final, start = 1L, end = 2L)) %>% 
  filter(cat == "SD") %>% 
  group_by(iso3c, date_implemented) %>% 
  summarise(text = paste(sort(unique(cat)), collapse = "\n")) %>% 
  ungroup() %>% 
  left_join(country_code, by = c("iso3c" = "Alpha-3 code")) %>% 
  group_by(Country) %>% 
  # mutate(mark = ifelse(str_detect(text, pattern = "LD"), "lockdown", NA)) %>% 
  ungroup() %>% 
  mutate(text = ifelse(text == "", NA, text)) #%>% 
# na.omit()



# join_1 <- acaps_dt %>% 
#   right_join(apple_mobility, by = c("iso3c" = "Alpha-3 code", "date_implemented" = "date"))
# 
# 
# 
# 
# 
# 
# apple_mobility %>% 
#   filter(Country %in% cc) %>% 
#   ggplot(aes(x = date, y = value, group = interaction(Country, measure), color = measure)) +
#   geom_line() +
#   geom_text_repel(data = label_dt, aes(x = date_implemented, y = 0, group = Country, color = NA, label = text), 
#                   size = 2, color = "black",
#                   min.segment.length = 0, segment.color = "grey80", 
#                   force = 1, #point.padding = unit(1, 'lines'),
#                   vjust = 1, direction = 'y', nudge_y =  1)+
#   facet_wrap(~Country)



## google community mobility report
## source https://osf.io/rzd8k/files/

url <- "https://osf.io/besh8/download"
cmr_dt <- read_csv(url) %>% 
  mutate(iso2c = toupper(iso2c), date = dmy(date)) %>% 
  select(-country) %>% 
  left_join(country_code, by = c("iso2c" = "Alpha-2 code")) %>% 
  select(-c(starts_with("not_enough_data"))) %>% 
  pivot_longer(cols = starts_with("y"), names_to = "measure", values_to = "value") %>% 
  mutate(measure = str_remove(measure, pattern = "y"),
         value = value +100) %>% # change scale to match apple google map 
  select(Country, date, measure, value)


# cmr_dt %>% 
#   filter(Country %in% cc) %>% 
#   ggplot(aes(x = date, y = value, group = interaction(Country, measure), color = measure))+
#   geom_line() +
#   geom_hline(yintercept = 0, linetype = "dotted", color = "grey60")+
#   geom_text_repel(data = label_dt, aes(x = date_implemented, y = 0, group = Country, color = NA, label = text), 
#                   size = 2, color = "black",
#                   min.segment.length = 0, segment.color = "grey80", 
#                   force = 1, #point.padding = unit(1, 'lines'),
#                   vjust = 1, direction = 'y', nudge_y =  1)+
#   scale_y_continuous(limits = c(-100,100))
#   facet_wrap(~Country)


  ## join apple_mobility and cmr_dt
  
  join_2 <- rbind(apple_mobility, cmr_dt) %>% 
    arrange(Country, date, measure) %>% 
    rename("Date" = "date")
  
  
  join_2 %>% 
    filter(Country %in% cc) %>% 
    ggplot(aes(x = Date, y = value, group = interaction(Country,measure), color = measure)) +
    geom_line() +
    geom_text_repel(data = join_2 %>% filter(Country %in% cc) %>% group_by(measure, Country) %>% filter(Date == max(Date)),
                    aes(label = measure), size = 3) +
    # change to label_Dt
    geom_text_repel(data = label_dt %>% filter(Country %in% cc), 
                    aes(x = date_implemented, y = 0, group = Country, color = NA, label = text), 
                    size = 2, color = "black",
                    min.segment.length = 0, segment.color = "grey80", 
                    force = 1, #point.padding = unit(1, 'lines'),
                    vjust = 1, direction = 'y', nudge_y =  1)+
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey60")+
    scale_y_continuous(breaks = seq(from = -200, to = 200, by = 20))+
    facet_wrap(~Country) +
    theme(legend.position = "none")
  
  
  
  

  
  ## load dt
  dt <- fread("data/dt.csv") %>%  
    mutate(Date = ymd(Date)) %>% 
    group_by(country_region, Date) %>% 
    summarise_at(vars(confirmed_cases, death_cases, recovered_cases), funs(sum)) %>% 
    ungroup() %>% 
    group_by(country_region) %>% 
    pivot_longer(cols = c("confirmed_cases", "death_cases", "recovered_cases"), names_to = "measure", values_to = "value") %>% 
    ungroup() %>% 
    rename("Country" = "country_region") %>% 
    arrange(Country, measure, Date) %>% 
    group_by(Country, measure) %>% 
    mutate(new_value = ifelse(is.na(lag(value)), 0, value - lag(value))) %>% 
    ungroup() %>% 
    select(-value) %>% 
    rename("value" = "new_value")
  
  
  join_3 <- rbind(join_2, dt) %>% 
    write_csv("prepare_data/join_policy_mobility_community_movement.csv")
  

  join_3 %>% 
    filter(Country %in% cc) %>% 
    ggplot(aes(x = Date, y = value, group = interaction(Country,measure), color = Country)) +
    geom_line() +
    geom_text_repel(data = join_2 %>% filter(Country %in% cc) %>% group_by(measure, Country) %>% filter(Date == max(Date)),
                    aes(label = Country), size = 3) +
    ## change to label_Dt
   geom_text_repel(data = label_dt %>% filter(Country %in% cc),
                   aes(x = date_implemented, y = 0, group = Country, color = Country, label = Country),
                   size = 3, 
                   min.segment.length = 0, segment.color = "grey80",
                   force = 1, #point.padding = unit(1, 'lines'),
                   vjust = 1, direction = 'y', nudge_y =  1)+
   geom_hline(yintercept = 0, linetype = "dotted", color = "grey60")+
   scale_y_continuous(labels = si_vec)+
    facet_wrap(~measure, scale = "free_y") +
    theme(legend.position = "none")
  






