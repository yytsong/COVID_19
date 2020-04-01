# load data in


# load population from wikipedia

# wikipedia_data_url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
# 
# outbreak_webpage <- read_html(wikipedia_data_url)
# 
# # parse the web page and extract the data from the first
# # table
# provinces_confirmed <- outbreak_webpage %>% html_nodes("table") %>% 
#   .[[1]] %>% html_table(fill = TRUE) %>% rename(Date = "Date (CST)")


# wp_region <- fread("data/reg_country.csv") %>% 
#   rename("Global" = "Global South")


#### region <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv")

wp_region <- read_excel("data/reg_country_wiki.xlsx")

pop <- fread("data/world_pop.csv") %>% 
  mutate(Country = str_remove(Country, pattern = "\xca"),
         Population = as.numeric(Population)) %>% 
  mutate(Country = str_remove(Country, '\\[.\\]$')) %>%
  mutate(Country = case_when(
    Country == "Mainland China" ~ "China",
    Country == "Czech Republic" ~ "Czechia",
    TRUE ~ Country
  )) %>% 
  rename("pop_date"="Date") %>% 
  select(-pop_date) %>% 
  left_join(wp_region, by = c("Country"))


# setdiff(unique(wp_region$Country),unique(pop$Country))
# setdiff(unique(pop$Country), unique(wp_region$Country))


# # # data collected by John Hopkins University

### github projec from JHU https://github.com/CSSEGISandData/COVID-19
jhu_url_confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
jhu_url_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
jhu_url_recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"


# jhu_url_recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

dt_death <- read_csv(jhu_url_deaths) %>% 
  rename(province_state = "Province/State", country_region = "Country/Region") %>% 
  pivot_longer(-c(province_state,  country_region, Lat, Long), names_to = "Date", values_to = "death_cases") %>% 
  # adjust JHU dates back one day to reflect US time, more orless
  mutate(Date = mdy(Date),province_state = ifelse(is.na(province_state), country_region, province_state),
         country_region = case_when(
           country_region == "Hong Kong SAR" ~ "Hong Kong",
           country_region == "Iran (Islamic Republic of)" ~ "Iran",
           country_region %in% c("Korea, South", "Republic of Korea") ~ "South Korea",
           country_region == "Macao SAR" ~ "Macau",
           country_region == "Russian Federation" ~ "Russia",
           country_region == "Taipei and environs" ~ "Taiwan",
           country_region == "Viet Nam" ~ "Vietnam",
           country_region == "Republic of Moldova" ~ "Moldova",
           country_region == "United Kingdom" ~ "UK",
           country_region == "Congo (Kinshasa)" ~ "Congo",
           country_region == "Taiwan*" ~ "Taiwan",
           country_region == "China" ~ "China",
           TRUE ~ country_region
         )) %>% 
  arrange(country_region, province_state, as.character(Date)) %>% 
  as.data.table()



dt_recovered <- read_csv(jhu_url_recovered) %>% 
  rename(province_state = "Province/State", country_region = "Country/Region") %>% 
  pivot_longer(-c(province_state,  country_region, Lat, Long), names_to = "Date", values_to = "recovered_cases") %>% 
  # adjust JHU dates back one day to reflect US time, more orless
  mutate(Date = mdy(Date),province_state = ifelse(is.na(province_state), country_region, province_state),
         country_region = case_when(
           country_region == "Hong Kong SAR" ~ "Hong Kong",
           country_region == "Iran (Islamic Republic of)" ~ "Iran",
           country_region %in% c("Korea, South", "Republic of Korea") ~ "South Korea",
           country_region == "Macao SAR" ~ "Macau",
           country_region == "Russian Federation" ~ "Russia",
           country_region == "Taipei and environs" ~ "Taiwan",
           country_region == "Viet Nam" ~ "Vietnam",
           country_region == "Republic of Moldova" ~ "Moldova",
           country_region == "United Kingdom" ~ "UK",
           country_region == "Congo (Kinshasa)" ~ "Congo",
           country_region == "Taiwan*" ~ "Taiwan",
           country_region == "China" ~ "China",
           TRUE ~ country_region
         )) %>% 
  arrange(country_region, province_state, as.character(Date)) %>% 
  as.data.table()


recovery_latest_day <- dt_recovered$Date %>% max()

# dt_recovered <- read.csv(jhu_url_recovered, header = TRUE) %>% 
#   mutate(recovered_cases =as.double(recovered_cases)) %>% 
#   rename(province_state = "Province.State", country_region = "Country.Region") %>%
#   mutate_if(is.factor, as.character) %>% 
#   pivot_longer(-c(province_state,  country_region, Lat, Long), names_to = "Date", values_to = "recovered_cases") %>%
#   mutate(Date = str_remove(Date, "X")) %>% 
#   mutate(Date = str_replace(Date, pattern = "\\.", replacement = "\\-")) %>% 
#   mutate(Date = str_replace(Date, pattern = "\\.", replacement = "-20")) %>% 
#   mutate(Date = as.Date(Date, "%m-%d-%Y")) %>% 
#   mutate(#Date = mdy(Date),
#          province_state = ifelse(province_state == '', country_region, province_state),
#          country_region = case_when(
#            country_region == "Hong Kong SAR" ~ "Hong Kong",
#            country_region == "Iran (Islamic Republic of)" ~ "Iran",
#            country_region %in% c("Korea, South", "Republic of Korea") ~ "South Korea",
#            country_region == "Macao SAR" ~ "Macau",
#            country_region == "Russian Federation" ~ "Russia",
#            country_region == "Taipei and environs" ~ "Taiwan",
#            country_region == "Viet Nam" ~ "Vietnam",
#            country_region == "Republic of Moldova" ~ "Moldova",
#            country_region == "United Kingdom" ~ "UK",
#            country_region == "Congo (Kinshasa)" ~ "Congo",
#            country_region == "Taiwan*" ~ "Taiwan",
#            country_region == "China" ~ "China",
#            TRUE ~ country_region
#          )) %>% 
#   arrange(country_region, province_state, as.character(Date)) %>% 
#   as.data.table()
  

# setdiff(unique(dt_recovered$country_region), unique(dt$country_region) )
# setdiff(unique(dt$country_region),unique(dt_recovered$country_region) )

dt <- read_csv(jhu_url_confirmed) %>%
  rename(province_state = "Province/State", country_region = "Country/Region") %>%
  pivot_longer(-c(province_state,  country_region, Lat, Long), names_to = "Date", values_to = "confirmed_cases") %>%
  # adjust JHU dates back one day to reflect US time, more orless
  mutate(Date = mdy(Date),
         province_state = ifelse(is.na(province_state), country_region, province_state),
         country_region = case_when(
           country_region == "Hong Kong SAR" ~ "Hong Kong",
           country_region == "Iran (Islamic Republic of)" ~ "Iran",
           country_region %in% c("Korea, South", "Republic of Korea") ~ "South Korea",
           country_region == "Macao SAR" ~ "Macau",
           country_region == "Russian Federation" ~ "Russia",
           country_region == "Taipei and environs" ~ "Taiwan",
           country_region == "Viet Nam" ~ "Vietnam",
           country_region == "Republic of Moldova" ~ "Moldova",
           country_region == "United Kingdom" ~ "UK",
           country_region == "Congo (Kinshasa)" ~ "Congo",
           country_region == "Taiwan*" ~ "Taiwan",
           country_region == "China" ~ "China",
           TRUE ~ country_region
         )) %>%
  arrange(country_region, province_state, as.character(Date)) %>%
  left_join(pop, by =c("country_region" = "Country")) %>%
  filter(!is.na(Population)) %>%
  as.data.table() %>% 
  left_join(dt_death,by = c("province_state", "country_region", "Lat", "Long", "Date")) %>% 
  left_join(dt_recovered,by = c("province_state", "country_region", "Lat", "Long", "Date"))



latest_date <- max(dt$Date)



# map_dt <- dt %>% 
#   select(province_state, country_region, Lat, Long, Date, Population,cumulative_cases)  %>% 
#   filter(cumulative_cases != 0) %>% 
#   group_by(province_state) %>% 
#   mutate(first_day = min(Date), passed_days = Date - first_day,
#          incident_cases = ifelse(is.na(lag(cumulative_cases)), cumulative_cases , cumulative_cases - lag(cumulative_cases)),
#          per_capita = cumulative_cases/Population*10000000, cumulative_cases_log=log(cumulative_cases)) %>% 
#   
#   ungroup()


### this is original us_dt from JHU
us_pop <- fread("data/us_pop.csv") %>% select(state, population)
# 
# us_dt <- 
#   dt%>% 
#   filter(country_region == "US") %>% 
#   separate(province_state, c("province", "state_ini"), ", ") %>% 
#   left_join(us_pop[,c("state_ini","state")], by = c("state_ini" = "state_ini")) %>% 
#   mutate(state= ifelse(is.na(state), province, state)) %>% 
#   left_join(us_pop[,c("state", "population")], by = "state") %>% 
#   select(country_region, state, population, Date, confirmed_cases,  death_cases, recovered_cases) 


# ### this is from NYtimes
# 
# us_dt <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")%>%
#   rename("confirmed_cases" = "cases", "death_cases" = "deaths", "Date" = "date") %>%
#   left_join(us_pop, by = "state") %>%
#   mutate(country_region = "US", recovered_cases = NA) %>%
#   select(country_region, state, population, Date, confirmed_cases, death_cases, recovered_cases) %>%
#   arrange(state, Date)

#### this is from JHU updated us county 
us_county_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
  group_by(Province_State) %>% 
  summarise_at(vars(ends_with("/2020")), funs(sum)) %>% 
  pivot_longer(cols = ends_with("/2020"), names_to = "Date", values_to = "confirmed_cases") %>% 
  select(Province_State,Date, confirmed_cases)%>% 
  filter(Province_State != "Recovered") %>% 
  mutate(Date = mdy(Date)) 

us_county_death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>% 
  group_by(Province_State) %>% 
  summarise_at(vars(ends_with("/2020")), funs(sum)) %>% 
  pivot_longer(cols = ends_with("/2020"), names_to = "Date", values_to = "death_cases") %>% 
  select(Province_State,Date, death_cases)%>% 
  filter(Province_State != "Recovered") %>% 
  mutate(Date = mdy(Date)) 

us_dt <- us_county_confirmed %>% 
  left_join(us_county_death, by = c("Province_State", "Date")) %>% 
  rename("state"="Province_State") %>% 
  left_join(us_pop, by = "state") %>% 
  mutate(recovered_cases = NA, country_region = "US")%>% 
  select(country_region, state, population, Date, confirmed_cases, death_cases, recovered_cases) %>% 
  arrange(state, Date)


au_pop <- fread("data/aus_pop.csv") 

au_dt <- 
  dt %>% 
  filter(country_region == "Australia") %>% 
  left_join(au_pop, by = c("province_state" = "state")) %>%
  rename("state" = "province_state") %>% 
  select(country_region, state, population, Date, confirmed_cases,  death_cases, recovered_cases) %>% 
  arrange(state, Date)



ch_pop <- fread("data/chn_pop.csv") %>% 
  mutate(Province = str_remove(Province, pattern = " Province| Municipality| Autonomous Region| Special Administrative Region"),
         Population = str_remove_all(Population, pattern = ","),
         Density = str_remove_all(Density, pattern = ",")) %>% 
  mutate(Province = 
           case_when(
             Province == "Guangxi Zhuang" ~ "Guangxi",
             Province == "Xinjiang Uyghur" ~ "Xinjiang",
             Province == "Ningxia Hui" ~ "Ningxia",
             TRUE ~ Province
           ),
         Population = as.integer(Population),
         Density = as.numeric(Density)) %>% 
  rename("population" = "Population") 

ch_dt <- 
  dt %>% 
  filter(country_region == "China") %>% 
  select(-Population) %>% 
  left_join(ch_pop, by = c("province_state" = "Province")) %>% 
  rename("state" = "province_state")  %>% 
  select(country_region, state, population, Date, confirmed_cases,  death_cases, recovered_cases) %>% 
  arrange(state, Date)



detailed_country_dt <- rbind(au_dt, ch_dt, us_dt)

country_state_list <- detailed_country_dt %>% 
  distinct(country_region, state)


combination <- crossing(measure = c("Cumulative Cases", "New Cases"),
                   aspect = c("Confirmed Cases", "Recovered Cases", "Death Cases"),
                   actual = c("Actual", "Per Capita (10 Million)")) %>% 
              mutate(id = 1:n())


region_pop <- dt %>% 
  distinct(country_region, Region, Population) %>% 
  group_by(Region) %>% 
  summarise(Population = sum(Population))


### region continent country


region_country_list <- dt %>% 
  distinct(Continent, Region, country_region) %>% 
  mutate(Continent = fct_relevel(Continent, "Oceania", after = Inf)) %>% 
  arrange(Continent,Region, country_region) %>% 
  mutate(Continent = as.character(Continent))


rc_list <- region_country_list %>% 
  distinct(Continent, Region) %>% 
  group_by(Continent) %>% 
  mutate(Region = list(Region)) %>% 
  distinct()


rc_dictionary <- setNames(rc_list$Region, rc_list$Continent)




# setdiff(dt$country_region,wb_dt$Country )
#  setdiff(wb_dt$Country,dt$country_region )

dt_country <- dt %>% 
  group_by(Continent, country_region, Date) %>% 
  summarise_at(vars(confirmed_cases, death_cases, recovered_cases), ~ sum(., na.rm = TRUE))

wb_dt <- fread("data/wb_data_2020-04-02.csv") %>% 
  right_join(dt_country, by = c("Country" = "country_region"))  %>% 
  filter(confirmed_cases > 0) %>% 
  group_by(Country) %>% 
  mutate(first_day = min(Date), days = Date - first_day + 1) %>% 
  ungroup() %>% 
  mutate(active_cases = confirmed_cases - death_cases - recovered_cases) %>% 
  mutate_if(is.logical, as.numeric)

# wb_dt %>% write_csv(str_c("other/behrooz_wb_dt_", Sys.Date(),".csv"))  




