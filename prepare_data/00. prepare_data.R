### this is to process data


wp_region <- read_excel("prepare_data/reg_country_wiki.xlsx")

pop <- fread("prepare_data/world_pop.csv") %>% 
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
  left_join(wp_region, by = c("Country")) %>% 
  write_csv("data/pop.csv")




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
  as.data.table() #%>% write_csv("prepare_data/death.csv")

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
  as.data.table() #%>% write_csv("prepare_data/recovered.csv")


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
  arrange(country_region, province_state, Date) %>%
  left_join(pop, by =c("country_region" = "Country")) %>%
  filter(!is.na(Population)) %>%
  as.data.table() %>% 
  left_join(dt_death,by = c("province_state", "country_region", "Lat", "Long", "Date")) %>% 
  left_join(dt_recovered,by = c("province_state", "country_region", "Lat", "Long", "Date")) %>% 
  mutate(Date = as.Date(Date)) %>% 
  write_csv("data/dt.csv")

### this is original us_dt from JHU
us_pop <- fread("prepare_data/us_pop.csv") %>% select(state, population)

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
  summarise_at(vars(ends_with("/20")), funs(sum)) %>% 
  pivot_longer(cols = ends_with("/20"), names_to = "Date", values_to = "confirmed_cases") %>% 
  select(Province_State,Date, confirmed_cases)%>% 
  filter(Province_State != "Recovered") %>% 
  mutate(Date = mdy(Date)) 

us_county_death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>% 
  group_by(Province_State) %>% 
  summarise_at(vars(ends_with("/20")), funs(sum)) %>% 
  pivot_longer(cols = ends_with("/20"), names_to = "Date", values_to = "death_cases") %>% 
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


au_pop <- fread("prepare_data/aus_pop.csv") 

au_dt <- 
  dt %>% 
  filter(country_region == "Australia") %>% 
  left_join(au_pop, by = c("province_state" = "state")) %>%
  rename("state" = "province_state") %>% 
  select(country_region, state, population, Date, confirmed_cases,  death_cases, recovered_cases) %>% 
  arrange(state, Date)



ch_pop <- fread("prepare_data/chn_pop.csv") %>% 
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


detailed_country_dt <- rbind(au_dt, ch_dt, us_dt) %>% 
  write_csv("data/detailed_country_dt.csv")


# ### source https://www.r-bloggers.com/meet-tidycovid19-yet-another-covid-19-related-r-package/
# ### connect to https://www.acaps.org/covid19-government-measures-dataset
library(tidycovid19)

acaps_npi <- download_acaps_npi_data() %>% 
  mutate(date_implemented = as.Date(date_implemented)) %>% 
  mutate(date_implemented = as.character(date_implemented)) %>% 
  select(country, iso3c, category, measure, targeted_pop_group, comments, date_implemented, source, source_type, link) 



category_npi <- acaps_npi %>%
  distinct(category, measure) %>%
  arrange(category, measure) %>%
  mutate(category_final = case_when(
    category == "Humanitarian exemption" ~ "HE - Humanitarian exemption",
    category == "Lockdown" ~ "LD - Lockdown",
    category == "Movement restrictions" ~ "MR - Movement restrictions",
    category == "Public health measures" ~ "PH - Public health measures",
    category == "Social and economic measures" ~ "SE - Social and economic measures",
    category == "Social distancing" ~ "SD - Social distancing")) %>% 
  mutate(code = str_sub(category_final, start = 1L, end = 2L)) %>% 
  group_by(category) %>% 
  mutate(code = str_c(code, 1:n())) %>% 
  mutate(measure_final = str_c(code, measure, sep = " - "))



acaps_npi %>%
  left_join(category_npi, by = c("category", "measure")) %>% 
  select(-c(category, measure)) %>% 
  write_csv("data/acaps_policy.csv")





oxford_dt <- read_excel("prepare_data/OxCGRT_Download_latest_data.xlsx") %>% 
  select(-contains(c("Notes", "IsGeneral", "StringencyIndex", "...35", "Confirmed"))) %>% 
  pivot_longer(cols = starts_with("S"), names_to = "variable", values_to = "value") %>% 
  mutate(vari = case_when(
    variable == "S1_School closing" ~ "Sch",
    variable == "S2_Workplace closing" ~ "Wrk",
    variable == "S3_Cancel public events" ~ "PEv",
    variable == "S4_Close public transport" ~ "PTran",
    variable == "S5_Public information campaigns" ~ "PInfo",
    variable == "S6_Restrictions on internal movement" ~ "IMov",
    variable == "S7_International travel controls" ~ "ITrav",
    variable == "S8_Fiscal measures" ~ "Fisc",
    variable == "S9_Monetary measures" ~ "MonM",
    variable == "S10_Emergency investment in health care" ~ "HCInv",
    variable == "S11_Investment in Vaccines" ~ "VacInv"),
    Date = ymd(Date)) %>% 
  rename("Country" = "CountryName") %>% 
  mutate(value = ifelse(vari %in% c("Fisc", "MonM", "VacInv", "HCInv"), si_vec(value), value)) %>% 
  write_csv("data/oxford_clean.csv")


#### interesting posts http://nrg.cs.ucl.ac.uk/mjh/covid19/

