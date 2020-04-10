# load data in


pop <- fread("data/pop.csv")

### data collected by John Hopkins University

# dt_death <- fread("data/death.csv") 
# 
# dt_recovered <- fread("data/recovered.csv")
# recovery_latest_day <- dt_recovered$Date %>% max()

dt <- fread("data/dt.csv") %>% 
  mutate(Date = ymd(Date))
latest_date <- max(dt$Date)


detailed_country_dt <- fread("data/detailed_country_dt.csv") %>% 
  mutate(Date = as.Date(Date))
country_state_list <- detailed_country_dt %>% distinct(country_region, state)


country_sorted_by_cases <- dt %>%
  group_by(`country_region`, Date) %>% 
  summarise(cumulative_cases = sum(confirmed_cases)) %>% 
  ungroup() %>% 
  group_by(country_region) %>% 
  filter(cumulative_cases == max(cumulative_cases, na.rm = TRUE)) %>% 
  filter(Date == max(Date)) %>% 
  arrange(desc(cumulative_cases)) %>% 
  pull(country_region)


us_state_sorted_by_cases <- detailed_country_dt %>%
  filter(country_region == "US") %>% 
  group_by(state) %>% 
  filter(confirmed_cases == max(confirmed_cases, na.rm = TRUE)) %>% 
  filter(Date == max(Date)) %>% 
  arrange(desc(confirmed_cases)) %>% 
  pull(state)

au_state_sorted_by_cases <-  detailed_country_dt %>%
  filter(country_region == "Australia") %>%
  group_by(state) %>% 
  filter(confirmed_cases == max(confirmed_cases, na.rm = TRUE)) %>% 
  filter(Date == max(Date)) %>% 
  arrange(desc(confirmed_cases)) %>% 
  pull(state)

ch_state_sorted_by_cases <-  detailed_country_dt %>%
  filter(country_region == "China") %>%
  group_by(state) %>% 
  filter(confirmed_cases == max(confirmed_cases, na.rm = TRUE)) %>% 
  filter(Date == max(Date)) %>% 
  arrange(desc(confirmed_cases)) %>% 
  pull(state)


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
# 
# 
# rc_list <- region_country_list[,c("Continent", "Region")] %>% 
#   distinct(Region, .keep_all = TRUE) %>% 
#   group_by(Continent) %>% 
#   mutate(Region = list(Region)) %>% 
#   distinct()
# 
# 
# rc_dictionary <- setNames(rc_list$Region, rc_list$Continent)


causes <- wb_dt <- fread("data/annual-number-of-deaths-by-cause.csv") %>%
  group_by(Entity) %>%
  filter (Year == max (Year)) %>%
  mutate (country_region = Entity)


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
  mutate_if(is.logical, as.numeric) %>% 
  left_join(causes, by = c("Country" = "country_region"))

# wb_dt %>% write_csv(str_c("other/behrooz_wb_dt_", Sys.Date(),".csv"))  



# fomratting functions for scales
si_num <- function (x) {
  
  if (!is.na(x)) {
    
    if (x < 0){ 
      sign <-  "-"
      x <- abs(x)
    }else{
      sign <-  ""
      x <- x
    }
    
    
    if (x >= 1e9) { 
      chrs <- strsplit(format(x, scientific=12), split="")[[1]];
      len <- chrs[seq(1,length(chrs)-9)] %>% length();
      rem <- chrs[seq(1,length(chrs)-8)];
      rem <- append(rem, ".", after = len) %>% append("B");
    }
    
    
    else if (x >= 1e6) { 
      chrs <- strsplit(format(x, scientific=12), split="")[[1]];
      len <- chrs[seq(1,length(chrs)-6)] %>% length();
      rem <- chrs[seq(1,length(chrs)-5)]
      rem <- append(rem, ".", after = len) %>% append("M");
    }
    
    else if (x >= 1e3) { 
      chrs <- strsplit(format(x, scientific=12), split="")[[1]];
      len <- chrs[seq(1,length(chrs)-3)] %>% length();
      rem <- chrs[seq(1,length(chrs)-2)];
      rem <- append(rem, ".", after = len) %>% append("K");
    }
    
    else {
      return(x);
    }
    
    return(str_c(sign, paste(rem, sep="", collapse=""), sep = ""));
  }
  else return(NA);
} 

si_vec <- function(x) {
  sapply(x, FUN=si_num);
}

###### policy data-------------
### additional source from OXFORD https://www.bsg.ox.ac.uk/research/research-projects/oxford-covid-19-government-response-tracker



oxford_dt <- fread("data/oxford_clean.csv") %>% 
  mutate(Date = ymd(Date))

oxford_cat_legend <- oxford_dt %>% 
  distinct(vari,variable) %>% 
  rename("Code" = "vari", "Policy Type" = "variable") 


acaps_dt <- fread("data/acaps_policy.csv")

acaps_cat_legend <- acaps_dt %>% 
  distinct(category_final,code, measure_final) %>% 
  rename("Code" = "code", "Policy Type" = "measure_final") %>% 
  mutate(no = str_extract(Code, pattern = "[0-9]+"),
         no = as.numeric(no)) %>% 
  arrange(category_final, no) %>% 
  select(Code, `Policy Type`)




radar_dt <- wb_dt %>%
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
  pivot_longer(cols = c("death_cases_rate","urban_pop", "life_expectancy", "pop_age_65", 
                        "nurse", "physicians", "hospital_bed", 
                        "unemployment", "gdp_capita","v_employment_male","v_employment_female"), 
               names_to = "Plot.title", values_to = "value") %>% 
  group_by(`Plot.title`) %>% 
  mutate(value = value/max(value, na.rm = TRUE)) %>% 
  mutate(value = ifelse(`Plot.title` %in% c("death_cases_rate","urban_pop", "pop_age_65", 
                                            "unemployment", "v_employment_male","v_employment_female"),
                        1-value, value)) %>% 
  ungroup() %>% 
  mutate(#death_cases_rate = death_cases_rate/max(death_cases_rate, na.rm = TRUE),
         Plot.title = case_when(
           Plot.title == "urban_pop" ~ "Urban\nPopulation",
           Plot.title == "life_expectancy" ~ "Life\nExpectancy",
           Plot.title == "pop_age_65" ~ "Population\nAge>=65",
           Plot.title == "nurse" ~ "Nurses",
           Plot.title == "physicians" ~ "Physicians",
           Plot.title == "hospital_bed" ~ "Hospital\nBeds",
           Plot.title == "unemployment" ~ "Unemployment",
           Plot.title == "gdp_capita" ~ "GDP\nper Capita",
           Plot.title == "v_employment_male" ~ "Vulnerable\nEmployment\nMale",
           Plot.title == "v_employment_female" ~ "Vulnerable\nEmployement\nFemale",
           Plot.title == "death_cases_rate" ~ "\nCOVID19\nDeath Rate")) %>% 
  as.data.table()


## load testing data in -----

test_dt_init <- fread("data/testing.csv") %>% 
  mutate(Date = as.Date(Date)) 

country_test <- dt %>% 
  group_by(country_region, Date, Population) %>% 
  summarise_at(vars(confirmed_cases, death_cases, recovered_cases), funs(sum)) %>% 
  ungroup() %>% 
  filter(country_region %in% unique(test_dt_init$Country)) %>% 
  select(country_region, Date, confirmed_cases, death_cases, recovered_cases) %>% 
  filter(confirmed_cases > 0) %>% 
  group_by(country_region) %>% 
  mutate(first_day = min(Date)) %>% 
  ungroup()

dayzero_test <- test_dt_init %>%
  left_join(country_test %>% distinct(country_region, first_day), by = c("Country" = "country_region")) %>% 
  mutate(days = as.numeric(Date - first_day)) 

dayzero_end_test <- dayzero_test %>%
  group_by(Country) %>%
  filter(days == last(days)) %>%
  ungroup()


 







