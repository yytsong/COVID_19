library(rjson)
library(tidyjson)
library(dplyr)
source ("Libraries.R")


json_file <- "https://covidlive.com.au//covid-live.json"
json_data <- fromJSON(file=json_file)
aus_dt_t<- json_data %>% spread_all %>% tibble()
colnames(aus_dt_t)
aus_dt <- aus_dt_t %>%
  filter (CODE == "VIC") %>%
  mutate(CASE_CNT = as.numeric(CASE_CNT),
         TEST_CNT = as.numeric(TEST_CNT),
         ACTIVE_CNT= as.numeric(ACTIVE_CNT),
         DEATH_CNT = as.numeric(DEATH_CNT),
         RECOV_CNT = as.numeric(RECOV_CNT),
         MED_ICU_CNT = as.numeric(MED_ICU_CNT),
         MED_VENT_CNT = as.numeric(MED_VENT_CNT),
         MED_HOSP_CNT=as.numeric(MED_HOSP_CNT),
         REPORT_DATE = ymd (REPORT_DATE)) %>%
  arrange(REPORT_DATE)%>%
  mutate (CASE_CNT_N = CASE_CNT - lag (CASE_CNT)) %>%
  mutate (TEST_CNT_N = TEST_CNT - lag (TEST_CNT)) %>%
  mutate(TEST_CNT_N = (TEST_CNT_N+lag(TEST_CNT_N,1)+lag(TEST_CNT_N,2))/3)


json_file <- "https://covidlive.com.au///covid-live-loc.json"
json_data <- fromJSON(file=json_file)
aus_dt_lga<- json_data %>% spread_all
aus_dt_lga <- tibble(aus_dt_lga)
colnames(aus_dt_lga)
vic_dt <- aus_dt_lga %>%
  select ( -`..JSON`)%>%
  mutate(CASE_CNT = as.numeric(CASE_CNT),
         ACTIVE_CASE_CNT = as.numeric(ACTIVE_CASE_CNT),
         REPORT_DATE = ymd (REPORT_DATE)) %>%
  arrange(REPORT_DATE)%>%
  mutate (CASE_CNT_N = CASE_CNT - lag (CASE_CNT)) %>%
  mutate (ACTIVE_CASE_CNT_N = ACTIVE_CASE_CNT - lag (ACTIVE_CASE_CNT)) %>%
  select(REPORT_DATE, LOCALITY_NAME,CASE_CNT,ACTIVE_CASE_CNT) %>%
  tidyr::complete(REPORT_DATE,LOCALITY_NAME) %>%
  mutate(CASE_CNT= replace_na(CASE_CNT, 0), ACTIVE_CASE_CNT = replace_na(ACTIVE_CASE_CNT,0))%>%
  mutate(Label = ifelse(REPORT_DATE == max (REPORT_DATE), LOCALITY_NAME, ""))