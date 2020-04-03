## test world bank data
## source code https://www.r-bloggers.com/tidying-the-new-johns-hopkins-covid-19-time-series-datasets/
## package used library(wbstats)
## this script is only required to run once in a while, and dt will be joint in 0. load_data_jhu.R

pull_worldbank_data <- function(vars) {
  new_cache <- wbcache()
  all_vars <- as.character(unique(new_cache$indicators$indicatorID))
  data_wide <- wb(indicator = vars, mrv = 10, return_wide = TRUE)
  new_cache$indicators[new_cache$indicators[,"indicatorID"] %in% vars, ] %>%
    rename(var_name = indicatorID) %>%
    mutate(var_def = paste(indicator, "\nNote:",
                           indicatorDesc, "\nSource:", sourceOrg)) %>%
    select(var_name, var_def) -> wb_data_def
  new_cache$countries %>%
    select(iso3c, iso2c, country, region, income) -> ctries
  left_join(data_wide, ctries, by = "iso3c") %>%
    rename(year = date,
           iso2c = iso2c.y,
           country = country.y) %>%
    select(iso3c, iso2c, country, region, income, everything()) %>%
    select(-iso2c.x, -country.x) %>%
    filter(!is.na(NY.GDP.PCAP.KD),
           region != "Aggregates") -> wb_data
  wb_data$year <- as.numeric(wb_data$year)
  wb_data_def<- left_join(data.frame(var_name = names(wb_data),
                                     stringsAsFactors = FALSE),
                          wb_data_def, by = "var_name")
  wb_data_def$var_def[1:23] <- c(
    "Three letter ISO country code as used by World Bank",
    "Two letter ISO country code as used by World Bank",
    "Country name as used by World Bank",
    "World Bank regional country classification",
    "World Bank income group classification",
    "Calendar year of observation",
    "Hospital beds (per 1,000 people)",
    "Urban population (% of total population)",
    "Population ages 65 and above (% of total population)",
    "Death rate, crude (per 1,000 people)",
    "Unemployment, total (% of total labor force) (modeled ILO estimate)",
    "Incidence of tuberculosis (per 100,000 people)",
    "GDP per capita growth (annual %)",
    "Imports of goods and services (% of GDP)",
    "Population living in slums (% of urban population)",
    "Personal remittances, received (current US$)",
    "Specialist surgical workforce (per 100,000 population)",
    "Nurses and midwives (per 1,000 people)",
    "Physicians (per 1,000 people)",
    "Vulnerable employment, male (% of male employment) (modeled ILO estimate)",
    "Vulnerable employment, female (% of female employment) (modeled ILO estimate)",
    "International tourism, number of arrivals",
    "International tourism, number of departures")
  wb_data_def$type = c("cs_id", rep("factor",  4), "ts_id",
                       rep("numeric", ncol(wb_data) - 6))
  return(list(wb_data, wb_data_def))
}

vars <- c("SP.POP.TOTL", "AG.LND.TOTL.K2", "EN.POP.DNST", 
          "EN.URB.LCTY", "SP.DYN.LE00.IN", "NY.GDP.PCAP.KD",
          "SH.MED.BEDS.ZS", "SP.URB.TOTL.IN.ZS","SP.POP.65UP.TO.ZS",
          "SP.DYN.CDRT.IN", "SL.UEM.TOTL.ZS", "SH.TBS.INCD",
          "NY.GDP.PCAP.KD.ZG", "NE.IMP.GNFS.ZS","EN.POP.SLUM.UR.ZS",
          "BX.TRF.PWKR.CD.DT", "SH.MED.SAOP.P5","SH.MED.NUMW.P3",
          "SH.MED.PHYS.ZS", "SL.EMP.VULN.MA.ZS","SL.EMP.VULN.FE.ZS",
          "ST.INT.ARVL", "ST.INT.DPRT")
wb_list <- pull_worldbank_data(vars)
wb_data <- wb_list[[1]]
wb_data_def <- wb_list[[2]]

wb_cs <- 
wb_data %>%
  group_by(iso2c, country, region, income) %>%
  arrange(iso2c, year) %>%
  summarise(
    population = last(na.omit(SP.POP.TOTL)),
    land_area_skm = ifelse(identical(na.omit(AG.LND.TOTL.K2), logical(0)), NA, last(na.omit(AG.LND.TOTL.K2))),
    pop_density = ifelse(identical(na.omit(EN.POP.DNST), logical(0)), NA, last(na.omit(EN.POP.DNST))),
    pop_largest_city = ifelse(identical(na.omit(EN.URB.LCTY), logical(0)), NA, last(na.omit(EN.URB.LCTY))), 
    gdp_capita = ifelse(identical(na.omit(NY.GDP.PCAP.KD), logical(0)), NA, last(na.omit(NY.GDP.PCAP.KD))),
    life_expectancy = ifelse(identical(na.omit(SP.DYN.LE00.IN), logical(0)), NA, last(na.omit(SP.DYN.LE00.IN))),
    hospital_bed = ifelse(identical(na.omit(SH.MED.BEDS.ZS), logical(0)), NA, last(na.omit(SH.MED.BEDS.ZS))),
    urban_pop = ifelse(identical(na.omit(SP.URB.TOTL.IN.ZS), logical(0)), NA, last(na.omit(SP.URB.TOTL.IN.ZS))),
    pop_age_65 = ifelse(identical(na.omit(SP.POP.65UP.TO.ZS), logical(0)), NA, last(na.omit(SP.POP.65UP.TO.ZS))),
   death_crude = ifelse(identical(na.omit(SP.DYN.CDRT.IN), logical(0)), NA, last(na.omit(SP.DYN.CDRT.IN))),
    unemployment = ifelse(identical(na.omit(SL.UEM.TOTL.ZS), logical(0)), NA, last(na.omit(SL.UEM.TOTL.ZS))),
    tuberculosis = ifelse(identical(na.omit(SH.TBS.INCD), logical(0)), NA, last(na.omit(SH.TBS.INCD))),
    gdp_per_capita_growth = ifelse(identical(na.omit(NY.GDP.PCAP.KD.ZG), logical(0)), NA, last(na.omit(NY.GDP.PCAP.KD.ZG))),
    import_goods = ifelse(identical(na.omit(NE.IMP.GNFS.ZS), logical(0)), NA, last(na.omit(NE.IMP.GNFS.ZS))),
    pop_slum = ifelse(identical(na.omit(EN.POP.SLUM.UR.ZS), logical(0)), NA, last(na.omit(EN.POP.SLUM.UR.ZS))),
    remittances = ifelse(identical(na.omit(BX.TRF.PWKR.CD.DT), logical(0)), NA, last(na.omit(BX.TRF.PWKR.CD.DT))),
    specialists = ifelse(identical(na.omit(SH.MED.SAOP.P5), logical(0)), NA, last(na.omit(SH.MED.SAOP.P5))),
   nurse = ifelse(identical(na.omit(SH.MED.NUMW.P3), logical(0)), NA, last(na.omit(SH.MED.NUMW.P3))),
    physicians = ifelse(identical(na.omit(SH.MED.PHYS.ZS), logical(0)), NA, last(na.omit(SH.MED.PHYS.ZS))),
   v_employment_male = ifelse(identical(na.omit(SL.EMP.VULN.MA.ZS), logical(0)), NA, last(na.omit(SL.EMP.VULN.MA.ZS))),
   v_employment_female = ifelse(identical(na.omit(SL.EMP.VULN.FE.ZS), logical(0)), NA, last(na.omit(SL.EMP.VULN.FE.ZS))),
   int_arrival = ifelse(identical(na.omit(ST.INT.ARVL), logical(0)), NA, last(na.omit(ST.INT.ARVL))),
   int_departure = ifelse(identical(na.omit(ST.INT.DPRT), logical(0)), NA, last(na.omit(ST.INT.DPRT)))
    ) %>% 
  ungroup() %>% 
  mutate(hospital_bed = hospital_bed * population/1000,
         death_crude = death_crude * population/1000,
         tuberculosis = tuberculosis * population/100000,
         specialists = specialists * population/100000,
         nurse = nurse * population/1000,
         physicians = physicians * population/1000)


country_code <- read_excel("prepare_data/country_code.xlsx")  %>% 
  select(Country, `Alpha-2 code`, `Alpha-3 code`)

# setdiff(region_country_list$country_region,country_code$Country )

wb_dt <- wb_cs %>% 
  right_join(country_code, by = c("iso2c" = "Alpha-2 code")) %>% 
  filter(!is.na(country)) 



### new heath measures
### source ??? ask behrooz


files <- list.files(path = "~/Dropbox/COVID_19/Other/Explore/", pattern = ".csv")

clean_a_file <- function(filename){
  # file <- files[2]
  
  dt1 <- fread(str_c("~/Dropbox/COVID_19/Other/Explore/",filename)) %>% 
    group_by(Code) %>% 
    filter(Year == max(Year)) %>% 
    ungroup() %>% 
    select(-c(Entity, Year)) %>% 
    mutate_all(na_if,"") %>% 
    filter(!is.na(Code)) %>% 
    as.data.table()
  
  print(str_c(filename, " is completed ..."))
  dt1
  
}


add_wb <- 
  map(files,clean_a_file) %>% 
  reduce(left_join, by = "Code")


wb_dt_final <- wb_dt %>% 
  left_join(add_wb, by =c("Alpha-3 code"="Code")) %>% 
  select(Country, `Alpha-3 code`, region, income, population:ncol(.)) 

write_csv(wb_dt_final, str_c("data/wb_data_", Sys.Date(), ".csv"))






# ### source https://www.r-bloggers.com/meet-tidycovid19-yet-another-covid-19-related-r-package/
# ### connect to https://www.acaps.org/covid19-government-measures-dataset
# library(tidycovid19)
# 
# acaps_npi <- download_acaps_npi_data()
# 
# category_npi <- acaps_npi %>%
#   distinct(category, measure) %>%
#   arrange(category, measure)





