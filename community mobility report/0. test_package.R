# testing idea of community movement report
## source https://www.r-bloggers.com/scraping-google-covid-19-community-movement-data-from-pdf-figures/

# remotes::install_github("joachim-gassen/tidycovid19")
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidycovid19)
  library(pdftools)
  library(png)
})

pdf_url <- "https://www.gstatic.com/covid19/mobility/2020-04-05_DE_Mobility_Report_en.pdf"

pdf_convert(pdf_url, pages = 1, filenames = "community mobility report/google_cmr_de_p1.png", verbose = FALSE)

bitmaps <- tidycovid19:::extract_line_graph_bitmaps(pdf_url, 1) 
png_file <- tempfile("community mobility report/bitmap_", fileext = ".png")
writePNG(bitmaps[[1]][[1]], "community mobility report/bitmap.png")

df <- tidycovid19:::parse_line_graph_bitmap(bitmaps[[1]][[1]])

ggplot(data = df, aes(x = date, y = measure)) + 
  geom_line(size = 2, color = "blue") + 
  geom_ribbon(
    aes(ymin = ifelse(measure > 0, 0, measure), ymax = 0), 
    fill = "blue", alpha = 0.2
  ) +
  theme_minimal() +
  scale_y_continuous(
    name="Retail & recreation", limits=c(-0.8, 0.8),
    breaks = c(-0.8, -0.4, 0, 0.4, 0.8)
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

merged_dta <- download_merged_data(cached = TRUE, silent = TRUE)

merged_dta %>% 
  filter(iso3c == "DEU", date >= "2020-02-23") %>%
  mutate(gov_interventions = (soc_dist + mov_rest)/
           max(soc_dist + mov_rest, na.rm = TRUE),
         lockdown = lockdown == 1) %>%
  select(date, lockdown, gov_interventions, starts_with("gcmr_")) %>%
  pivot_longer(cols = c(3:9), names_to = "measure", values_to = "value") %>%
  na.omit() -> dta

ggplot(dta, aes(x = date, y = value, group = measure, color = measure)) + 
  theme_minimal() +
  annotate("rect", xmin = min(dta$date[dta$lockdown]), xmax = max(dta$date), 
           ymin = -Inf, ymax = Inf, 
           fill = "lightblue", color = NA, alpha = 0.2) +
  geom_line()  

gcmr_cl_data <- scrape_google_cmr_data(cached = TRUE, silent = TRUE, daily_data = FALSE)

merged_dta %>%
  filter(!is.na(soc_dist), !is.na(mov_rest)) %>%
  mutate(
    gov_interventions = (soc_dist/max(soc_dist, na.rm = TRUE) + 
                           mov_rest/max(mov_rest, na.rm = TRUE) + lockdown)/3
  ) %>% 
  group_by(iso3c) %>%
  summarise(gov_interventions = mean(gov_interventions)) %>%
  ungroup() %>%
  mutate(gov_interventions = percent_rank(gov_interventions)) %>%
  left_join(gcmr_cl_data, by = "iso3c") %>%
  mutate(mn_gcmr = (retail_recreation + grocery_pharmacy + 
                      transit_stations + workplaces)/4) %>%
  na.omit() %>%
  ggplot(aes(x = gov_interventions, y = mn_gcmr)) + 
  geom_point() + 
  theme_minimal() +
  geom_smooth(method = "lm", formula = "y ~ x")


merged_dta %>%
  filter(!is.na(lockdown)) %>%
  group_by(iso3c) %>%
  summarize(lockdown = max(lockdown) == 1) %>% 
  ungroup() %>%
  left_join(gcmr_cl_data, by = "iso3c") %>%
  mutate(mn_gcmr = (retail_recreation + grocery_pharmacy + 
                      transit_stations + workplaces)/4) %>%
  na.omit() %>%
  ggplot(aes(x = lockdown, y = mn_gcmr)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(position = position_jitter(width=.3, height=0)) +
  theme_minimal()


