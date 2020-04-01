
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(scales)
# library(plotly)
library(data.table)
library(lubridate)
library(ggrepel)
library(xml2)
#library(rsconnect)
library(bit64)
#library(gganimate)
#library(gifski)
#library(png)
#library(maps)
library(RCurl)
library(httr)
library(readxl)
library(wbstats)
#library(ggalluvial)
pkgload::load_all(path= "GAlogger/")
theme_set(theme_minimal())
#library(tidylog)
source("R/0. load_data_jhu.R")
source("R/1. functions.R")



ui <- navbarPage("COVID-19 Application",
                 
                 id = "tab",
                 selected = "About",
                 


                 
                # tags$head(tags$script(HTML(("google-analytics.html"))),
                #           tags$style(HTML("#tab > li:first-child { display: none; }"))),
                 
                #header =singleton(tags$head(includeHTML("google-analytics.html"))),
                 
             #    tags$style(type = 'text/css',
              #              '.navbar { background-color: skyblue;}',
              #              '.navbar-default .navbar-brand{color: white;}',
              #              '.tab-panel{ background-color: red; color: white}',
              #              '.nav navbar-nav li.active:hover a, .nav navbar-nav li.active a {
              #          background-color: green ;
              #          border-color: green;
              #          }'

               #  ),

                 
                # fluid = FALSE,
                
            #    tabsetPanel(
      ####### this is a start of a page "About" ----------------
                tabPanel("About",
                         # br(), 
                         # tags$strong(style="color: red", str_c("THERE IS CURRENTLY AN ISSUE WITH THE SOURCE DATA AS THE LASTEST UPDATES OF CONFIRMED AND DEATH CASES IS ON ", latest_date, " BUT FOR RECOVERED CASES IS ON ", recovery_latest_day, ".")),br(),
                         # tags$strong(style="color: red", str_c("WE ARE ALSO EXPERIENCING A HIGH VOLUME OF SESSIONS ON OUR DASHBOARD. IF YOU RECEIVE A SERVER DISCONNECTION MESSAGE, WE ARE VERY SORRY FOR THAT. WE DID NOT EXPECT THIS LEVEL OF INTEREST AND WE ARE TRYING TO FIX THIS ISSUE ASAP.")),br(),
                         # tags$strong(style="color: red", "We have updated all the figure titles to reflect the latest day. We will monitor the situation and update accordingly."),br(),
                         # 
                         # 
                         # br(), 
                         strong("How to Use this APP"), br(),
                         "We have developed this app to provide live data on the trends of COVID-19 coronavirus. The app provides you with a range of analysis and visualisations that enable you to track the spread of virus around the world and helps you get a sense of the patterns across different countries and regions",
                         br(), br(),
                         
                         fluidRow(
                           
                           column(4),
                           column(4,
                           plotOutput("plt_continent", height = "400px"))#, 
                         
                          #  column(5,
                          # plotOutput("plt_region", height = "350px"))
                          ),
                         
                         
                         
                         "You have a few options in the top menu:", br(), br(),
                         strong ("World - Explorer:"),"This page allows you to visualise different indicators such as the number of cases, deaths, or recoveries, both cumulative and new, in actual or per capita terms.",br(),br(),
                         #shiny::img(src="image1.png", align = "centre"), 
                         # fluidRow(
                         #   column(3),
                         #   column(6,
                         #          plotOutput("plt_italy_china", height = "600px")),
                         #   column(3)),
                       #  br(), br(),
                         strong ("World - Day Zero:"),"This page presents the number of cases (reported, deaths, recoveries) in actual and per capita terms. You can add or remove countries and compare them. The x-axis is number of days since the first case was reported which provides you with an opportunity to understand how long it has taken a country to reach a specific number of cases. You can change this number to be a higher value, for example, days since the 100th case was reported.",br(),br(),
                         strong ("World - Growth:"),"This page attempts to present a basic model to predict the number of new cases every day compared to the total number of cases the day before. The graphs can be used to compare the growth rate of reported, death or recovery cases across countries. The plot on the right hand side compares the growth rate of each country with a fixed line to help identifying countries with high or low growth patterns.",br(),br(),
                         strong ("World - Ratio:"),"This page helps you to follow the death and recovery ratios across countries and over time. Please notice that the recovry data has some issues which seem to be due to different reporting practices.",br(),br(),
                         strong ("World - Healthcare"),"We aim to provide a range of data, mainly sourced from World Bank data, to help provide a better understanding of the capacity of healthcare systems to cope with the outbreak.",br(),br(),
                       
                         strong ("AUS-US-CHN Day Zero"),"These graphs are very similar to World Day Zero, however, since the sub-region (province/state) data is available for some counntries we have provided the graph at the next level.",br(),br(),
                       
                         
                         br(), 
                                strong("Acknowledgements"),"We acknowledge and appreciate the support that the RStudio team provided by offering an unlimited access account for this application.",
                                "The data is sourced from ",
                                a(href="https://github.com/CSSEGISandData/COVID-19","Johns Hopkins University", target = "_blank")," and ",
                               # a(href="https://github.com/nytimes/covid-19-data","New York Times", target = "_blank")," (US State), and ", 
                                a(href="https://data.worldbank.org","World Bank", target = "_blank"), ".",br(),
                                
                                strong("Feedback"),
                                "Please contact us on ",
                                a(href="https://www.linkedin.com/in/behroozh/","Behrooz Hassani-M, PhD", target = "_blank"), " and ",
                                a(href="https://www.linkedin.com/in/ytsong/","Yutong (Yuri) Song, PhD", target = "_blank"),".", br(),
                       
                       strong("Project"),
                       "This project is avaliable at",
                       a(href="https://rstudio.cloud/project/1029711","RStudio Cloud Project.", target = "_blank"), 
                         
                         ),
                
      ####### this is a start of a page  "Explorer"----------------
            
                tabPanel("World - Explorer",
                         
                         fluidRow(
                           
                           column(6,
                           strong("Description"),
        "On this page, you can plot a range of different indicators against each other. For example, if you want to generate a graph that shows the number of deaths against the number of reported cases across countries, you can select for the x-axis, Confirmed Cases from X-measure, then choose Cumulative Cases, and then Actual, and for the y-axis select Death Cases, Cumulative and then Actual. This would then show you Death against Cases and so you can get a sense of changes in death rate across countries.
        You can change the list of countries included"),
      column(6,
             strong("Acknowledgements"),"We acknowledge and appreciate the support that the RStudio team provided by offering an unlimited access account for this application.",
             "The data is sourced from ",
             a(href="https://github.com/CSSEGISandData/COVID-19","Johns Hopkins University", target = "_blank"),".", br(),
             
             strong("Feedback"),
             "Please contact us on ",
             a(href="https://www.linkedin.com/in/behroozh/","Behrooz Hassani-M, PhD", target = "_blank"), " and ",
             a(href="https://www.linkedin.com/in/ytsong/","Yutong (Yuri) Song, PhD", target = "_blank"),".")


                         ), br(),
                         
                         

                           fluidRow(
                           
                           column(2,
                                  pickerInput(inputId = "region0", 
                                                 label = "Region",
                                                 choices = region_country_list$Continent %>% unique(),
                                                 selected = region_country_list$Continent %>% unique(),
                                              options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                             liveSearchStyle = 'contains'),
                                                 multiple = TRUE)
                                  ),
                             
                          column(2,
                                 uiOutput("country0")
                                  ),
                           
                           
                           column(2,
                                  pickerInput(inputId = "aspect_x",
                                              label = "X - Measure",
                                              choices = c("Confirmed Cases", "Death Cases", "Recovered Cases"),
                                              selected = "Confirmed Cases",
                                              multiple= FALSE)),
                           
                           
                           column(2,             
                                  pickerInput(inputId = "measure_x",
                                              label = "X - Cumulative/New",
                                              choices = c("Cumulative Cases", "New Cases"),
                                              selected = "Cumulative Cases",
                                              multiple= FALSE)),
                           

                           
                           column(2,
                                  pickerInput(inputId = "actual_x",
                                              label = "X - Actual/Per Capita",
                                              choices = c("Actual", "Per Capita (10 Million)"),
                                              selected = "Actual",
                                              multiple= FALSE)),
                          
                          column(2,
                                 numericInput(
                                   inputId = "min_case0",
                                   label = "Since Confirmed Cases >=",
                                   min = 0,
                                   max = 1000,
                                   value = 10,
                                   step = 1))
                          
                           
                           ), # close fluidrow
                           
                           
                           fluidRow(
                             column(4),
                             
                             column(2,
                                    pickerInput(inputId = "aspect_y",
                                                label = "Y - Measure",
                                                choices = c("Confirmed Cases", "Death Cases", "Recovered Cases"),
                                                selected = "Death Cases",
                                                multiple= FALSE)),
                             
                             column(2,             
                                    pickerInput(inputId = "measure_y",
                                                label = "Y - Cumulative/New",
                                                choices = c("Cumulative Cases", "New Cases"),
                                                selected = "Cumulative Cases",
                                                multiple= FALSE)),
                             

                             
                             column(2,
                                    pickerInput(inputId = "actual_y",
                                                label = "Y - Actual/Per Capita",
                                                choices = c("Actual", "Per Capita (10 Million)"),
                                                selected = "Actual",
                                                multiple= FALSE))), # close fluidrow
                           br(),
                         
                         
                         fluidRow(
                           column(3),
                           column(6,
                                  plotOutput("plt_world_explore", height = "600px")),
                           column(3))
                        
                         
                ),
                
                
      ####### this is a start of a page "Day Zero" ----------------
      
                 tabPanel("World - Day Zero",
                  
                          fluidRow(
                            
                            column(6,
                                   strong("Description"),
                                   "On this page, you can look at the trend for the number of new and total cases, both actual (left) and per capita (right, per 10 M of population). On the x-axis we the plot the number of days since the Nth case was reported in each country, you can change this number. Please notice that the y-axis is logged."),
                            column(6,
                                   strong("Acknowledgements"),"We acknowledge and appreciate the support that the RStudio team provided by offering an unlimited access account for this application.",
                                   "The data is sourced from ",
                                   a(href="https://github.com/CSSEGISandData/COVID-19","Johns Hopkins University", target = "_blank"),".", br(),
                                   
                                   strong("Feedback"),
                                   "Please contact us on ",
                                   a(href="https://www.linkedin.com/in/behroozh/","Behrooz Hassani-M, PhD", target = "_blank"), " and ",
                                   a(href="https://www.linkedin.com/in/ytsong/","Yutong (Yuri) Song, PhD", target = "_blank"),".")
                            
                            
                          ), br(),
                          
                          
                          
                          
                          
                 fluidRow(
                   column(2,
                          pickerInput(inputId = "region", 
                                         label = "Region",
                                         choices = region_country_list$Continent %>% unique(),
                                         selected = region_country_list$Continent %>% unique(),
                                         options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                        liveSearchStyle = 'contains'),
                                         multiple = TRUE)),
                   
                   column(2,  uiOutput("country")),
                   
  
                      column(2,             
                                   pickerInput(inputId = "measure",
                                               label = "Cumulative/New",
                                               choices = c("Cumulative Cases", "New Cases"),
                                               selected = "Cumulative Cases",
                                               multiple= FALSE)),
                     
                     column(2,
                            pickerInput(inputId = "aspect",
                                        label = "Measure",
                                        choices = c("Confirmed Cases", "Death Cases", "Recovered Cases"),
                                        selected = "Confirmed Cases",
                                        multiple= FALSE)),
        
                   column(2,
                          numericInput(
                            inputId = "min_case",
                            label = "Since Measured Cases >=",
                            min = 0,
                            max = 1000,
                            value = 10,
                            step = 1))
                   
                   ),br(),
                              

                     fluidRow(         
                         column(6,
                                  plotOutput("plt_pass_day", height = "600px")),
                         column(6,
                                  plotOutput("plt_per_capita", height = "600px")))

                 ),
                
      ####### this is a start of a page "Growth"----------------
      
                tabPanel("World - Growth",
                         
                         
                         fluidRow(
                           
                           column(6,
                                  strong("Description"),
                                  "This page attempts to present a basic model to predict the number of new cases every day compared to the total number of cases the day before. The graphs can be used to compare the growth rate of reported, death, or recovery cases across countries. The plot on the right hand side compares the growth rate of each country with a fixed line to help identifying countries with high or low growth patterns."),
                           column(6,
                                  strong("Acknowledgements"),"We acknowledge and appreciate the support that the RStudio team provided by offering an unlimited access account for this application.",
                                  "The data is sourced from ",
                                  a(href="https://github.com/CSSEGISandData/COVID-19","Johns Hopkins University", target = "_blank"),".", br(),
                                  
                                  strong("Feedback"),
                                  "Please contact us on ",
                                  a(href="https://www.linkedin.com/in/behroozh/","Behrooz Hassani-M, PhD", target = "_blank"), " and ",
                                  a(href="https://www.linkedin.com/in/ytsong/","Yutong (Yuri) Song, PhD", target = "_blank"),".")
                           
                           
                         ), br(),
                         
                         
                         fluidRow(
                           column(2,
                                  pickerInput(inputId = "region2", 
                                                 label = "Region",
                                                 choices = region_country_list$Continent %>% unique(),
                                                 selected = region_country_list$Continent %>% unique(),
                                                 options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                                liveSearchStyle = 'contains'),
                                                 multiple = TRUE)
                           ),
                           
                           column(2, uiOutput("country2")),
                           
                           
                           column(2,
                                  pickerInput(inputId = "aspect5",
                                              label = "Measure",
                                              choices = c("Confirmed Cases", "Death Cases", "Recovered Cases"),
                                              selected = "Confirmed Cases",
                                              multiple= FALSE)),
                           
                           column(2,
                                  numericInput(
                                    inputId = "min_case2",
                                    label = "Since Measured Cases >=",
                                    min = 0,
                                    max = 1000,
                                    value = 10,
                                    step = 1))
                           
                           ),br(),
                         
                         
                         fluidRow(         
                           column(6,
                                  plotOutput("plt_pass_day_2", height = "600px")),
                           column(6,
                                  plotOutput("plt_per_capita_2", height = "600px"))
                           
                           )
                ),
                
      ####### this is a start of a page "Ratio"----------------
      
                tabPanel("World - Ratio",
                         
                         fluidRow(
                           
                           column(6,
                                  strong("Description"),
                                  "This page helps you to follow the death and recovery ratios across countries and over time. The plot on the right shows the death and the plot on the left provides the recovery ratio. Please notice that on early days the values can be volatile due to small number of cases."),
                           column(6,
                                  strong("Acknowledgements"),"We acknowledge and appreciate the support that the RStudio team provided by offering an unlimited access account for this application.",
                                  "The data is sourced from ",
                                  a(href="https://github.com/CSSEGISandData/COVID-19","Johns Hopkins University", target = "_blank"),".", br(),
                                  
                                  strong("Feedback"),
                                  "Please contact us on ",
                                  a(href="https://www.linkedin.com/in/behroozh/","Behrooz Hassani-M, PhD", target = "_blank"), " and ",
                                  a(href="https://www.linkedin.com/in/ytsong/","Yutong (Yuri) Song, PhD", target = "_blank"),".")
                           
                           
                         ), br(),
                         
                         fluidRow(
                           column(2,
                                  pickerInput(inputId = "region3", 
                                                 label = "Region",
                                                 choices = region_country_list$Continent %>% unique(),
                                                 selected = region_country_list$Continent %>% unique(),
                                                 options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                                liveSearchStyle = 'contains'),
                                                 multiple = TRUE)
                           ),
                           
                           column(2, uiOutput("country3")),
                           
                           column(2,
                                  numericInput(
                                    inputId = "min_case3",
                                    label = "Since Confirmed Cases >=",
                                    min = 0,
                                    max = 1000,
                                    value = 10,
                                    step = 1))
                           
                           
                           ),br(),
                         
                         
                         fluidRow(         
                           column(6,
                                  plotOutput("plt_perc_recovery", height = "600px")),
                           column(6,
                                  plotOutput("plt_perc_death", height = "600px")))
                ),
      
      
      ####### this is a start of a page "Heathcare System" ----------------
      
      tabPanel("World - Healthcare System",

               fluidRow(

                 column(6,
                        strong("Description"),
                        "This page consolidates world bank data with COVID-19 cases, we mainly focus on indicators that would help us better understand the capacity of the healthcare system and how much it is stressed.", br(), 
                        strong("Formula"), "Active Cases = Confirmed Cases - (Death Cases + Recovered Cases)"),
                 column(6,
                        strong("Acknowledgements"),"We acknowledge and appreciate the support that the RStudio team provided by offering an unlimited access account for this application.",
                        "The data is sourced from ",
                        a(href="https://github.com/CSSEGISandData/COVID-19","Johns Hopkins University", target = "_blank"),".", br(),
                        
                        strong("Feedback"),
                        "Please contact us on ",
                        a(href="https://www.linkedin.com/in/behroozh/","Behrooz Hassani-M, PhD", target = "_blank"), " and ",
                        a(href="https://www.linkedin.com/in/ytsong/","Yutong (Yuri) Song, PhD", target = "_blank"),".")


               ), br(),

               fluidRow(
                 
                 column(2,
                        pickerInput(inputId = "region7", 
                                    label = "Region",
                                    choices = wb_dt$Continent %>% unique(),
                                    selected = wb_dt$Continent %>% unique(),
                                    options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                   liveSearchStyle = 'contains'),
                                    multiple = TRUE)),
                 
                 column(2, uiOutput("country7"))

               ),br(),


               fluidRow(
                 column(4,plotOutput("plt_hospital_bed", height = "300px")),
                 column(4,plotOutput("plt_physicians", height = "300px")),
                 column(4,plotOutput("plt_specialists", height = "300px"))), br(),br(),
               fluidRow(
                 column(4,plotOutput("plt_nurse", height = "300px")),
                 column(4, plotOutput("plt_daily_death", height = "300px")),
                 column(4, plotOutput("plt_annual_death", height = "300px")))
      ),
      
                
      ####### this is a start of a page "State/Province"----------------
      
                tabPanel("State/Province - Day Zero",
                         
                         fluidRow(
                           
                           column(6,
                                  strong("Description"),
                                  "On this page, you can look at the trends for the number of new and total cases, both actual (left) and per capita (right, per 10 M of population). On the x-axis we the plot counts the number of days since the first case was reported in each country. Please notice that the y-axis is logged."),
                           column(6,
                                  strong("Acknowledgements"),"We acknowledge and appreciate the support that the RStudio team provided by offering an unlimited access account for this application.",
                                  "The data is sourced from ",
                                  a(href="https://github.com/CSSEGISandData/COVID-19","Johns Hopkins University", target = "_blank"),".", br(),
                                  
                                  strong("Feedback"),
                                  "Please contact us on ",
                                  a(href="https://www.linkedin.com/in/behroozh/","Behrooz Hassani-M, PhD", target = "_blank"), " and ",
                                  a(href="https://www.linkedin.com/in/ytsong/","Yutong (Yuri) Song, PhD", target = "_blank"),".")
                           
                           
                         ), br(),
                         
                         
                         fluidRow(          
                           
                           column(2,
                                  
                                  pickerInput(inputId = "detail_country",
                                              label = "Country",
                                              choices = #c("Australia", "China"),
                                                unique(country_state_list$country_region),
                                              selected = "Australia",
                                              options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                                             liveSearchStyle = 'contains'),
                                              multiple= TRUE)),
                           column(2,
                                 
                                  uiOutput("detail_state")),
                           

                           column(2,             
                                  pickerInput(inputId = "measure4",
                                              label = "Cumulative/New",
                                              choices = c("Cumulative Cases", "New Cases"),
                                              selected = "Cumulative Cases",
                                              multiple= FALSE)),
                           
                           column(2,
                                  pickerInput(inputId = "aspect4",
                                              label = "Measure",
                                              choices = c("Confirmed Cases", "Death Cases", "Recovered Cases"),
                                              selected = "Confirmed Cases",
                                              multiple= FALSE)),
                           column(2,
                                  numericInput(
                                    inputId = "min_case4",
                                    label = "Since Measured Cases >=",
                                    min = 0,
                                    max = 1000,
                                    value = 5,
                                    step = 1))
                           
                         ),br(),
                         
                         
                         fluidRow(         
                           column(6,
                                  # verbatimTextOutput("text"),
                                  # br(),
                                  plotOutput("plt_pass_day_sp", height = "600px")),
                           column(6,
                                  plotOutput("plt_per_capita_sp", height = "600px"))),br(),br(),
                         
                         
                         
                )#,

# tags$script(HTML("var header = $('.navbar> .container-fluid');
#                        header.append('<div style=\"float:right\"><h3>Company name text here</h3></div>');
#                        console.log(header)"))
                
        
            #    ) # close of tabsetPanel
                
                
                # 
                # tabPanel("Per Capita Map"#,
                #        #  imageOutput("plt_map_per_capita"), 
                #        #  imageOutput("plt_map_incident")
                #          )
                 
                 
)

###### start of server -----------------------
server <- function(input, output, session) {
  
  ga_set_tracking_id("UA-161256542-1")
  ga_set_approval(consent = TRUE)
  
  
  ##### first page -------
  
  
  output$plt_continent <- renderPlot({
    
    plot_world_lvl(lvl = "Continent")
    
  })
  
  
  # output$plt_region <- renderPlot({
  #   plot_world_lvl(lvl = "Region")
  # })
  
  
  
    
  ### world explorer -----
  
  output$country0 <- renderUI({
    req(input$region0)
    
    country <- region_country_list %>% filter(Continent %in% input$region0) %>% pull(country_region) %>% sort()
    
    pickerInput(inputId = "country0",
                label = "Country",
                choices = country,
                selected = c("US", "Italy"),
                options = list(`actions-box` = TRUE, `live-search` = TRUE,
                               liveSearchStyle = 'contains'),
                multiple= TRUE)
  })
  
  observe({
      
      if(length(input$region0) == 0){
      
        country0 <- region_country_list %>% filter(Continent %in% input$region0) %>% pull(country_region) %>% sort()
        
        updatePickerInput(session = session,
                          inputId = "country0",
                          choices = country0,
                          selected = c("US", "Italy"))  
        
          
      }else if(input$region0 > 0){
      
        country0 <- region_country_list %>% filter(Continent %in% input$region0) %>% pull(country_region) %>% sort()
        
      updatePickerInput(session = session,
                        inputId = "country0",
                        choices = country0,
                        selected = NULL)
                       
      }
    
  })
  
  world_explore_dt <- reactive({
    req(input$region0, input$country0, input$measure_x, input$aspect_x, input$actual_x,  
        input$measure_y, input$aspect_y, input$actual_y, input$min_case0)
    
    filtered_data_explore(c = input$country0, xc = input$measure_x, xm =input$aspect_x, xa = input$actual_x, 
                          yc = input$measure_y, ym = input$aspect_y, ya = input$actual_y, min_case =input$min_case0)
  })
  
  output$plt_world_explore <- renderPlot({
    req(input$region0, world_explore_dt(), input$measure_x, input$aspect_x, input$actual_x,  
        input$measure_y, input$aspect_y, input$actual_y, input$min_case0)
    plot_explore_country(df = world_explore_dt(), 
                         xc = input$measure_x, xm =input$aspect_x, xa = input$actual_x, 
                         yc = input$measure_y, ym = input$aspect_y, ya = input$actual_y, min_case =input$min_case0)
  })
  
  
  ##### find state analysis from here ---------------
  
  output$detail_state <- renderUI({
    req(input$detail_country)
    
    sp <-  country_state_list %>% filter(country_region %in% input$detail_country) %>% pull(state)
    
    pickerInput(inputId = "detail_state",
                label = "State/Province",
                choices = sp,
                selected = sp,
                options = list(`actions-box` = TRUE, `live-search` = TRUE,
                               liveSearchStyle = 'contains'),
                multiple= TRUE)
    
  })
  
  tabinfo <- reactive({
    req(input$tab)
    if (input$tab == "World - Day Zero"){
      t <- "world"}else if(input$tab == "State/Province - Day Zero"){
        t <- "other"}
    t
    
  })
  
  
  
    ##### world day zero ------

  output$country <- renderUI({
    req(input$region)
    
    country <- region_country_list %>% filter(Continent %in% input$region) %>% pull(country_region)%>% sort()
    
    pickerInput(inputId = "country",
                label = "Country",
                choices = country,
                selected = c("US", "Italy"),
                options = list(`actions-box` = TRUE, `live-search` = TRUE,
                               liveSearchStyle = 'contains'),
                multiple= TRUE)
    
  })
  
  observe({
    
    if(length(input$region) == 0){
      
      country <- region_country_list %>% filter(Continent %in% input$region) %>% pull(country_region) %>% sort()
      
      updatePickerInput(session = session,
                        inputId = "country",
                        choices = country,
                        selected = c("US", "Italy"))  
      
      
    }else if(input$region > 0){
      
      country <- region_country_list %>% filter(Continent %in% input$region) %>% pull(country_region) %>% sort()
      
      updatePickerInput(session = session,
                        inputId = "country",
                        choices = country,
                        selected = NULL)
      
    }
    
  })
  
    pass_day_dt <- reactive({
        req(input$region,input$country,input$aspect, input$min_case)

        filtered_day_passed_data(c = input$country, a = input$aspect, min_case = input$min_case)

    })
    
    output$plt_pass_day <- renderPlot({
        req(tabinfo(),  pass_day_dt(), input$region, input$country, input$measure, input$aspect, input$min_case)
           # ggplotly(
      plot_day_passed_day(df = pass_day_dt(), m = input$measure, a = input$aspect, g = tabinfo(), min_case = input$min_case
                                    )#, tooltip = "text") %>%
            #    config(displayModeBar = TRUE) 
    })
    
    output$plt_per_capita <- renderPlot({
        req( tabinfo(),pass_day_dt(), input$region, input$country, input$measure,  input$aspect, input$min_case)
        # ggplotly(
      plot_per_capita(df = pass_day_dt(), m = input$measure, a = input$aspect, g = tabinfo(), min_case = input$min_case
        )#, tooltip = "text") %>%
        #    config(displayModeBar = TRUE) 
    })
    
 ##### world growth ------------
    output$country2 <- renderUI({
      req(input$region2)
      
      country <- region_country_list %>% filter(Continent %in% input$region2) %>% pull(country_region)%>% sort()
      
      pickerInput(inputId = "country2",
                  label = "Country",
                  choices = country,
                  selected = c("US", "Italy"),
                  options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                 liveSearchStyle = 'contains'),
                  multiple= TRUE)
      
    })
    
    observe({
      
      if(length(input$region2) == 0){
        
        country2 <- region_country_list %>% filter(Continent %in% input$region2) %>% pull(country_region) %>% sort()
        
        updatePickerInput(session = session,
                          inputId = "country2",
                          choices = country2,
                          selected = c("US", "Italy"))  
        
        
      }else if(input$region2 > 0){
        
        country2 <- region_country_list %>% filter(Continent %in% input$region2) %>% pull(country_region) %>% sort()
        
        updatePickerInput(session = session,
                          inputId = "country2",
                          choices = country2,
                          selected = NULL)
        
      }
      
    })
    
    pass_day_dt2 <- reactive({
      req(input$region2,input$country2, input$aspect5, input$min_case2)
      
      filtered_day_passed_data(c = input$country2, a = input$aspect5, min_case = input$min_case2)
      
    })
    
    output$plt_pass_day_2 <- renderPlot({
      req(input$region2,input$country2, pass_day_dt2(), input$aspect5, input$min_case2)
      # ggplotly(
      plot_new_cases_growth(df = pass_day_dt2(), a = input$aspect5, min_case = input$min_case2
      )#, tooltip = "text") %>%
      #    config(displayModeBar = TRUE) 
    })
    
    output$plt_per_capita_2 <- renderPlot({
      req(input$region2, input$country2, pass_day_dt2(), input$aspect5, input$min_case2)
      # ggplotly(
      plot_new_case_growth_facet(df = pass_day_dt2(), a = input$aspect5, min_case = input$min_case2
      )#, tooltip = "text") %>%
      #    config(displayModeBar = TRUE) 
    })
    
    
    #### world recovery percent -------
    
    output$country3 <- renderUI({
      req(input$region3)
      
      country <- region_country_list %>% filter(Continent %in% input$region3) %>% pull(country_region)%>% sort()

             pickerInput(inputId = "country3",
                         label = "Country",
                         choices = country,
                         selected = c("US","Italy"),
                         options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                        liveSearchStyle = 'contains'),
                         multiple= TRUE)
      
    })
    
    observe({
      
      if(length(input$region3) == 0){
        
        country3 <- region_country_list %>% filter(Continent %in% input$region3) %>% pull(country_region) %>% sort()
        
        updatePickerInput(session = session,
                          inputId = "country3",
                          choices = country3,
                          selected = c("US","Italy"))  
        
        
      }else if(input$region3 > 0){
        
        country3 <- region_country_list %>% filter(Continent %in% input$region3) %>% pull(country_region) %>% sort()
        
        updatePickerInput(session = session,
                          inputId = "country3",
                          choices = country3,
                          selected = NULL)
        
      }
      
    })
    
    output$plt_perc_recovery <- renderPlot({
      req(input$region3,input$country3, input$min_case3)
      
      plot_perc_rec(c = input$country3, r= "Recovery Percentage", min_case = input$min_case3)
    })
    
    output$plt_perc_death <- renderPlot({
      req(input$region3, input$country3, input$min_case3)
      
      plot_perc_rec(c = input$country3, r= "Death Percentage", min_case = input$min_case3)
    })
    
    
    
    
    #### world bank page -----------
    
    
    output$country7 <- renderUI({
      req(input$region7)
      country <- wb_dt %>% distinct(Continent, Country) %>% filter(Continent %in% input$region7) %>% pull(Country)%>% sort()
      
             pickerInput(inputId = "country7",
                         label = "Country",
                         choices = country,
                         selected = c("Italy", "US"),
                         options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                        liveSearchStyle = 'contains'),
                         multiple = TRUE)
    })
    
    observe({
      
      if(length(input$region7) == 0){
        
        country7 <- wb_dt %>% distinct(Continent, Country) %>% filter(Continent %in% input$region7) %>% pull(Country)%>% sort()
        
        updatePickerInput(session = session,
                          inputId = "country7",
                          choices = country7,
                          selected = c("Italy", "US"))  
        
        
      }else if(input$region7 > 0){
        
        country7 <- wb_dt %>% distinct(Continent, Country) %>% filter(Continent %in% input$region7) %>% pull(Country)%>% sort()
        
        updatePickerInput(session = session,
                          inputId = "country7",
                          choices = country7,
                          selected = NULL)
        
      }
      
    })
    
    output$plt_hospital_bed <- renderPlot({
      req(input$country7, input$region7)
      
      plot_health_measure(c = input$country7, m ="hospital_bed")
    })
    
    output$plt_physicians <- renderPlot({
      req(input$country7, input$region7)
      
      plot_health_measure(c = input$country7, m ="physicians")
    })
    
    output$plt_specialists <- renderPlot({
      req(input$country7, input$region7)
      
      plot_health_measure(c = input$country7, m ="specialists")
    })
    
    output$plt_nurse <- renderPlot({
      req(input$country7, input$region7)
      
      plot_health_measure(c = input$country7, m ="nurse")
    })
    
    output$plt_daily_death <- renderPlot({
      req(input$country7, input$region7)
      
      plot_death_crude(c = input$country7, m ="new")
    })

    output$plt_annual_death <- renderPlot({
      req(input$country7, input$region7)
      
      plot_death_crude(c = input$country7, m ="cumulative")
    })
    
    

    

    ########## State HERE -------------
    
    pass_day_dt_sp <- reactive({
      req(input$detail_country, input$aspect4, input$detail_state, input$min_case4)
      
      filtered_state_data(s = input$detail_state, a = input$aspect4, g = input$detail_country, min_case = input$min_case4)
      
    })
    
    output$plt_pass_day_sp <- renderPlot({
      req(#input$level, 
        input$measure4, pass_day_dt_sp(), input$aspect4, tabinfo(),input$min_case4, 
        input$detail_country,input$detail_state)
      # ggplotly(
      plot_day_passed_day(df = pass_day_dt_sp(), m = input$measure4, a = input$aspect4, g = tabinfo(), min_case = input$min_case4
      )#, tooltip = "text") %>%
      #    config(displayModeBar = TRUE) 
    })
    
    output$plt_per_capita_sp <- renderPlot({
      req(#input$level, 
        input$measure4, pass_day_dt_sp(), input$aspect4, tabinfo(),input$min_case4, 
        input$detail_country,input$detail_state)
      # ggplotly(
      plot_per_capita(df = pass_day_dt_sp(), m = input$measure4,  a = input$aspect4, g = tabinfo(), min_case = input$min_case4
      )#, tooltip = "text") %>%
      #    config(displayModeBar = TRUE) 
    })
    
  

    

    
  # output$plt_map_per_capita <- renderImage({
  #   
  #   outfile <- tempfile(fileext='.gif') # temporary file will be removed later by renderImage
  #   
  #   plt <- ggplot()+
  #     borders("world", colour="gray50", fill="gray90") +
  #     geom_point(data = map_dt, alpha = 0.5, aes(x = Long, y = Lat, size = per_capita,
  #                                                color = country_region)) +
  #     scale_color_manual(values = country_color, guide = FALSE) +
  #     scale_size_continuous(name = "Number of Cases", labels = si_vec)+
  #     theme(legend.position = "bottom")+
  #     transition_time(Date) +
  #     labs(x = "Longitude", y = "Latitude", title = str_c("COVID-19 Cases Per 10 Million of Population Date: {frame_time}") )
  #   
  #   anim_save("outfile.gif", animate(plt, nframe = 50, fps = 5, height = 461, width = 644)) 
  #   
  #   list(src = "outfile.gif",contentType = 'image/gif')}, deleteFile = TRUE)
  #   
  #   
  # output$plt_map_incident <- renderImage({
  #   
  #   outfile <- tempfile(fileext='.gif') # temporary file will be removed later by renderImage
  #   
  #   plt <- ggplot()+
  #     borders("world", colour="gray50", fill="gray90") +
  #     geom_point(data = map_dt, alpha = 0.5, aes(x = Long, y = Lat, size = per_capita,
  #                                                color = country_region)) +
  #     scale_color_manual(values = country_color, guide = FALSE) +
  #     scale_size_continuous(name = "Number of Cases", labels = si_vec)+
  #     theme(legend.position = "bottom")+
  #     transition_time(Date) +
  #     labs(x = "Longitude", y = "Latitude", title = str_c("COVID-19 Incident Cases Date: {frame_time}") )
  #   
  #   anim_save("outfile.gif", animate(plt, nframe = 50, fps = 5, height = 461, width = 644)) 
  #   
  #   list(src = "outfile.gif",contentType = 'image/gif')}, deleteFile = TRUE)
  
  
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)



