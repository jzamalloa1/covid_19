library(data.table)
library(ggplot2)
library(reshape2)
library(plotly)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(gganimate)
library(RcppRoll)
library(ggthemes)
library(ggrepel)
library(zoo)
library(directlabels)

all_data <- fread("https://open-covid-19.github.io/data/v2/master.csv")

all_data <- fread("https://storage.googleapis.com/covid19-open-data/v2/main.csv")
all_epi  <- fread("https://storage.googleapis.com/covid19-open-data/v2/epidemiology.csv")
all_hospi <- fread("https://storage.googleapis.com/covid19-open-data/v2/hospitalizations.csv") 
all_age  <- fread("https://storage.googleapis.com/covid19-open-data/v2/by-age.csv")

all_hospi %>%
  filter(key=="US_CA") %>%
  as.data.table()

all_age %>%
  filter(grepl("US", key)) %>%
  distinct(key) %>%
  as.data.table()

prep_data <- all_data %>%
  mutate(day = as.Date(date, format= "%Y-%m-%d"), 
         country_name=replace(country_name, country_name=="United States of America", "US")) %>%
  distinct(key, country_name, subregion1_name, subregion2_name,
           day, new_confirmed, new_deceased, total_confirmed, total_deceased,
           new_tested, total_tested,
           population, population_density, population_male, population_female,
           human_development_index, diabetes_prevalence, smoking_prevalence, comorbidity_mortality_rate, health_expenditure
  ) %>%
  
  # Clean up (negatives..)
  mutate(new_deceased = replace(new_deceased, new_deceased<0, 0)) %>%
  
  # Get daily smoothed cases, deaths and tests over 7 days
  arrange(country_name, subregion1_name, subregion2_name,  day) %>%
  group_by(country_name, subregion1_name, subregion2_name) %>%
  mutate(s_new_confirmed = roll_mean(new_confirmed, n=7, align = "right", fill = NA),
         s_new_deceased = roll_mean(new_deceased, n=7, align="right", fill=NA),
         s_new_tested = roll_mean(new_tested, n=7, align="right", fill=NA),
         trend14 = (s_new_confirmed - lag(s_new_confirmed, n = 14, default = NA)) / lag(s_new_confirmed, n = 14, default = NA)) %>%
  
  # Binarize Developed status by HDI
  mutate(Developed = human_development_index>0.846) %>%
    
  # Get incidence
  mutate(new_case_inc = s_new_confirmed/population, new_death_inc = s_new_deceased/population) %>%
  arrange(country_name, subregion1_name, subregion2_name,  day) %>%
  group_by(country_name, subregion1_name, subregion2_name) %>%
  mutate(inc_14_day = roll_sum(s_new_confirmed, n=14, align="right", fill=NA)/population,
         case_14_day = roll_sum(s_new_confirmed, n=14, align="right", fill=NA),
         test_14_day = roll_sum(s_new_confirmed, n=14, align="right", fill=NA) / roll_sum(s_new_tested, n=14, align="right", fill=NA)) %>%
  as.data.table()


function_plot_region <-  function(x, country, level_1="", level_2=""){
  
  x <- x %>%
    filter(day>"2020-03-01") %>%
    filter(country_name==country) %>%
    filter(subregion1_name==level_1) %>%
    filter(subregion2_name==level_2) %>%
    as.data.table()
  
  bars  <- x %>%
    mutate(new_deceased = new_deceased*10) %>%
    distinct(day, new_confirmed, new_deceased) %>%
    melt(id.vars=c("day")) %>%
    as.data.table()
  
  lines <- x %>%
    mutate(s_new_deceased = s_new_deceased*10) %>%
    distinct(day, s_new_confirmed, s_new_deceased) %>%
    melt(id.vars=c("day")) %>%
    as.data.table()

  p <- ggplot() +
    geom_bar(data=bars, aes(day, value, fill=variable), stat="identity", position="dodge", alpha=0.7) +
    
    geom_line(data=lines[variable=="s_new_confirmed"], aes(day, value), color="darkblue", size=1.5) +
    geom_line(data=lines[variable=="s_new_deceased"], aes(day, value), color="darkred", size=1.5) +
    
    geom_line(data=lines[variable=="s_new_confirmed"], aes(day, value/10, color="1% Mortality"), 
              color="darkred", linetype="dashed", size=1.2) +
    
    geom_dl(data=lines[variable=="s_new_confirmed"], aes(day, value/10, label="1% Mortal"),
            method = list(dl.combine("last.points"), cex = 1, hjust=0, vjust=0.1)) +
    
    scale_fill_manual(name="Events", values = c("darkblue", "darkred"), labels=c("Cases", "Deaths")) +
    scale_y_continuous(sec.axis = sec_axis(~./10, name="New Deaths")) +
    theme_minimal() +
    theme(axis.title=element_text(size=16), axis.text = element_text(size=13),
          legend.title = element_text(size=14), legend.text=element_text(size=12)) +
    xlab("Date") + ylab("New Cases")
  
  return(p)
}

function_plot_region(prep_data, "US")
function_plot_region(prep_data, "US", "Arizona")
function_plot_region(prep_data, "Peru", "Lima")
function_plot_region(prep_data, "US", "California", "Los Angeles County")

function_select_top_region <- function(x, global=T, country="US", choice="cases", n=10, pop_th=0){
  # choice: "cases", "incidence", "test_ratio", "trend"
  x <- x %>%
    filter(population > pop_th) %>%
    filter(day == last(day)) %>%
    as.data.table()
  
  if (global==T){
    regions <- x %>%
      filter(subregion1_name=="" & subregion2_name=="") %>%
      mutate(region = country_name) %>%
      as.data.table()
  } else {
    regions <- x %>%
      filter(country_name==country & subregion1_name!="", subregion2_name=="") %>%
      mutate(region = subregion1_name) %>%
      as.data.table()
  }
  
  # Choose sorting criteria
  if (choice=="cases"){
    regions <- regions %>%      
      arrange(-case_14_day) 
  } else if (choice=="incidence"){
    regions <- regions %>%      
      arrange(-inc_14_day) 
  } else if (choice=="test_ratio"){
    regions <- regions %>%
      arrange(-test_14_day)
  } else if (choice=="trend"){
    regions <- regions %>%
      arrange(-trend14)
  }
  regions <- regions %>% pull(region)
   
  
  return(regions[1:n])
}

function_select_top_region(prep_data, global = F, country="US", choice = "trend", pop_th = 5e6)

function_select_top_region(prep_data, global = F, country = "US", n=11,
                           choice = "incidence", pop_th = 5e6)

function_plot_case_inc <- function(x, countries=c("US"), level_1=c(""), level_2=c(""), incidence=F){
  
  # Prep data
  x <- x %>%
    filter(day>"2020-03-01") %>%
    filter(country_name %in% countries) %>%
    filter(subregion1_name %in% level_1) %>%
    filter(subregion2_name %in% level_2) %>%
    mutate(region = paste0(country_name, " - ", subregion1_name, " - ", subregion2_name)) %>%
    mutate(incidence=incidence) %>%
    mutate(numbers = ifelse(incidence ==T, round(new_case_inc*1e5,2), s_new_confirmed)) %>%
    as.data.table()
  
  y_label <- ifelse(incidence ==T, "Cases per 100K", "Cases")
  
  p <- x %>%
    ggplot(aes(day, numbers, colour=region)) +
    geom_line(size=1.1) +
    
    theme_minimal() +
    # scale_colour_brewer("Region", palette = "Set2") +
    scale_color_tableau() +
    labs(color="Region") +
    theme(axis.title=element_text(size=16), axis.text = element_text(size=13),
          legend.title = element_text(size=14), legend.text=element_text(size=12)) +
    xlab("Date") + ylab(y_label)
  
  ggplotly(p)
}

function_plot_case_inc(prep_data, 
                       countries=c("US"),
                       level_1 = function_select_top_region(prep_data, global = F, country = "US", n=7,
                                                            choice = "trend", pop_th = 5e4),
                       incidence = T
                       )

function_plot_test <- function(x, countries=c(""), level_1=c(""), level_2=c("")){
  
  # Prep data
  x <- x %>%
    filter(day>"2020-03-01") %>%
    filter(country_name %in% countries) %>%
    filter(subregion1_name %in% level_1) %>%
    filter(subregion2_name %in% level_2) %>%
    mutate(region = paste0(country_name, " - ", subregion1_name, " - ", subregion2_name)) %>%
    mutate(test_prop = s_new_confirmed/s_new_tested) %>%
    as.data.table()
  
  # Plot
  p <- x %>%
    ggplot(aes(day, test_prop, colour=region)) +
    geom_line(size=1.2) +
    
    theme_minimal() +
    scale_colour_brewer("Region", palette = "Set1") +
    theme(axis.title=element_text(size=16), axis.text = element_text(size=13),
          legend.title = element_text(size=14), legend.text=element_text(size=12)) +
    xlab("Date") + ylab("Positive/Tests")
  
  ggplotly(p)
}


function_plot_test(prep_data, countries = c("US", "Denmark", "Colombia", "Italy"), 
                   level_1 = c("California", "Arizona", "Florida"))

function_plot_trend <- function(x, countries=c(""), level_1=c(""), level_2=c("")){
  
  require(wesanderson)
  # Prep data
  x <- x %>%
    filter(day>"2020-03-01") %>%
    filter(country_name %in% countries) %>%
    filter(subregion1_name %in% level_1) %>%
    filter(subregion2_name %in% level_2) %>%
    mutate(region = paste0(country_name, " - ", subregion1_name, " - ", subregion2_name)) %>%
    group_by(region) %>%
    mutate(last_trend14 = last(trend14)) %>%
    as.data.table()
  
  # Plot
  x$region <- factor(x$region, 
                     levels = x %>% distinct(region, last_trend14) %>% arrange(-last_trend14) %>% pull(region)
                     )
  
  p <- x %>%
    ggplot(aes(day, s_new_confirmed, colour=round(last_trend14,2))) +
    geom_line(size=1.2) +
    facet_wrap(~region, scales="free_y", ncol = 1) +
    
    theme_minimal() +
    # scale_colour_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous")) +
    scale_color_gradient2("Trend (%)", midpoint = 0.3, low="darkgreen", mid="yellow", high = "red", space = "Lab") +
    theme(axis.title=element_text(size=14), axis.text = element_text(size=12),
          legend.title = element_text(size=14), legend.text=element_text(size=12),
          strip.text.x = element_text(size = 14)) +
    xlab("Date") + ylab("New Cases")
  
  ggplotly(p)
}

function_plot_trend(prep_data, countries = c("US", "Denmark", "Colombia", "Italy"), 
                   level_1 = c("California", "Arizona", "Florida", "Louisiana", "New York", "New Jersey"))


prep_data %>%
  filter(subregion1_name=="California") %>%
  as.data.table()




##################################################### APP ############################################

ui <- dashboardPage(
  
  ## Title
  dashboardHeader(title="Covid-19 Analytics"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Insights", tabName = "insights", icon = icon("dashboard")),
      menuItem("Granular", tabName = "granular", icon = icon("th"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "insights",
              fluidRow(
                tabBox(
                  title = "Historical data",
                  id = "historical_tab",
                  width=12,
                  
                  tabPanel(
                    title="Global Incidence",
                    fluidRow(
                      column(5,
                        selectInput("input_case_inc_g", "Display:", choices=c("Cases", "Incidence"))
                        ),
                      column(5,
                        selectInput("input_case_inc_rank_g", "Region selected by:", 
                                    choices= c("Highest cases"="cases", "Highest incidence"="incidence", 
                                               "Percent of tested"="test_ratio", "14-Day trend" = "trend"))
                        )
                      ),
                    plotlyOutput("country_incidence")
                    ),
                  
                  tabPanel(
                    title="US State Incidence",
                    selectInput("input_case_inc_s", "Display:", choices=c("Cases", "Incidence")),
                    selectInput("input_case_inc_rank_s", "Region selected by:", 
                                choices= c("Highest cases"="cases", "Highest incidence"="incidence", 
                                           "Percent of tested"="test_ratio", "14-Day trend" = "trend")),
                    plotlyOutput("us_state_incidence")
                  )
                  
                )
              ),
              fluidRow(
                box(
                  h2("Place holder")
                  ),
                )
              ),
      
      # Second tab content
      tabItem(tabName = "granular",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  
  output$country_incidence <- renderPlotly({
    function_plot_case_inc(prep_data, 
                           countries = function_select_top_region(prep_data, global = T, n=7,
                                                                choice = input$input_case_inc_rank_g, pop_th = 5e6),
                           incidence = (input$input_case_inc_g=="Incidence")
    )
  })
  
  output$us_state_incidence <- renderPlotly({
    function_plot_case_inc(prep_data, 
                           countries=c("US"),
                           level_1 = function_select_top_region(prep_data, global = F, country = "US", n=7,
                                                                choice = input$input_case_inc_rank_s, pop_th = 5e6),
                           incidence = (input$input_case_inc_s=="Incidence")
    )
  })
}

shinyApp(ui, server)

