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

all_data %>%
  mutate(day = as.Date(date, format= "%Y-%m-%d")) %>%
  filter(subregion1_code=="LIM") %>%
  arrange(day) %>%
  as.data.table()


prep_data <- all_data %>%
  mutate(day = as.Date(date, format= "%Y-%m-%d"), 
         country_name=replace(country_name, country_name=="United States of America", "US")) %>%
  filter(subregion1_name=="" & subregion2_name=="", aggregation_level==0) %>%
  distinct(key, country_name, day, new_confirmed, new_deceased, total_confirmed, total_deceased,
           new_tested, total_tested,
           population, population_density, population_male, population_female,
           human_development_index, diabetes_prevalence, smoking_prevalence, comorbidity_mortality_rate, health_expenditure
  ) %>%
  
  # Get daily smoothed cases, deaths over 7 days
  arrange(country_name, day) %>%
  group_by(country_name) %>%
  mutate(s_new_confirmed = roll_mean(new_confirmed, n=7, align = "right", fill = NA),
         s_new_deceased = roll_mean(new_deceased, n=7, align="right", fill=NA)) %>%
  
  # Binarize Developed status by HDI
  mutate(Developed = human_development_index>0.846) %>%
  
  # Get incidence
  mutate(new_case_inc = s_new_confirmed/population, new_death_inc = s_new_deceased/population) %>%
  as.data.table()


prep_data_l1 <- all_data %>%
  mutate(day = as.Date(date, format= "%Y-%m-%d"), 
         country_name=replace(country_name, country_name=="United States of America", "US")) %>%
  filter(aggregation_level==1) %>%
  distinct(key, country_name, subregion1_code, subregion1_name,
           day, new_confirmed, new_deceased, total_confirmed, total_deceased, 
           new_tested, total_tested,
           population
  ) %>%
  
  # Filter out negative value
  mutate(new_deceased = replace(new_deceased, new_deceased<0, 0)) %>%
  
  # Get daily smoothed cases, deaths over 7 days
  arrange(country_name, subregion1_code, day) %>%
  group_by(country_name, subregion1_code) %>%
  mutate(s_new_confirmed = roll_mean(new_confirmed, n=7, align = "right", fill = NA),
         s_new_deceased = roll_mean(new_deceased, n=7, align="right", fill=NA)) %>%
  
  # Get incidence
  mutate(new_case_inc = s_new_confirmed/population, new_death_inc = s_new_deceased/population) %>%
  as.data.table()

function_plot_region <-  function(x, country, level_1=""){
  
  x <- x %>%
    filter(day>"2020-03-01") %>%
    filter(country_name==country) %>%
    as.data.table()
  
  if (level_1!=""){
    x <- x %>%
      filter(subregion1_name==level_1) %>%
      as.data.table()
  }
  
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
function_plot_region(prep_data_l1, "US", "California")
function_plot_region(prep_data_l1, "Peru", "Lima")

prep_data_l1 %>%
  filter(country_name=="US") %>%
  filter(subregion1_name=="Nevada") %>%
  mutate(trend_14_day_perc =  ((s_new_confirmed / lag(s_new_confirmed, n = 13)) - 1)*100 ) %>%
  filter(day>"2020-06-23") %>%
  as.data.table()




p <- prep_data %>%
  filter(total_confirmed>5000 & day>"2020-03-14") %>%
  arrange(country_name, day) %>%
  mutate(day = as.numeric(day - as.Date("2020-03-14"))) %>%
  as.data.table() %>%
  
  ggplot(aes(new_case_inc, new_death_inc, color=Developed, 
             frame=day, size=population_density, label=country_name)) +
    geom_point() +
    geom_abline(slope = 0.01, intercept = 0, linetype="dashed", color="red") +
    # scale_color_viridis_c() +
    scale_color_discrete()
    # theme_wsj()
ggplotly(p)


p <- prep_data %>%
  filter(!is.na(Developed)) %>%
  filter(total_confirmed>=100) %>%
  arrange(country_name, day) %>%
  group_by(country_name) %>%
  mutate(day_since_100 = seq(1:length(key)), 
         max_cases = max(total_confirmed),
         max_day_since_100 = max(day_since_100)
         ) %>%
  mutate(core = country_name %in% c("Peru", "Brazil", "South Africa", "Chile", "Mexico", "US", "Russia")) %>%
  filter(max_cases>20000) %>%
  as.data.table() %>%
  
  ggplot(aes(day_since_100, s_new_confirmed, shape=core, group=country_name, color=Developed)) +
    geom_line() + 
    geom_text(aes(day_since_100, s_new_confirmed,
                        label=ifelse((day_since_100==max_day_since_100 & core==T), country_name, ""))) +
    scale_color_economist() +
    theme_minimal() +
    theme(axis.title=element_text(size=16)) +
    xlab("Days since 100 cases") + ylab("New Cases") #+
    # theme(legend.title=element_blank(),
    #       panel.grid.major.x=element_blank())
gp <- ggplotly(p) 
gp$x$data[[2]]$showlegend <-F
gp$x$data[[3]]$showlegend <-F
gp$x$data[[1]]$showlegend <-F
gp$x$data[[4]]$showlegend <-T
gp$x$data[[5]]$showlegend <-T
gp$x$data[[8]]$showlegend <-F
gp$x$data[[11]]$showlegend <-F
for (i in 1:11){
  print(c(i, gp$x$data[[i]]$legend, gp$x$data[[i]]$showlegend))
  gp$x$data[[i]]$showlegend <- T
}

gp %>%
  layout(annotations= list(yref='paper',xref="paper",y=1.05,x=1.1, text="Developed",showarrow=F))


p<-prep_data %>%
  filter(!is.na(Developed)) %>%
  filter(total_confirmed>=100) %>%
  arrange(country_name, day) %>%
  group_by(country_name) %>%
  mutate(day_since_100 = seq(1:length(key)), 
         max_cases = max(total_confirmed),
         max_day_since_100 = max(day_since_100)
  ) %>%
  mutate(core = country_name %in% c("Peru", "Brazil", "South Africa", "Chile", "Mexico", "US", "Russia")) %>%
  filter(max_cases>20000) %>%
  as.data.table()
  
plot_ly(p, x=~day_since_100) %>%
  add_trace(y=~s_new_confirmed, color=~Developed, split=~country_name, data=p[core==T], 
              name="Core", alpha=1, type="scatter", mode = 'lines+markers', colors="Dark2",
            text = ~paste("Country: ", country_name), showlegend=T, legendgroup="Core") %>%
  add_trace(y=~s_new_confirmed, color=~Developed, split=~country_name, data=p[core==F], 
              name="Others", alpha=0.2, type="scatter", mode = 'lines+markers', colors="Dark2", 
            text=~country_name, showlegend=T, legendgroup="Others") %>%
  layout(
    showlegend = F
  )


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
                box(
                  sliderInput("slider_date", "Date choices:", 
                              as.Date("2020-03-16"), max(prep_data$day), as.Date("2020-03-16"), animate=T)
                  )
                ),
              fluidRow(
                box(plotlyOutput("main_plot", height = 500)),
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
  
  output$main_plot <- renderPlotly({
    data <- prep_data %>% 
      filter(!is.na(new_case_inc)) %>%
      filter(!is.na(new_death_inc)) %>%
      filter(day>"2020-02-15") %>% 
      filter(day==input$slider_date) %>%
    p <- ggplot(data, aes(new_case_inc, new_death_inc, 
                          color=human_development_index, size=population_density, ids=country_name)
           ) +
      geom_point() +
      xlim(0, max(prep_data[!is.na(new_case_inc)]$new_case_inc)) +
      ylim(0, max(prep_data[!is.na(new_death_inc)]$new_death_inc))
    ggplotly(p)
  })
}

shinyApp(ui, server)
