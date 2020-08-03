library(data.table)
library(ggplot2)
library(reshape2)
library(plotly)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(gganimate)
library(RcppRoll)
library(ggthemes)
library(ggrepel)
library(zoo)
library(directlabels)
library(ggthemes)
library(gridExtra)
library(cowplot)

all_data <- fread("https://open-covid-19.github.io/data/v2/master.csv")
                  
all_data <- fread("https://storage.googleapis.com/covid19-open-data/v2/main.csv")
all_epi  <- fread("https://storage.googleapis.com/covid19-open-data/v2/epidemiology.csv")
all_hospi <- fread("https://storage.googleapis.com/covid19-open-data/v2/hospitalizations.csv") 
all_age  <- fread("https://storage.googleapis.com/covid19-open-data/v2/by-age.csv")
all_demo <- fread("https://storage.googleapis.com/covid19-open-data/v2/demographics.csv")
all_geo  <- fread("https://storage.googleapis.com/covid19-open-data/v2/geography.csv")
all_resp <- fread("https://storage.googleapis.com/covid19-open-data/v2/oxford-government-response.csv")
all_bank <- fread("https://storage.googleapis.com/covid19-open-data/v2/worldbank.csv")
all_health <- fread("https://storage.googleapis.com/covid19-open-data/v2/health.csv")

all_geo %>% filter(key=="US_CA") #area
all_demo %>% filter(key=="US_CA") #pop older

all_demo_age <- all_demo %>%
  group_by(key) %>%
  summarise(pop_young = sum(c(population_age_00_09, population_age_10_19), na.rm = T)/population,
            pop_mid = sum(c(population_age_20_29, population_age_30_39, population_age_40_49, population_age_50_59), na.rm = T)/population,
            pop_old = sum(c(population_age_60_69, population_age_70_79, population_age_80_89, population_age_90_99), na.rm = T)/population
            ) %>%
  as.data.table()

all_data %>%
  filter(country_name=="Brazil") %>%
  filter(aggregation_level==0)

all_epi %>%
  filter(key=="US_CA")


prep_data <- all_data %>%
  filter(total_confirmed>10) %>%
  mutate(day = as.Date(date, format= "%Y-%m-%d"), 
         country_name=replace(country_name, country_name=="United States of America", "US")) %>%
  distinct(key, country_name, subregion1_name, subregion2_name, locality_name,
           day, new_confirmed, new_deceased, total_confirmed, total_deceased,
           new_tested, total_tested,
           population, population_density, population_male, population_female,
           human_development_index, diabetes_prevalence, smoking_prevalence, comorbidity_mortality_rate, health_expenditure
  ) %>%
  
  # Clean up (negatives..)
  mutate(new_deceased = replace(new_deceased, new_deceased<0, 0)) %>%
  mutate(new_confirmed = replace(new_confirmed, new_confirmed<0, 0)) %>%
  
  # Get daily smoothed cases, deaths and tests over 7 days
  arrange(country_name, subregion1_name, subregion2_name, locality_name, day) %>%
  group_by(country_name, subregion1_name, subregion2_name, locality_name) %>%
  mutate(s_new_confirmed = roll_mean(new_confirmed, n=7, align = "right", fill = 0, na.rm = T),
         s_new_deceased = roll_mean(new_deceased, n=7, align="right", fill=0, na.rm = T),
         s_new_tested = roll_mean(new_tested, n=7, align="right", fill=NA),
         trend14 = (s_new_confirmed - lag(s_new_confirmed, n = 14, default = NA)) / lag(s_new_confirmed, n = 14, default = NA),
         std_confirmed = (s_new_confirmed - min(s_new_confirmed)) /(max(s_new_confirmed) - min(s_new_confirmed)) ) %>%
  
  # Binarize Developed status by HDI
  mutate(Developed = human_development_index>0.846) %>%
    
  # Get incidence
  mutate(new_case_inc = s_new_confirmed/population, new_death_inc = s_new_deceased/population) %>%
  arrange(country_name, subregion1_name, subregion2_name, locality_name,  day) %>%
  group_by(country_name, subregion1_name, subregion2_name, locality_name) %>%
  mutate(inc_14_day = roll_sum(s_new_confirmed, n=14, align="right", fill=NA, na.rm = T)/population,
         inc_30_day = roll_sum(s_new_confirmed, n=30, align="right", fill=NA, na.rm = T)/population,
         case_14_day = roll_sum(s_new_confirmed, n=14, align="right", fill=NA, na.rm = T),
         test_14_day = roll_sum(s_new_confirmed, n=14, align="right", fill=NA) / roll_sum(s_new_tested, n=14, align="right", fill=NA)) %>%
  as.data.table()

prep_data %>%
  # distinct(country_name)
  filter(country_name=="Peru" & subregion1_name=="" & subregion2_name=="" & locality_name=="")


function_shift_cor <- function(x, country, min_cases){
  
  # Get states
  if (country!="global"){
    states <- prep_data %>% filter(country_name==country & subregion1_name!="" & subregion2_name=="" & locality_name=="") %>%
      distinct(subregion1_name) %>% pull(subregion1_name)
  }
  
  x <- x %>%
    filter(country_name==country & subregion1_name!="" & subregion2_name=="" & locality_name=="") %>%
    filter(total_confirmed > min_cases) %>%
    mutate(pos_rate = s_new_confirmed/s_new_tested)
  
  main_table <- data.table()
  for (i in 1:22){
    print(i)
    i_table <- x %>%
      group_by(key, subregion1_name) %>%
      mutate(s = shift(pos_rate, n = i, fill=NA)) %>%
      summarise(cor_day = cor(s, s_new_deceased, use="pairwise.complete.obs")) %>%
      as.data.table() %>% mutate(d_shift=i)
    
    main_table <- rbind(main_table, i_table)
  }
  
  return(main_table)  
}

us_pos_death_shift <- function_shift_cor(prep_data, country = "US", min_cases = 20000)

us_pos_death_shift %>%
  ggplot(aes(d_shift, subregion1_name, fill=cor_day)) +
  geom_tile() +
  scale_fill_gradient2(low = "seagreen3", mid = "khaki1", high="firebrick3", midpoint = 0)

us_pos_death_shift %>%
  group_by(subregion1_name) %>%
  mutate(count_na = sum(is.na(cor_day))) %>%
  filter(count_na <6) %>% 
  as.data.table() %>% 
  ggplot(aes(factor(d_shift), cor_day)) +
  geom_boxplot()

us_pos_death_shift %>%
  merge(prep_data %>% 
          distinct(key, population, population_male, population_density, human_development_index,
                   smoking_prevalence, diabetes_prevalence, comorbidity_mortality_rate, health_expenditure),
        by=c("key")
        ) %>%
  merge(all_demo_age, by="key") %>%
  mutate(pop_male = population_male/population) %>%
  
  group_by(subregion1_name) %>%
  mutate(count_na = sum(is.na(cor_day))) %>%
  filter(count_na <6) %>% 
  group_by(subregion1_name) %>%
  summarise(max_day = d_shift[which.max(cor_day)], max_cor=max(cor_day, na.rm = T),
            pop_young, pop_mid, pop_old, pop_male) %>% 
  distinct(.) %>%
  as.data.table() %>%
  
  # Plot
  ggplot(aes(max_day, max_cor, label=subregion1_name, color=pop_old)) +
  geom_point(size=3) +
  geom_text_repel() + 
  scale_color_gradient2(low = "seagreen3", mid = "khaki1", high="firebrick3", midpoint = 0.25) +
  theme_dark()
  

prep_data %>%
  filter(subregion1_name=="California" & subregion2_name=="" & locality_name=="") %>%
  filter(total_confirmed>5000) %>%
  mutate(pos_rate = s_new_confirmed/s_new_tested) %>%
  ggplot(aes(x=day)) + 
    geom_line(aes(y=pos_rate, color="pos_rate")) +
    geom_line(aes(y=s_new_deceased, color="deaths")) +
    scale_y_log10()


function_plot_std <- function(x, x_target, today){
  
  x <- merge(x, x_target, by=c("country_name", "subregion1_name", "subregion2_name", "locality_name")) %>%
    mutate(label = paste0(country_name, "-", subregion1_name, "-", subregion2_name, "-", locality_name)) %>%
    filter(day < today) %>%
    
    # Get latest incidence value to sort facets by
    arrange(country_name, subregion1_name, subregion2_name, locality_name, day) %>%
    group_by(country_name, subregion1_name, subregion2_name, locality_name) %>%
    mutate(latest_value = last(inc_14_day)) %>%
    filter(!is.na(latest_value)) %>%
    as.data.table()
  
  # PLOTS
  std_plot <- x %>%
    mutate(label = reorder(label, -latest_value)) %>%
    
    ggplot(aes(x=day, y=std_confirmed, fill=std_confirmed)) +
    geom_histogram(stat="identity") +
    facet_grid(rows = vars(label)) +
    theme_bw() +
    theme(panel.spacing.y=unit(-0.5, "lines"),
          panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          strip.text.y = element_text(size=10, color="black",  angle=0),
          strip.background.y = element_rect(color="white", fill="white"),
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    scale_fill_gradient2(low = "seagreen3", mid = "khaki1", high="firebrick3", midpoint = 0.5)
  
  inc_loli_plot <- x %>%
    distinct(label, latest_value, population) %>%
    mutate(label = reorder(label, latest_value)) %>%
    mutate(latest_value = latest_value*26) %>%
    
    ggplot(aes(label, latest_value, label=round(latest_value,1))) +
    geom_point(stat='identity', color="firebrick3", aes(size=population))  +
    geom_segment(aes(y = 0, 
                     x = label, 
                     yend = latest_value, 
                     xend = label), 
                 color = "black") +
    geom_text(color="white", size=4) +
    coord_flip() +
    theme_minimal() + ylab("Annualized Incidence") + #ylab("14-Day Incidence in 1M") +
    theme(axis.text.x = element_text(size=12), axis.title.y = element_blank(),
          axis.title.x = element_text(size=14), axis.text.y = element_text(size=12)) +
    scale_size(range = c(8, 20))
  
  return(plot_grid(std_plot, inc_loli_plot))
}

function_extract_target_table <- function(x, country="Peru"){
  
  x <- x %>%
    filter(country_name==country) %>%
    distinct(country_name, subregion1_name, subregion2_name, locality_name) %>%
    filter(subregion2_name=="" & locality_name=="" & subregion1_name!="")
  
  return(x)
}

prep_data %>% filter(country_name=="Peru") %>% 
  filter(subregion1_name=="Lima Region")
  
function_extract_target_table(prep_data, "Peru")

target_table <- as.data.table(
  rbind(c("US", rep("",3)),
        c("Italy", rep("",3)),
        c("Peru", rep("",3)),
        c("Spain", rep("",3)),
        c("France", rep("",3)),
        c("United Kingdom", rep("",3)),
        c("US", "California", rep("",2)),
        c("US", "Florida", rep("",2)),
        c("US", "Arizona", rep("",2)),
        c("US", "New York", rep("",2)),
        c("Peru", "Lima Province", rep("",2)),
        c("Peru", "Arequipa", rep("",2))
  ),
) %>%
  rename(country_name="V1", subregion1_name="V2", subregion2_name="V3", locality_name="V4")

function_plot_std(prep_data, target_table,
                  today = "2020-07-31"
                  )

function_plot_std(prep_data, function_extract_target_table(prep_data, "Brazil"), today = "2020-07-30") 

function_country_inc_cor <- function(x, total_th=100, pop_size=5e6){
  
  x <- x %>%
    # Only filter for countries
    filter(subregion1_name=="" & subregion2_name=="" & locality_name=="") %>%
    filter(population>=pop_size) %>%
    distinct(country_name, day, total_confirmed, new_case_inc, new_death_inc) %>%
    
    # Minimum number of cum cases
    group_by(country_name) %>%
    mutate(max_cases = max(total_confirmed)) %>%
    filter(max_cases >= total_th) %>%
    select(-max_cases) %>%
    
    # Start at 100 cases
    filter(total_confirmed>=500) %>% #filter(-total_confirmed) %>%
    group_by(country_name) %>%
    mutate(n_count = 1:length(day)) %>%
    as.data.table()
    
  # Get combined case death matrix
  case_matrix  <- x %>% acast(n_count~country_name, value.var = "new_case_inc", fill = NA)
  death_matrix <- x %>% acast(n_count~country_name, value.var = "new_death_inc", fill = NA)
  
  x <- rbind(case_matrix,
             death_matrix[,colnames(case_matrix)]
             )
  
  # Obtain correlation
  x_cor <- x %>%
    cor(use = "pairwise.complete.obs", method = "pearson") %>%
    melt() %>%
    as.data.table() %>%
    
    # Clean up (remove NAs, no same country)
    rename(country_1=Var1, country_2=Var2, corr=value) %>%
    filter(country_1!=country_2) %>%
    filter(!is.na(corr)) %>%
    arrange(-corr)
  
  # Obtain k-means
  x_kmeans <- case_matrix[complete.cases(case_matrix),]
  x_kmeans <- kmeans(t(x_kmeans), centers = 8, iter.max = 100)
  
  return(list(cor=x_cor, k=x_kmeans))
}

country_cor <- function_country_inc_cor(prep_data, total_th = 1000, pop_size = 10e6)

as.data.table(t(data.frame(t(country_cor$k$cluster))), keep.rownames = T) %>%
  rename(country=rn, cluster=V1) %>%
  arrange(cluster)


country_cor$cor %>% filter(country_1=="US" & country_2=="United Kingdom")



function_data_network <- function(x, node_attr){
  require(tibble)
  
  # Make sure all are in node_attr
  x <- x %>% 
    filter(country_1 %in% unique(node_attr$label)) %>% 
    filter(country_2 %in% unique(node_attr$label))
  
  # Remove duplicates
  all_countries <- unique(x$country_1)
  combinations  <- data.table(t(combn(all_countries, 2, simplify = T))) %>%
    rename(country_1 = V1, country_2=V2)
  
  x <- merge(x, combinations, by=c("country_1", "country_2")) %>%
    rename(weight=corr)
  
  # Convert to network object
  sources <- x %>%
    distinct(country_1) %>%
    rename(label = country_1)
  
  destinations <- x %>%
    distinct(country_2) %>%
    rename(label = country_2)
  
  # Nodes
  nodes <- full_join(sources, destinations, by = "label") %>% 
    rowid_to_column("id")
  
  # Edges
  edges <- x %>% 
    left_join(nodes, by = c("country_1" = "label")) %>% 
    rename(from = id)
  
  edges <- edges %>% 
    left_join(nodes, by = c("country_2" = "label")) %>% 
    rename(to = id)
  
  edges <- edges %>% select(from, to, weight)
  
  # Add additional node attributes
  # nodes <- nodes %>% merge(node_attr, by="label")
  print(nodes[1:5,])
  # Create network
  country_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE, 
                             directed = F, loops = F, multiple = F)

  return(country_network)
}


nodes_attr <- prep_data %>%
  filter(subregion1_name=="" & subregion2_name=="" & locality_name=="") %>%
  distinct(label=country_name, population, human_development_index)

country_cor_net <- function_data_network(country_cor$cor %>% filter(corr>0.2), 
                                         node_attr = nodes_attr)

country_cor_net
summary(country_cor_net)

plot(country_cor_net)

# install.packages("ggnewscale")
library(ggnewscale)
p <- ggplot(ggnetwork(country_cor_net, weights="weight") %>%
              as.data.table() %>%
              merge(nodes_attr, by="label"),
       aes(x = x, y = y, xend = xend, yend = yend, label=label, alpha=factor(weight))) +
  geom_edges() + 
  # scale_colour_gradient2(low = "white", mid = "skyblue", high="blue", midpoint = 0.6) +
  # scale_colour_gradient(low = "white", high="darkgrey") +
  # new_scale_colour() +
  geom_nodes(aes(colour=human_development_index,size=population)) +
  scale_colour_gradient2(low = "seagreen3", mid = "khaki1", high="firebrick3", midpoint = 0.7) +
  scale_size(range = c(5, 15)) +
  theme_blank()
  
  

ggplotly(p)


install.packages("ggnetwork")
library(ggnetwork)
library(ITNr)
library(intergraph)
library(network)
library(igraph)
library(sna)




install.packages("networkD3")
library(networkD3)
country_cor_net <- graph.data.frame(country_cor %>% filter(corr>0.3), directed = F)




# Basic Graph
library(randomNames)
random_name_df  <- function(nl=100, size=1000, smpl=10, seed=221){
  df = data.frame(source = randomNames(size,which.names='both', name.order = 'first.last', name.sep=' '), target = '')
  df = df[rep(seq_len(nrow(df)), sample(1:smpl,nrow(df), replace=T)),]
  df = df[sample(nrow(df),nl),] 
  df$target = sample(df$source,nrow(df), replace = T)
  df = df[df[,1]!=df[,2], ] 
  return(df)
}

df <- random_name_df(seed=221)
g <- graph.data.frame(df, directed=F) # raw graph

## Make a vertices df
vertices<-data.frame(
  name = V(g)$name,
  group = edge.betweenness.community(g)$membership
  # betweenness = (betweenness(g,directed=F,normalized=T)*115)+0.1 #so size isn't tiny
) 
#nb. can also adjust nodesize with `radiusCalculation`

# create indices (indexing needs to be JS format)
df$source.index = match(df$source, vertices$name)-1
df$target.index = match(df$target, vertices$name)-1

df$weight <- c(sample(seq(0.1, 0.3, 0.001), nrow(df)/2), sample(seq(0.8, 1.0, 0.001), (nrow(df)/2)+1))

df
d3 = forceNetwork(Links = df, Nodes = vertices, Value = "weight",
                  Source = 'source.index', Target = 'target.index',
                  NodeID = 'name',
                  Group = 'group', # color nodes by group calculated earlier
                  # charge = -50, # node repulsion
                  linkDistance = 20,
                  zoom = T, 
                  opacity = 1,
                  fontSize=24)

show(d3)

#####
df <- read.csv("https://raw.githubusercontent.com/bcdunbar/datasets/master/parcoords_data.csv")

fig <- df %>%
  plot_ly(width = 1000, height = 600) 
fig <- fig %>% add_trace(type = 'parcoords',
                         line = list(color = ~colorVal,
                                     colorscale = 'Jet',
                                     showscale = TRUE,
                                     reversescale = TRUE,
                                     cmin = -4000,
                                     cmax = -100),
                         dimensions = list(
                           list(range = c(~min(blockHeight),~max(blockHeight)),
                                constraintrange = c(100000,150000),
                                label = 'Block Height', values = ~blockHeight),
                           list(range = c(~min(blockWidth),~max(blockWidth)),
                                label = 'Block Width', values = ~blockWidth),
                           list(tickvals = c(0,0.5,1,2,3),
                                ticktext = c('A','AB','B','Y','Z'),
                                label = 'Cyclinder Material', values = ~cycMaterial),
                           list(range = c(-1,4),
                                tickvals = c(0,1,2,3),
                                label = 'Block Material', values = ~blockMaterial),
                           list(range = c(~min(totalWeight),~max(totalWeight)),
                                visible = TRUE,
                                label = 'Total Weight', values = ~totalWeight),
                           list(range = c(~min(assemblyPW),~max(assemblyPW)),
                                label = 'Assembly Penalty Weight', values = ~assemblyPW),
                           list(range = c(~min(HstW),~max(HstW)),
                                label = 'Height st Width', values = ~HstW),
                           list(range = c(~min(minHW),~max(minHW)),
                                label = 'Min Height Width', values = ~minHW),
                           list(range = c(~min(minWD),~max(minWD)),
                                label = 'Min Width Diameter', values = ~minWD),
                           list(range = c(~min(rfBlock),~max(rfBlock)),
                                label = 'RF Block', values = ~rfBlock)
                         )
)


fig


fread("https://raw.githubusercontent.com/bcdunbar/datasets/master/iris.csv") %>% 
  plot_ly(type = 'parcoords',
    line = list(color = ~species_id,
                colorscale = list(c(0,'red'),c(0.5,'green'),c(1,'blue'))),
    dimensions = list(
      list(range = c(2,4.5),
           label = 'Sepal Width', values = ~sepal_width),
      list(range = c(4,8),
           constraintrange = c(5,6),
           label = 'Sepal Length', values = ~sepal_length),
      list(range = c(0,2.5),
           label = 'Petal Width', values = ~petal_width),
      list(range = c(1,7),
           label = 'Petal Length', values = ~petal_length)
    )
)





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

function_plot_region(prep_data, "Spain")
function_plot_region(prep_data, "Peru")
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
                    boxPlus(width = 8,
                        fluidRow(
                          column(3,
                                 selectInput("input_case_inc_g", "Display:", choices=c("Cases", "Incidence"))
                                 ),
                          column(3,
                                 selectInput("input_case_inc_rank_g", "Region selected by:", 
                                        choices= c("Highest cases"="cases", "Highest incidence"="incidence", 
                                                   "Percent of tested"="test_ratio", "14-Day trend" = "trend"))
                                 ),
                          column(3,
                                 sliderInput("input_case_slider_pop_g", "Minimum population:", min=0,
                                             max=20e6, value = 5e6, step = 1e6)
                                 )
                          ),
                        plotlyOutput("country_incidence")
                        ),
                    box(width = 4,
                        function_plot_trend(prep_data, countries = c("US", "Denmark", "Colombia", "Italy"))
                        )
                    ),
                  
                  tabPanel(
                    title="US State Incidence",
                    fluidRow(
                      column(3,
                             selectInput("input_case_inc_s", "Display:", choices=c("Cases", "Incidence"))
                             ),
                      column(3,
                             selectInput("input_case_inc_rank_s", "Region selected by:", 
                                    choices= c("Highest cases"="cases", "Highest incidence"="incidence", 
                                               "Percent of tested"="test_ratio", "14-Day trend" = "trend"))
                             ),
                      column(3,
                             sliderInput("input_case_slider_pop_s", "Minimum population:", min=0,
                                         max=20e6, value = 5e6, step = 1e6)
                             )
                      ),
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
                                                                choice = input$input_case_inc_rank_g, 
                                                                pop_th = input$input_case_slider_pop_g),
                           incidence = (input$input_case_inc_g=="Incidence")
    )
  })
  
  output$us_state_incidence <- renderPlotly({
    function_plot_case_inc(prep_data, 
                           countries=c("US"),
                           level_1 = function_select_top_region(prep_data, global = F, country = "US", n=7,
                                                                choice = input$input_case_inc_rank_s, 
                                                                pop_th = input$input_case_slider_pop_s),
                           incidence = (input$input_case_inc_s=="Incidence")
    )
  })
}

shinyApp(ui, server)

