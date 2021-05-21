#Libraries
library("dplyr")
library("knitr")
library("ggplot2")
library("plotly")
library("leaflet")

#Dataset
mass_shootings_2018 <- read.csv("data/shootings-2018.csv")

#Summary Information
#How many shootings occurred?
number_of_mass_shootings <- nrow(shootings_2018)

#How many lives were lost?
number_of_lives_lost <- sum(shootings_2018$num_killed)

#Which city was most impacted by shootings?
city_most_impacted <- shootings_2018 %>%
  group_by(city) %>%
  summarize(total_victims_killed = sum(null.killed), total_victims_injured = 
              sum(null.injured)) %>%
  mutate(total_victims = total_victims_killed + total_victims_injured) %>%
  filter(max(total_victims))
  pull(city)
  
#Two other insights of your choice
#State with most shootings
  most_shootings <- shootings_2018 %>%
    group_by(state) %>%
    tally() %>%
    filter(max(n))
  pull(state)

#State with least shootings
  least_shootings <- shootings_2018 %>%
    group_by(state) %>%
    tally() %>%
    filter(min(n))
  pull(state)
  
#Summary Table
#Make table
  summary_table <- kable(summary_df, 
                         col.names = c("State", "Number of Shootings",
                                       "Number Killed"),
                         align = 'c')
  
#Data for state
  data_for_state_df <- shootings_2018 %>%
    group_by(state)
    summarize(total_victims_killed = sum(null.killed), total_victims_injured = 
                sum(null.injured)) %>%
    arrange(desc(number_of_mass_shootings))
  

#Interactive Map
  mass_shooting_map <- leaflet(data = total_victims) %>% {
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -122.3321, lat = 47.6062, zoom = 10) %>%
    addCircleMarkers(lat = ~Latitude,
                     lng = ~Longitude,
                     stroke = FALSE,
                     radius = ~total_victims
                     color = "000000",
                     size = 15,
                     popup = marker_popups
                     )
  }
  
#Text markers pop up when hovered over
  marker_popups <- paste0(total_victims$date, ",", sep = " ",
                          total_victims$state, ",", sep = " ",
                          total_victims$city, ",", sep = " ")
  
  
#Plot of my choice- What is the relationship between mass shootings and 
#month of the year?
#Find information for shootings categorized per month
  mass_shootings_per_month <- mass_shootings_2018 %<%
    mutate(month = (substr(date, 1, 3)) %>%
    group_by(month) %>%
    tally() %>%
    arrange(match(month, c("January", "February", "March", "April", "May", 
                           "June", "July", "August", "September", "October",
                           "November", "December")))
    )
  
#Bar graph for relationship between mass shootings and months of the year
  mass_shootings_per_month <- ggplot(data = mass_shootings_per_month) +
    geom_bar(mapping = aes(x = month, y = n)) +
    labs(title = "Mass Shootings per Month in the US",
         x = "Month",
         y = "Shootings")
  
