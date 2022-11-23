library(tidyverse)
library(dplyr)
library(ggplot2)
# The functions might be useful for A4
source("../source/a4-helpers.R")


## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>

data <- read.csv("incarceration_trends.csv", nrows = 10000, stringsAsFactors = FALSE)

jailAverage <- data %>%
  filter(year == 2002) %>%
  summarize(averageNum = mean(total_jail_pop, na.rm = TRUE)) %>%
  pull(averageNum)

blackPrisonAverage <- data %>%
  summarize(averageValue = mean(black_prison_pop, na.rm = TRUE)) %>%
  pull(averageValue)
whitePrisonAverage <- data %>%
  summarize(averageValue = mean(white_prison_pop, na.rm = TRUE)) %>%
  pull(averageValue)


#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#

# This function ... <todo:  update comment>
# This function produce a bar chart shows the growth of the U.S. prison population from 1970 to 2018
get_year_jail_pop <- function() {
  # TODO: Implement this function
  total_jail_pop_year <- data %>%
    group_by(year) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(total_jail_pop_year)
}

# This function produce a bar chart shows the growth of the U.S. prison population from 1970 to 2018
plot_jail_pop_for_us <- function() {
  # TODO: Implement this function
  total_jail_pop_chart <- get_year_jail_pop() %>%
   
     ggplot(total_jail_pop_year, mapping = aes(x = year, y = total_jail_pop)) +
    geom_col() +
    labs(
      title = "Increase of Jail Population in U.S. from (1970-2018)",
      x = "Year",
      y = "Total Population Jail"
    )
  return(total_jail_pop_chart)   
}
plot_jail_pop_for_us()
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_year_jail_pop_states <- function(states) {
   growth_by_state <- data %>%
     filter(state %in% states) %>%
     group_by(state, year) %>%
     summarise(year_jail_pop = sum(total_pop,na.rm = TRUE) )
   return(growth_by_state)
  }

state_vector <- c("AL", "CA", "AR")
plot_jail_pop_for_states <- function(states) {
jail_pop_chart <- ggplot(get_year_jail_pop_states(states)) + 
geom_line(mapping = aes(x =year, y=year_jail_pop, color = state)) + 
labs(x = "Year", y = "jail Population Per Year", title = "Yearly Growth of Prison
       Population", caption = "Yearly Growth of Prison Population from 1970-2018 in
       the Selected States")
return(jail_pop_chart)
  
  
}

plot_jail_pop_for_states(state_vector)









## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_female_pop_by_state <- function() {
  female_data <- data %>% 
    group_by(year, state) %>% 
    summarise(female_pop = sum(female_jail_pop, na.rm = TRUE))
  return(female_data)
}

plot_female_pop_by_state <- function() {
  chart<- ggplot(data = get_female_pop_by_state(), 
                   aes(x = year, y = female_pop, group = state)) +
    geom_line(aes(color = state)) +
    xlim(2000, 2018) +
    xlab("Year") +
    ylab("Female Incarcerated Population") +
    ggtitle("Female Incarcerated Population by State (2000-2018)")
  
  return(chart)
}

plot_female_pop_by_state()




## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_2010_female_by_states <-function(){
  male <- filter(data, year == 2010) %>%
    select(state, female_jail_pop, total_jail_pop) %>%
    group_by(state) %>%
    summarise(female_pop = sum(female_jail_pop, na.rm = TRUE),
              total_pop = sum(total_jail_pop, na.rm = TRUE)
    ) %>%
    mutate(female_proportion = female_pop/ total_pop *100) %>%
    select(state, female_proportion) %>%
    mutate(state = tolower(abbr2state(state)))
  return(female)
    
}


get_mapping_data <- function(){
  state_shape <- map_data("state") %>%
    rename(state = region) %>%
    left_join(get_2010_female_by_states(), by="state")
  return(state_shape)
}

plot_2010_female_by_states <-function() {
  plot <-ggplot(get_mapping_data()) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = female_proportion),
      color = "black"
       ) +
    coord_map()+
    scale_fill_continuous(low = "black", high = "white") +
    labs(fill = "Female proportion",
    title = "Female proportion in jail in each state in 2010"
         )
         return(plot)
}

## Load data frame ---- 



