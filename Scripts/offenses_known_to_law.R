#first thing you should do in every R script. remember ctrl + enter runs line of code. janitor is used to clean table names into a more usable form.
library(tidyverse)
library(janitor)

#press tab to auto-fill file names! view([variable here]) shows data table
crimes <- read_csv("Data/Offenses_Known_to_Law_Enforcement_by_State_by_City_2019.csv")

#janitor function!
crimes <- crimes %>% clean_names()

#sorting and finding the largest populations. select keeps only certain columns. arrange sorts by biggest/smallest.
crimes %>% select(state, city, population) %>% arrange(desc(population))

#pressing enter after a pipe keeps code compact. filter, well, filters out certain criteria and shows those only.
crimes %>% filter(state == "NEW YORK") %>%
  select(state, city, population, violent_crime) %>%
  arrange(desc(violent_crime))

#what are the urban populations by state?
crimes %>% group_by(state) %>%
  summarize(sum_pop = sum(population)) %>%
  arrange(desc(sum_pop))

#calculating a murder rate using mutate.
crimes %>% filter(population > 100000) %>%
  mutate(murder_rate = murder_and_nonnegligent_manslaughter/population*100000) %>%
  select(state, city, murder_rate) %>%
  arrange(desc(murder_rate))

#Which city had the highest rate in violent crime?
crimes %>% filter(population > 100000) %>%
  mutate(violent_crime_rate = violent_crime/population*100000) %>%
  select(state, city, violent_crime_rate) %>%
  arrange(desc(violent_crime_rate))

#How many motor vehicle thefts have there been in total? (is.na to filter NA values)
crimes %>% 
  filter(!is.na(motor_vehicle_theft)) %>%
  summarise(total_mvt = sum(motor_vehicle_theft))

#How many cities have more than 50 murders?
crimes %>%
  mutate(more_than_fifty = murder_and_nonnegligent_manslaughter > 50) %>%
  select(state, city, population, more_than_fifty) %>%
  filter(more_than_fifty == TRUE) %>%
  summarise(how_many_with_fifty = sum(more_than_fifty))