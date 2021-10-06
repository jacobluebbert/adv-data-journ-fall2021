library(tidyverse)
library(janitor)

covid <- read_csv("Data/Provisional_COVID-19_Deaths_by_Race_and_Hispanic_Origin__and_Age.csv")

covid <- covid %>% clean_names()

#Which race has suffered the most COVID-19 deaths?
deaths_by_race <- covid %>%
  filter(!is.na(covid_19_deaths) & state == "United States" & age_group == "All Ages") %>%
  group_by(race_and_hispanic_origin_group) %>%
  summarise(covidDeaths = sum(covid_19_deaths))

deathPercenteByAgeGroup <- covid %>% 
  filter(!is.na(total_deaths) & !is.na(covid_19_deaths)) %>%
  group_by(age_group) %>% 
  summarise(death_rate = (sum(covid_19_deaths)/sum(total_deaths))*100)

#test before you join a table
#table_name > count(things you want to join) > arrange(desc by n)

#by a single field//if the fields have the same name:
#main_table %>% inner_join(second_table, by = "field")

#by a single field // if the fields have different names:
#LOOK IN REPO FOR CODE