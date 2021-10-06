library(tidyverse)
library(janitor)

pop <- read_csv("Data/pop-by-race-eth-age.csv")
deaths <- read_csv("Data/Provisional_COVID-19_Deaths_by_Race_and_Hispanic_Origin__and_Age.csv")

#oops! "Non-Hispanic" is differently spelled in each table. here's how we changed that:
pop <- pop %>% mutate(race_eth = str_replace(race_eth, "Non Hispanic", "Non-Hispanic"))

deaths <- deaths %>% clean_names()

#INNER JOINS:
#pop is our second table. deaths table comes first in matching section (by = c()). simplified the join with select(). 
#now that the tables are joined, we can calculate a death rate using mutate().
#the joined tables are assigned to a new table, death_rate.
death_rate <- deaths %>%
  inner_join(pop, by = c("state"="name", "age_group", "race_and_hispanic_origin_group"="race_eth")) %>%
  select(state, age_group, race_and_hispanic_origin_group, covid_19_deaths, pop2019) %>%
  mutate(death_rate = covid_19_deaths/pop2019*1000)

#on Monday, our group thought Hispanic groups in new mexico were dying at disproportionate rates. 
#let's see if that's true now that these tables are joined.
hispanicNewMexico <- death_rate %>%
  filter(state == "New Mexico" & race_and_hispanic_origin_group == "Hispanic" | state == "Arizona" & race_and_hispanic_origin_group == "Hispanic")

#ANSWER: not necessarily true. Arizona, another southwestern state, had a higher death rate than New Mexico.