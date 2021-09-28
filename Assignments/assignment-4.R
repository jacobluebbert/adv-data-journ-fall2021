#importing libraries
library(tidyverse)
library(janitor)

#importing data
drugs <- read_delim("Data/drug&alcohol_deaths_by_state-age-year.txt", delim = "\t")

#cleaning data names with janitor
drugs <- drugs %>%
  clean_names()

#pulling out the notes from my data
drugs <- drugs %>%
  filter(is.na(notes)) %>%
  select(-notes)

#1. Which state had the most drug and alcohol deaths in 2019?

#changing deaths to numbers only
drugs <- drugs %>%
  mutate(newDeaths = as.numeric(deaths))

#finding state with most deaths in 2019
twentyNineteenDeaths <- drugs %>% filter(!is.na(newDeaths) & year == "2019") %>%
  group_by(state, year) %>%
  summarise(state_deaths = sum(newDeaths))

#ANSWER: California had the most drug and alcohol deaths in the United States in 2019 with 291,074 deaths.

#2. Which year had the highest death rate from 2015 to 2019?

#changing population to numbers only
drugs <- drugs %>%
  mutate(newPop = as.numeric(population))

#finding the death rate by year
deathRateByYear <- drugs %>% 
  filter(!is.na(newPop) & !is.na(newDeaths)) %>%
  group_by(year) %>% 
  summarise(death_rate = (sum(newDeaths)/sum(newPop))*100000)

#ANSWER: 2019 had the highest death rate, with 205 drug/alcohol deaths per 100,000 people.

#3. Which state had the highest percent in death rate from 2015 to 2019?

#finding state death rates in 2015 and 2019
deathRateByState <- drugs %>% 
  filter(!is.na(newPop) & !is.na(newDeaths) & year %in% c("2015", "2019")) %>%
  group_by(state, year) %>%
  summarise(death_rate = (sum(newDeaths)/sum(newPop))*100000)

#finding percent change from 2015 to 2019
deathRateByState <- deathRateByState %>% 
  pivot_wider(names_from = year, values_from = death_rate) %>%
  mutate(pct_chg = (`2019` - `2015`)/`2015`) %>%
  arrange(desc(pct_chg))

#ANSWER: West Virginia led the way with 30.6% more deaths in 2019 than in 2015.

#4. How many people died of unintentional drug overdose in 2019?

#finding all unintentional drug overdose deaths in 2019
twentyNineteenOverdoseDeaths <- drugs %>% 
  filter(!is.na(newDeaths) & year == "2019" & mcd_drug_alcohol_induced_cause_code == "D2") %>%
  group_by(year) %>%
  summarise(overdose_deaths = sum(newDeaths))

#ANSWER: There were 4,435 unintentional drug overdose deaths in 2019.

#5. Which age group had the most unintentional drug overdose deaths in 2019?

#finding unintentional drug overdose deaths by age in 2019
twentyNineteenOverdoseDeathsByAge <- drugs %>% 
  filter(!is.na(newDeaths) & year == "2019" & mcd_drug_alcohol_induced_cause_code == "D2") %>%
  group_by(year, ten_year_age_groups) %>%
  summarise(age_deaths = sum(newDeaths))

#ANSWER: 55-64 year olds had the most deaths in 2019 due to unintentional drug overdoses.

#GRAF ONE
#West Virginia experienced the largest growth in accidental drug overdoses from 2015 to 2019. 
#From 2015 to 2019, West Virginia experienced a 30.6% increase in accidental overdose deaths.
#In second was Arkansas, with just a 19.6% increase in accidental drug overdose deaths.

#GRAF TWO
#Fifty-five to sixty-four year olds experienced the most deaths in 2019 due to unintentional drug overdoses.
#Over 1100 55-64 year olds died due to accidental drug overdose.
#In second was 45-54 year olds, who also had over 1100 accidental drug overdose deaths.