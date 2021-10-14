library(tidyverse)
library(janitor)

# Import the Provisional Deaths and the Population files
# Change the paths if needed
pop <- read_csv("Data/pop-by-race-eth-age.csv")
deaths <- read_csv("Data/Provisional_COVID-19_Deaths_by_Race_and_Hispanic_Origin__and_Age.csv")

# Clean up the data: alter the pop file (as we discussed in class)
# Clean up the column names in the deaths data
pop <- pop %>% mutate(race_eth = str_replace(race_eth, "Non Hispanic", "Non-Hispanic"))
deaths <- deaths %>% clean_names()

# Join the tables
combined <- deaths %>% inner_join(pop, by=c("state"="name","age_group","race_and_hispanic_origin_group"="race_eth"))

# Your job is to analyze the data and come up with the most compelling story idea to pitch to your editor. 
# I expect some thorough analysis here: explore several possible ideas. Walk me through those ideas 
# in your code. I should be able to run the code below and check your work and verify the numbers in your pitch. 
# Include a short pitch (a few sentences) with numbers from your analysis that outlines your story idea.
# You will be graded on the code you write, the level of your analysis, and the strength of your pitch 
# (don't pitch a story idea that simply states the death rate for one group, for example. Think in terms of comparisons.)

#How many American Indians have died from COVID-19?

americanIndianDeaths <- deaths %>%
  filter(race_and_hispanic_origin_group == "Non-Hispanic American Indian or Alaska Native" & age_group == "All Ages") %>%
  group_by(race_and_hispanic_origin_group, covid_19_deaths)

#7,550
#Obviously, there are less American Indians in the United States than other races, so let's calculate a death rate.
#There's not a population figure for all American Indians in the United States in this data, so I'll use total deaths as a replacement.

americanIndianDeaths <- deaths %>%
  filter(race_and_hispanic_origin_group == "Non-Hispanic American Indian or Alaska Native" & age_group == "All Ages") %>%
  group_by(race_and_hispanic_origin_group, covid_19_deaths) %>%
  mutate(aiDeathPercent = covid_19_deaths/total_deaths)

#So 18% of American Indian deaths from the past year and a half were due to COVID-19.
#How does that compare to other racial groups?

combined <- combined %>%
  mutate(percentOfTotalDeaths = covid_19_deaths/total_deaths)

#The top races shown here are all minority groups, with American Indians, Hispanic Peoples and Pacific Islanders having the highest percent of total deaths attributed to covid-19.
#Which state has the most American Indians? What is their percent of total deaths attributed to covid-19 and how does it compare to the rest of the country?

americanIndianPop <- combined %>%
  filter(race_and_hispanic_origin_group == "Non-Hispanic American Indian or Alaska Native") %>%
  group_by(state, race_and_hispanic_origin_group) %>%
  summarise(statePops = sum(pop2019))

americanIndianDeathsInOklahoma <- deaths %>%
  filter(race_and_hispanic_origin_group == "Non-Hispanic American Indian or Alaska Native" & state == "Oklahoma") %>%
  group_by(state, race_and_hispanic_origin_group) %>%
  mutate(aiDeathPercent = covid_19_deaths/total_deaths)

#Oklahoma has the biggest American Indian population in the United States. I've calculated their percent deaths attributed to covid-19.
#Let's compare that to Missouri, a more middle of the pack state in terms of Native population.

americanIndianDeathsInMissouri <- deaths %>%
  filter(race_and_hispanic_origin_group == "Non-Hispanic American Indian or Alaska Native" & state == "Missouri") %>%
  group_by(state, race_and_hispanic_origin_group) %>%
  mutate(aiDeathPercent = covid_19_deaths/total_deaths)

#Missouri doesn't really have enough data. Maybe we could see with state has the most covid-19 deaths per thousand among American Indians.

americanIndianCovid <- combined %>% filter(!is.na(pop2019) & !is.na(covid_19_deaths) & race_and_hispanic_origin_group == "Non-Hispanic American Indian or Alaska Native") %>%
  group_by(state, race_and_hispanic_origin_group) %>%
  summarise(death_rate = (sum(covid_19_deaths)/sum(pop2019))*1000)

#It seems that New Mexico has the most deaths due to covid-19 per thousand among American Indians. How does nine covid-19 deaths per one thousand compare to other race groups?

covidDeathRates <- combined %>% filter(!is.na(pop2019) & !is.na(covid_19_deaths)) %>%
  group_by(state, race_and_hispanic_origin_group) %>%
  summarise(death_rate = (sum(covid_19_deaths)/sum(pop2019))*1000)

#American Indians in New Mexico have died the most per one thousand due to covid-19 out of all race groups. Let's compare that to other race groups in New Mexico. 

covidDeathRates <- combined %>% filter(!is.na(pop2019) & !is.na(covid_19_deaths) & state == "New Mexico") %>%
  group_by(state, race_and_hispanic_origin_group) %>%
  summarise(death_rate = (sum(covid_19_deaths)/sum(pop2019))*1000)

#No other racial group comes even close.

#PITCH:
#According to CDC data, American Indians in New Mexico are dying more than any other race to covid-19 per one thousand people.
#While nine American Indians in New Mexico have died to covid-19 per one thousand people, that number hangs around 2 or lower for other racial groups.
#This is especially alarming because New Mexico has the 5th highest American Indian population in the country, at just under 200,000 people.