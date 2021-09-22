library(tidyverse)
library(janitor)

#site: wonder.cdc.gov
#downloaded on Sept. 20, 2021
deaths <- read_delim("Data/deaths-2010-2019.txt", delim = "\t")

#pulls only the notes from death table
notes <- deaths %>%
  filter(!is.na(Notes)) %>%
  select(Notes)

#filters out notes out of the deaths table
deaths <- deaths %>%
  filter(is.na(Notes)) %>%
  select(-Notes)

deaths <- deaths %>%
  clean_names()

deaths %>%
  group_by(year) %>%
  summarise(deaths = sum(deaths))

deaths %>%
  group_by(year) %>%
  summarise(deaths = sum(deaths)/sum(population))

deaths %>%
  filter(population != "Not Applicable") %>%
  group_by(year) %>%
  summarise(death_rate = sum(deaths)/sum(as.numeric(population))*100000)

write.csv(notes, "Data/deaths-notes.csv", na="")
write.csv(deaths, "Data/clean-deaths-2010-2019.csv", na="")

deaths <- read_csv("Data/clean-deaths-2010-2019.csv")

deaths %>%
  filter(population == "Not Applicable")

deaths <- deaths %>%
  mutate(newPop = as.numeric(population))

#intro to ggplot
death_by_year <- deaths %>% filter(!is.na(newPop)) %>%
  group_by(year) %>% summarise(death_rate = (sum(deaths)/sum(newPop))*100000)

ggplot(data = death_by_year, aes(x=year, y=death_rate, group=1)) +
  geom_line() +
  geom_point()

age_by_year <- deaths %>% filter(!is.na(newPop)) %>%
  group_by(year, ten_year_age_groups) %>% summarise(death_rate = (sum(deaths)/sum(newPop))*100000)

ggplot(data = age_by_year, aes(x=year, y=death_rate, group=ten_year_age_groups, color=ten_year_age_groups)) +
  geom_line() +
  geom_point()