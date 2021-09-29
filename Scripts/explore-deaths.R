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

write_csv(deaths, "Data/clean2-deaths-2010-2019.csv", na="")

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

#compare 2010 and 2019 death rates by state with percent change
state_year <- deaths %>% filter(!is.na(newPop) & year %in% c("2010", "2019")) %>%
  group_by(state, year) %>%
  summarise(death_rate = (sum(deaths)/sum(newPop))*100000)

state_year %>% pivot_wider(names_from = year, values_from = death_rate) %>%
  mutate(pct_chg = (`2019` - `2010`)/`2010`) %>%
  arrange(desc(pct_chg))

#compare 2010 and 2019 death rates by age group with percent change
age_year <- deaths %>% filter(!is.na(newPop) & year %in% c("2010", "2019")) %>%
  group_by(ten_year_age_groups, year) %>%
  summarise(death_rate = (sum(deaths)/sum(newPop))*100000)

age_year %>% pivot_wider(names_from = year, values_from = death_rate) %>%
  mutate(pct_chg = (`2019` - `2010`)/`2010`) %>%
  arrange(desc(pct_chg))

#compare death rate for all years for age groups
age_year2 <- deaths %>% filter(!is.na(newPop)) %>%
  group_by(ten_year_age_groups, year) %>%
  summarise(death_rate = (sum(deaths)/sum(newPop))*100000)

age_year2_pivot <- age_year2 %>% pivot_wider(names_from = year, values_from = death_rate) %>%
  mutate(pct_chg = (`2019` - `2010`)/`2010`) %>%
  arrange(desc(pct_chg))

#line chart for 25-34, 35-44 age groups
age_year_pair <- deaths %>% filter(!is.na(newPop) & ten_year_age_groups %in% c("25-34 years", "35-44 years")) %>%
  group_by(ten_year_age_groups, year) %>%
  summarise(death_rate = (sum(deaths)/sum(newPop))*100000)

ggplot(data = age_year_pair, aes(x=year, y=death_rate, group=ten_year_age_groups, color=ten_year_age_groups)) +
  geom_line() +
  geom_point()

deaths <- read_csv("Data/clean2-deaths-2010-2019.csv")

#CASE WHEN: if statement in excel basically. used to sort of group multiple groups of data into one bigger one.
deaths <- deaths %>% mutate(new_age = case_when(
  ten_year_age_groups_code %in% c("65-74", "75-84", "85+") ~ "65+",
  ten_year_age_groups_code %in% c("25-34", "35-44", "45-54", "55-64") ~ "25-64",
  ten_year_age_groups_code %in% c("1", "1-4", "5-14", "15-24") ~ "Under 25",
  TRUE ~ ten_year_age_groups_code
))