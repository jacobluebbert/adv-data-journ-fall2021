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
