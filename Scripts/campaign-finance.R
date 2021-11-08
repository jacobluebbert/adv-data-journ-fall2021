library(tidyverse)

# fix the paths below as needed:
contrib <- read_csv("Data/mo_contributions.csv")
cands <- read_csv("Data/mo_candidates.csv")
comms <- read_csv("Data/mo_committees.csv")

contrib %>% count(transaction_tp)

#type   description
#-------------------
# 15   | contrib from an individual
# 15C  | candidates contributing to themselves
# 15E  | earmarked from an intermediary
# 22Y  | refund to an individual
# 24K  | donation from a PAC to a candidate

### when transaction_tp == "24K", the filer (cmte_id) is a PAC giving to a candidate's principal campaign committee (other_id)
### for everything else, the filer (cmte_id) is the candidate's principal campaign committee, receiving individual contributions

#Who is receiving the most PAC money in Missouri Congress races?
contrib %>% 
  inner_join(cands, by=c("other_id"="pcc")) %>%
  filter(transaction_tp == "24K") %>%
  group_by(cand_name) %>%
  summarise(total = sum(transaction_amt)) %>%
  arrange(desc(total))

#Which PAC is giving the most money?
contrib %>% 
  inner_join(comms, by=c("cmte_id")) %>%
  filter(transaction_tp == "24K") %>%
  group_by(cmte_nm) %>%
  summarise(total = sum(transaction_amt)) %>%
  arrange(desc(total))

# Which Missouri cand is getting the most individual contributions? 
indivContribs <- contrib %>% inner_join(cands, by=c("cmte_id"="pcc")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y")) %>%
  group_by(cand_name) %>% 
  summarise(total = sum(transaction_amt)) %>%
  arrange(desc(total))

vickyPAC <- contrib %>%  inner_join(comms, by=c("cmte_id")) %>%
  filter(name == "VICKY HARTZLER FOR CONGRESS" & transaction_tp == "24K") %>%
  group_by(cmte_nm) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))

#this ended up not working how I wanted, but doesn't look like it matters
contrib %>% inner_join(comms, by=c("cmte_id")) %>%
  filter(cmte_nm == "EMPLOYEES OF NORTHROP GRUMMAN CORPORATION PAC" | cmte_nm == "L3HARRIS TECHNOLOGIES, INC. PAC" | cmte_nm == "POET PAC") %>%
  group_by(name) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))

#This is what I wanted, but yeah, not much changed
contrib %>% inner_join(comms, by=c("cmte_id")) %>%
  filter(cmte_nm == "EMPLOYEES OF NORTHROP GRUMMAN CORPORATION PAC" | cmte_nm == "L3HARRIS TECHNOLOGIES, INC. PAC" | cmte_nm == "POET PAC") %>%
  group_by(cmte_nm, name) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))

contrib %>% inner_join(comms, by=c("cmte_id")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y")) %>%
  group_by(tres_nm) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))

#None contributed to other candidates
contrib %>% inner_join(cands, by=c("cmte_id" = "pcc")) %>%
  filter(transaction_tp == "24K")

#Outta state contribs from individuals
contrib %>% inner_join(comms, by=c("cmte_id")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y") & state != "MO") %>%
  group_by(cmte_nm) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))

#In state contribs from individuals
contrib %>% inner_join(comms, by=c("cmte_id")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y") & state == "MO") %>%
  group_by(cmte_nm) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))

#PITCH: Since this is being pitched to local publications, I wanted to focus on Vicky Hartzler, Columbia's Congresswoman who is now running for Missouri's empty Senate seat. The most interesting fact to me was Hartzler's top PAC donors: most of them were defense contractors. Also, just as an aside, it seems like Eric Schmitt has to be the frontrunner right now, just based on funding. It'd be interesting to see how often the highest earning politician wins, and see if we could predict who would win the Senate seat.



