              #### ASSIGNMENT 2 ####

# Answer the following questions using the dispatch.csv file 
# and the tidyverse functions we've learned 


# Question 1: What's the timeframe that this dataset covers?  

# Your code: 
library(tidyverse)
              
setwd("/Users/Jacob/Documents/GitHub")
              
dispatch <- read_csv("adv-data-journ-fall2021/Data/dispatch.csv")

class(dispatch)

dispatch %>% select(CallDateTime) %>% arrange(desc(CallDateTime))
# The answer
# 8/23/2021 to 8/29/2021



# Question 2: Which day of the week had the most incidents? 

# Your code: 
tail(names(sort(table(dispatch$DOW))), 1)



# The answer: 

#Tuesday


# Question 3:  Which hour of the day had the most incidents?

# Your code: 
tail(names(sort(table(dispatch$Hour))), 1)



# The answer: 

#1:00 PM


# Question 4:  Which police district had the most traffic stops?

# Your code: 
dispatch %>% filter(ExtNatureDisplayName == "TRAFFIC STOP") %>% count(PolArea)

# The answer:

#50E



# Question 5:  What's the most common reason police are dispatched to the airport? (11300 S AIRPORT DR)

# Your code: 
dispatch %>% filter(Address == "11300 S AIRPORT DR") %>% count(ExtNatureDisplayName)



# The answer:

#CHECK AREA


