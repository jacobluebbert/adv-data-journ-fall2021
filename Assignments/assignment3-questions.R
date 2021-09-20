## LOAD TIDYVERSE AND THE DATA FILE
library(tidyverse)

baltimore <- read_csv("Data/Baltimore_City_Employee_Salaries.csv")
## INTEGRITY CHECKS

# Is any individual in here twice? Why?
baltimore %>%
  group_by(Name) %>% 
  summarise(n = n()) %>%
  arrange(desc(n))

#There are multiple individuals that appear more than twice in this data set (Stacey Thomas, Angela Carter, etc.). To be honest, I'm not 100% sure why some of these individuals appear more than once. Sometimes their jobs change slightly, or they change departments, or their salary changes.
#-----------------------------------------------------------------------------------------------------------
# How many years of data do we have?
baltimore %>%
  filter(!is.na(FiscalYear)) %>%
  summarise(time_frame = range(FiscalYear))

#2011 to 2020.
#------------------------------------------------------------------------------------------------------------
# What's the min and max annual salary given out in 2020?
baltimore %>%
  filter(!is.na(AnnualSalary)) %>%
  summarise(time_frame = range(AnnualSalary))

#Min: 0. Max: 275000.
#------------------------------------------------------------------------------------------------------------
# What jobs get paid $0?
baltimore %>%
  filter(AnnualSalary == 0) %>%
  select(JobTitle)

#AIDE BLUE CHIP, School Health Aide
#------------------------------------------------------------------------------------------------------------
# How clean are the job titles? Are there a lot of duplicates? 
# Clean up the JobTitles by making everything lowercase 
# (hint: use mutate to overwrite the current JobTitle field, using the function str_to_lower())
baltimore %>%
  mutate(JobTitle = str_to_lower(JobTitle))

# Take a look at agency names; how clean are they? 
baltimore %>%
  mutate(AgencyName = str_to_lower(AgencyName))


## QUESTIONS OF THE DATA

# Who's the highest paid employee in FY2020?
baltimore %>%
  filter(!is.na(AnnualSalary) & FiscalYear == "FY2020") %>%
  select(Name, JobTitle, AnnualSalary) %>%
  arrange(desc(AnnualSalary))
# Which job title has the highest average salary in 2020? (hint: use mean() )
baltimore %>%
  select(JobTitle, AnnualSalary) %>%
  summarise(mean = mean(AnnualSalary))
# Any potential problems with citing these results? 

# How many people work in the police department in 2020?

# How many are "police officers"? 
# What was their total salary?

