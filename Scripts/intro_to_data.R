library(tidyverse)

MLB <- read_csv("Data/MLB_Salaries_1.csv")

MLB$Salary

MLB%>% arrange(Salary)