#Twitter package that allows you to explore twitter data.
install.packages("rtweet")

#Installing libraries.
library(tidyverse)
library(rtweet)

#Both Fangraphs leaderboards and relevant stats. Filter by 300 plate appearances for hitters and 130 innings pitched for pitchers.
#Fangraphs is an independent baseball stats aggregation website: https://en.wikipedia.org/wiki/FanGraphs
#They are one of the industry leaders in baseball statistics.
hittingLeaderboards <- read.csv("Final Project/Fangraphs Hitting Leaderboard.csv")
pitchingLeaderboards <- read.csv("Final Project/Fangraphs Pitching Leaderboard.csv")


#List of player names with at least 300 plate appearances. This was how I was going to search the number of times the
#player's name was Googled/Tweeted.
playerNamesHitting <- read.csv("Final Project/playernamesHitting.csv")

#Creates character vector of all relevant hitters.
playerListHitting <- readLines("Final Project/playernamesHitting.csv")

#Here is where things got very interesting. Scrubbing Twitter data is MUCH harder than I thought it would be.
#I originally thought it would be as simple as typing in a string and seeing how many times that string was
#searched over a time period I could set. Unfortunately, I haven't been able to find a way to do this. I
#tried to pivot to Google Trends, but that allows a maximum of five queries. I have 262 hitters alone. I did
#some pretty extensive Googling to find solutions, but I couldn't really find anything. This is the best
#solution I could find. Twitter also has rate limits which prevent you from getting over 15000 tweets or so at
#a time before they limit you. I have no idea how to get past this or how I should pivot. I'm not sure Twitter
#is going to work.

#Either way, we can start to analyze the Fangraphs data at least.

#Who had the highest fWAR among position players?
highestFWAR <- hittingLeaderboards %>%
  select(ï..Name, Team, WAR) %>%
  arrange(desc(WAR))

#The answer is the Dodgers' Trea Turner. The player with at least 300 PA and the lowest WAR is...
hittingLeaderboards %>%
  select(ï..Name, Team, WAR) %>%
  arrange(WAR)

#Pitsburgh's Gregory Polanco.

#I might adopt fWAR as my primary stat in determining a players' value. Since WAR tries to measure a player's skill in
#all aspects of the game and turn it into a single number, it seems like the perfect stat for measuring a player's value.
#My thinking is this (if I can actually get a Twitter or Google data scrubber working): WAR/number of tweets. 
#The more tweets about you, the lower your new WAR would become.

#Which team had the highest combined fWAR?
highestTeam <- hittingLeaderboards %>%
  group_by(Team) %>%
  summarize(teamWar = sum(WAR))

#This highlights an issue with my data which I will fix manually. Players who changed teams mid-season play for '----'.
#I'll see if Fangraphs has a data set with WAR splits for the teams a player played for. For example, Joey Gallo played
#for the Yankees and the Rangers. I'd like to see his WAR for both teams.

#So, Fangraphs does have that data, but it also splits the player's plate appearances for both teams. For example,
#Joey Gallo may have had 300 PA for the Rangers, but not the Yankees. This messes up my data, so I'll probably just
#change the players with the team '----' to the team they last played for.

#I know this project is severely lacking right now, but it'll get better once I figure out how to judge a player's
#popularity with data. Twitter or Google might still be able to work, but I need to do a lot of work to figure that out.
baseballTweets <- search_tweets(
  "MLB", n = 18000, include_rts = FALSE
)

baseballTweets %>%
  