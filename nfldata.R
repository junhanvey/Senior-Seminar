# loading necessary packages
library(tidyverse)
library(ggplot2)
library(dplyr)

# read in data
pen <- read_csv("penaltystats.csv")
sched <- read_csv("schedule.csv")
teams <- read_csv("nfl_teams.csv")

# add abbreviations to tibbles to match on
cities <- tibble(city = unique(pen$Home),
                 abb = c('KC', 'BAL', 'DEN', 'PIT', 'WAS', 'CHI', 'ATL', 
                         'SEA', 'NO', 'LAC', 'NE', 'NYG', 'CLE',
                         'MIN', 'IND', 'NYJ', 'PHI', 'ARI', 'JAX', 
                         'DET', 'TB', 'LAR', 'CIN', 'HOU', 'TEN', 
                         'DAL', 'BUF', 'CAR', 'SF', 'GB', 'MIA', 'LV')) 
sched <- mutate(sched,
                home = Home %in% teams$Name) |> 
  mutate(home = teams$Abbreviation[match(Home, teams$Name)]) 

sched <- mutate(sched,
                  away = Away %in% teams$Name) |> 
  mutate(away = teams$Abbreviation[match(Away, teams$Name)]) 
  

pen <- mutate(pen, 
              home = Home %in% cities$city) |> 
  mutate(home = cities$abb[match(Home, cities$city)]) 


pen <- mutate(pen, 
              away = Away %in% cities$city) |> 
  mutate(away = cities$abb[match(Away, cities$city)]) 

#create a new data set with penalty and offense stats selecting only relevant data

nfl <- merge(sched, pen, by = c('Date', 'home', 'away')) |> 
  select(Date, home, away, Week, 'Winner/tie', 'Loser/tie', PtsW, PtsL, YdsW, 
         TOW, YdsL, TOL, Crew, Accepted, Yards, aCount, aYards, hCount, hYards, 
         total) |> 
  rename(date = Date, home = home, away = away, week = Week, 
         Winner = 'Winner/tie', Loser = 'Loser/tie', ptsW = PtsW, ptsL = PtsL,
         ydsW = YdsW, toW = TOW, ydsL = YdsL, toL = TOL, crew = Crew, accepted = Accepted, 
         totYds = Yards, aCount = aCount, aYards = aYards, hCount = hCount, 
         hYards = hYards, total = total) |> 
  mutate(winner = Winner %in% teams$Name) |> 
  mutate(winner = teams$Abbreviation[match(Winner, teams$Name)]) |> 
  mutate(loser = Loser %in%teams$Name) |> 
  mutate(loser = teams$Abbreviation[match(Loser, teams$Name)]) |> 
  select(!c(Loser, Winner))





