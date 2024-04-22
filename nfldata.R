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
#
# #create a new data set with penalty and offense stats selecting only relevant data
#
merged <- merge(sched, pen, by = c('Date', 'home', 'away')) |>
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

# creating new tibble where each observation is an individual game.
nfl <- tibble(date = character(),
              week = numeric(),
              team = character(),
              opp = character(),
              crew = character(),
              points = numeric(),
              oYards = numeric(),
              pYards = numeric(),
              pCount = numeric(),
              to = numeric(),
              won = numeric(),
              tDiff = numeric(),
              penDiff = numeric(),
              ptsDiff = numeric(),
              ydsDiff = numeric(),
              pYdDiff = numeric(),
              home = numeric()
              )

for(i in 1:nrow(merged)){
  if (merged$winner[i] == merged$home[i]){
    nfl <- add_row(nfl,
                   date = merged$date[i],
                   week = merged$week[i],
                   team = merged$home[i],
                   opp = merged$away[i],
                   crew = merged$crew[i],
                   points = merged$ptsW[i],
                   oYards = merged$ydsW[i],
                   pYards = merged$hYards[i],
                   pCount = merged$hCount[i],
                   to = merged$toW[i],
                   won = 1,
                   tDiff = merged$toW[i] - merged$toL[i],
                   penDiff = merged$hCount[i] - merged$aCount[i],
                   ptsDiff = merged$ptsW[i] - merged$ptsL[i],
                   ydsDiff = merged$ydsW[i] - merged$ydsL[i],
                   pYdDiff = merged$hYards[i] - merged$aYards[i],
                   home = 1)
  }


}
for(i in 1:nrow(merged)){
if (merged$loser[i] == merged$home[i]){
  nfl <- add_row(nfl,
                 date = merged$date[i],
                 week = merged$week[i],
                 team = merged$home[i],
                 opp = merged$away[i],
                 crew = merged$crew[i],
                 points = merged$ptsL[i],
                 oYards = merged$ydsL[i],
                 pYards = merged$hYards[i],
                 pCount = merged$hCount[i],
                 to = merged$toL[i],
                 won = 0,
                 tDiff = merged$toL[i] - merged$toW[i],
                 penDiff = merged$hCount[i] - merged$aCount[i],
                 ptsDiff = merged$ptsL[i] - merged$ptsW[i],
                 ydsDiff = merged$ydsL[i] - merged$ydsW[i],
                 pYdDiff = merged$hYards[i] - merged$aYards[i],
                 home = 1)

}
}
#
for(i in 1:nrow(merged)){
  if (merged$winner[i] == merged$away[i]){
    nfl <- add_row(nfl,
                   date = merged$date[i],
                   week = merged$week[i],
                   team = merged$away[i],
                   opp = merged$home[i],
                   crew = merged$crew[i],
                   points = merged$ptsW[i],
                   oYards = merged$ydsW[i],
                   pYards = merged$aYards[i],
                   pCount = merged$aCount[i],
                   to = merged$toW[i],
                   won = 1,
                   tDiff = merged$toW[i] - merged$toL[i],
                   penDiff = merged$aCount[i] - merged$hCount[i],
                   ptsDiff = merged$ptsW[i] - merged$ptsL[i],
                   ydsDiff = merged$ydsW[i] - merged$ydsL[i],
                   pYdDiff = merged$aYards[i] - merged$hYards[i],
                   home = 0)

  }
}
#
for(i in 1:nrow(merged)){
  if (merged$loser[i] == merged$away[i]){
    nfl <- add_row(nfl,
                   date = merged$date[i],
                   week = merged$week[i],
                   team = merged$away[i],
                   opp = merged$home[i],
                   crew = merged$crew[i],
                   points = merged$ptsL[i],
                   oYards = merged$ydsL[i],
                   pYards = merged$aYards[i],
                   pCount = merged$aCount[i],
                   to = merged$toL[i],
                   won = 0,
                   tDiff = merged$toL[i] - merged$toW[i],
                   penDiff = merged$aCount[i] - merged$hCount[i],
                   ptsDiff = merged$ptsL[i] - merged$ptsW[i],
                   ydsDiff = merged$ydsL[i] - merged$ydsW[i],
                   pYdDiff = merged$aYards[i] - merged$hYards[i],
                   home = 0)

  }
}

nfl <- nfl |>
  mutate(tie = case_when(ptsDiff == 0 ~ 1,
                         ptsDiff != 0 ~ 0))
for(i in 1:nrow(nfl)){
  if(nfl$tie[i] == 1){
    nfl$won[i] = 0
  }
}

# write.csv(nfl, file = "/Users/Jun/Documents/Senior-Seminar/nfl.csv")

# read in nfl data
df <- read_csv("nfl.csv")


# attempt to create a tibble for ref stats
#only way i could summarize number of games officiated by each crew for some dumb reason
gCount <- df |>
  count(crew)


# commented out bc names are weird and i need to be able to reference whats what
refStats <- tibble(
  crew = games[,1],
  games = games[,2],
  penalties = tapply(df$pCount, df$crew, sum),
  hPen = tapply(df$pCount, list(df$crew, df$home), sum),
  hYards = tapply(df$pYards, list(df$crew, df$home), sum),
  wPen = tapply(df$pCount, list (df$crew, df$won), sum),
  wYards = tapply(df$pYards, list(df$crew, df$won), sum)
  )

refStats <- tibble(
  crew = gCount$crew,
  games = (gCount$n)/2,
  penalties = tapply(df$pCount, df$crew, sum),
  hPen = tapply(df$pCount, list(df$crew, df$home), sum)[,2],
  aPen = tapply(df$pCount, list(df$crew, df$home), sum)[,1],
  hYards = tapply(df$pYards, list(df$crew, df$home), sum)[,2],
  aYards = tapply(df$pYards, list(df$crew, df$home), sum)[,1],
  wPen = tapply(df$pCount, list (df$crew, df$won), sum)[,2],
  lPen = tapply(df$pCount, list (df$crew, df$won), sum)[,1],
  wYards = tapply(df$pYards, list(df$crew, df$won), sum)[,2],
  lYards = tapply(df$pYards, list(df$crew, df$won), sum)[,1])
write.csv(refStats, file = "/Users/Jun/Documents/Senior-Seminar/refStats.csv")

#Plot of total penalties across seasons by each crew

ggplot(refStats, aes(x = crew, y = penalties)) +
  geom_bar(stat = "identity", fill = "blue4") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Referee Crew", y = "Total Penalties")

#More importantly, penalties per game
ggplot(refStats, aes(x = crew, y = penalties/games)) +
  geom_bar(stat = "identity", fill = "blue4") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Referee Crew", y = "Penalties per Game")

# Which refs favor the home team more?
x <- refStats$aPen - refStats$hPen

ggplot(refStats, aes(x = crew, y = x)) +
  geom_bar(stat = "identity", fill = "blue4") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Referee Crew", y = "Difference in Home-Away")

#Which teams are penalized the most? 
teamPen <- df |> 
  group_by(team) |> 
  summarise(penalties = sum(pCount), 
            wins = sum(won)) |> 
  arrange(desc(penalties))

ggplot(teamPen, aes(x = reorder(team, penalties), y = penalties)) +
  geom_bar(stat = "identity", fill = "blue4") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Team", y = "Penalties from 22-23 Seasons") 

#Same Plot, but with number of wins
ggplot(teamPen, aes(x = reorder(team, penalties), y = wins)) +
  geom_bar(stat = "identity", fill = "blue4") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Team", y = "Wins in 22-23 Seasons") 


# Correlation
ggplot(teamPen, aes(x = penalties, y = wins)) +
  geom_point() +
  ggtitle("Correlation Between Number of Wins and Penalties") +
  theme_minimal() +
  labs(x = "Total Penalties", y = "Wins (out of a possible 34)") 










  
  

  









