library(baseballr)
library(data.table)
library(tidyverse)
library(lubridate)
library(baseballDBR)
library(rvest)

# Load data for each day of the season

datBat <- data.table()
datPitch <- data.table()

t_diff <- Sys.Date() - ymd("2020-07-23")
roll_days <- 3  # number of days for the rolling average for rate changes

for( i in 1:t_diff){
  #i <- 1
  datTemp <- daily_batter_bref(t1 = "2020-07-23", t2 = as.character(ymd("2020-07-23")+i))
  datTemp <- as.data.table(datTemp)
  datTemp[, Date := as.character(ymd("2020-07-22")+i)]
  datBat <- rbind(datBat,datTemp)
}

for( i in 1:t_diff){
  #i <- 1
  datTemp <- daily_pitcher_bref(t1 = "2020-07-22", t2 = as.character(ymd("2020-07-23")+i))
  datTemp <- as.data.table(datTemp)
  datTemp[, Date := as.character(ymd("2020-07-22")+i)]
  datPitch <- rbind(datPitch,datTemp)
}

#######
# B_Team League Available Players
#######

url <- "https://baseball.fantasysports.yahoo.com/b1/165789/players"
batterAvail <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)

Avial <- as.data.table(batterAvail[[1]])

AvialBat <- as.data.table(cbind(Avial[,2],
                                Avial[,8],
                                Avial[,9],
                                Avial[,10],
                                Avial[,13],
                                Avial[,14],
                                Avial[,15],
                                Avial[,16]))

AvialBat  <- AvialBat [2:(nrow(AvialBat )-1),]

names(AvialBat ) <- c("Batters","GP","Rank_Pre","Rank","R","HR","RBI","SB")

B_Team_Avail_Bat =
  AvialBat  %>%
  mutate(
    Player = stringr::str_extract(Batters,"(?<=\\s)(.*)(?=\\-)"),
    Team = stringr::str_sub(Player,-4,-1),
    Team = stringr::str_trim(Team),
    Player = stringr::str_sub(Player,1,-5),
    Player = stringr::str_trim(Player)) %>%
  select("Player","Team","GP","Rank_Pre","Rank","R","HR","RBI","SB")

url <- "https://baseball.fantasysports.yahoo.com/b1/165789/players?status=A&pos=P&cut_type=33&stat1=S_S_2020&myteam=0&sort=28&sdir=1"
pitcherAvail <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)


Avial <- as.data.table(pitcherAvail [[1]])

AvialPitch <- as.data.table(cbind(Avial[,2],
                                  Avial[,8],
                                  Avial[,9],
                                  Avial[,10],
                                  Avial[,13],
                                  Avial[,14],
                                  Avial[,15],
                                  Avial[,16],
                                  Avial[,17]))

AvialPitch  <- AvialPitch [2:(nrow(AvialPitch )-1),]

names(AvialPitch ) <- c("Pitchers","GP","Rank_Pre","Rank","W","SV","K","ERA","WHIP")

B_Team_Avail_Pitch =
  AvialPitch  %>%
  mutate(
    Player = stringr::str_extract(Pitchers,"(?<=\\s)(.*)(?=\\-)"),
    Team = stringr::str_sub(Player,-4,-1),
    Team = stringr::str_trim(Team),
    Player = stringr::str_sub(Player,1,-5),
    Player = stringr::str_trim(Player)) %>%
  select("Player","Team","GP","Rank_Pre","Rank","W","SV","K","ERA","WHIP")


url <- "https://baseball.fantasysports.yahoo.com/b1/165789/players?status=A&pos=RP&cut_type=33&stat1=S_S_2020&myteam=0&sort=32&sdir=1"
pitcherRefiefAvail <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)

Avial <- as.data.table(pitcherRefiefAvail  [[1]])

AvialPitchRP <- as.data.table(cbind(Avial[,2],
                                    Avial[,7],
                                    Avial[,8],
                                    Avial[,9],
                                    Avial[,12],
                                    Avial[,13],
                                    Avial[,14],
                                    Avial[,15],
                                    Avial[,16]))

AvialPitchRP  <- AvialPitchRP [2:(nrow(AvialPitchRP )-1),]

names(AvialPitchRP ) <- c("Pitchers","GP","Rank_Pre","Rank","W","SV","K","ERA","WHIP")

B_Team_Avail_Pitch_RP =
  AvialPitchRP  %>%
  mutate(
    Player = stringr::str_extract(Pitchers,"(?<=\\s)(.*)(?=\\-)"),
    Team = stringr::str_sub(Player,-4,-1),
    Team = stringr::str_trim(Team),
    Player = stringr::str_sub(Player,1,-5),
    Player = stringr::str_trim(Player)) %>%
  select("Player","Team","GP","Rank_Pre","Rank","W","SV","K","ERA","WHIP")



#######
# F_Team League Available Players
#######

url <- "https://baseball.fantasysports.yahoo.com/b1/173307/players"
batterAvail <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)

Avial <- as.data.table(batterAvail[[1]])

AvialBat <- as.data.table(cbind(Avial[,2],
                                Avial[,8],
                                Avial[,9],
                                Avial[,10],
                                Avial[,13],
                                Avial[,14],
                                Avial[,15],
                                Avial[,16]))

AvialBat  <- AvialBat [2:(nrow(AvialBat )-1),]

names(AvialBat ) <- c("Batters","GP","Rank_Pre","Rank","R","HR","RBI","SB")

F_Team_Avail_Bat =
  AvialBat  %>%
  mutate(
    Player = stringr::str_extract(Batters,"(?<=\\s)(.*)(?=\\-)"),
    Team = stringr::str_sub(Player,-4,-1),
    Team = stringr::str_trim(Team),
    Player = stringr::str_sub(Player,1,-5),
    Player = stringr::str_trim(Player)) %>%
  select("Player","Team","GP","Rank_Pre","Rank","R","HR","RBI","SB")

url <- "https://baseball.fantasysports.yahoo.com/b1/173307/players?status=A&pos=P&cut_type=33&stat1=S_S_2020&myteam=0&sort=28&sdir=1"
pitcherAvail <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)


Avial <- as.data.table(pitcherAvail [[1]])

AvialPitch <- as.data.table(cbind(Avial[,2],
                                  Avial[,8],
                                  Avial[,9],
                                  Avial[,10],
                                  Avial[,13],
                                  Avial[,14],
                                  Avial[,15],
                                  Avial[,16],
                                  Avial[,17]))

AvialPitch  <- AvialPitch [2:(nrow(AvialPitch )-1),]

names(AvialPitch ) <- c("Pitchers","GP","Rank_Pre","Rank","W","SV","K","ERA","WHIP")

F_Team_Avail_Pitch =
  AvialPitch  %>%
  mutate(
    Player = stringr::str_extract(Pitchers,"(?<=\\s)(.*)(?=\\-)"),
    Team = stringr::str_sub(Player,-4,-1),
    Team = stringr::str_trim(Team),
    Player = stringr::str_sub(Player,1,-5),
    Player = stringr::str_trim(Player)) %>%
  select("Player","Team","GP","Rank_Pre","Rank","W","SV","K","ERA","WHIP")


url <- "https://baseball.fantasysports.yahoo.com/b1/173307/players?status=A&pos=RP&cut_type=33&stat1=S_S_2020&myteam=0&sort=32&sdir=1"
pitcherRefiefAvail <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)

Avial <- as.data.table(pitcherRefiefAvail  [[1]])

AvialPitchRP <- as.data.table(cbind(Avial[,2],
                                    Avial[,7],
                                    Avial[,8],
                                    Avial[,9],
                                    Avial[,12],
                                    Avial[,13],
                                    Avial[,14],
                                    Avial[,15],
                                    Avial[,16]))

AvialPitchRP  <- AvialPitchRP [2:(nrow(AvialPitchRP )-1),]

names(AvialPitchRP ) <- c("Pitchers","GP","Rank_Pre","Rank","W","SV","K","ERA","WHIP")

F_Team_Avail_Pitch_RP =
  AvialPitchRP  %>%
  mutate(
    Player = stringr::str_extract(Pitchers,"(?<=\\s)(.*)(?=\\-)"),
    Team = stringr::str_sub(Player,-4,-1),
    Team = stringr::str_trim(Team),
    Player = stringr::str_sub(Player,1,-5),
    Player = stringr::str_trim(Player)) %>%
  select("Player","Team","GP","Rank_Pre","Rank","W","SV","K","ERA","WHIP")

###################
# Team Rosters
######################
url <- "https://baseball.fantasysports.yahoo.com/b1/165789/10"
B_Team_Roster <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)

url <- "https://baseball.fantasysports.yahoo.com/b1/173307/5"
F_Team_Roster <- xml2::read_html(url) %>%
  html_nodes(css = 'table') %>%
  html_table(fill = TRUE)

rost =
  B_Team_Roster[[1]] %>%
  as.data.table() 

rostBat <- as.data.table(cbind(rost[,1],
                               rost[,2],
                               rost[,7]))

rostBat <- rostBat[2:(nrow(rostBat)-1),]

names(rostBat) <- c("Pos","Batters","Opp")

rost =
  B_Team_Roster[[2]] %>%
  as.data.table() 

rostPitch <- as.data.table(cbind(rost[,1],
                                 rost[,2],
                                 rost[,7]))

rostPitch <- rostPitch[2:(nrow(rostPitch)-1),]

names(rostPitch) <- c("Pos","Batters","Opp")

B_Team_Rost =
  rbind(
    rostBat %>%
      mutate(
        Player = stringr::str_extract(Batters,"(?<=\\s)(.*)(?=\\-)"),
        Team = stringr::str_sub(Player,-4,-1),
        Team = stringr::str_trim(Team),
        Player = stringr::str_sub(Player,1,-5),
        Player = stringr::str_trim(Player)) %>%
      select("Pos","Player","Team"),
    
    rostPitch %>%
      mutate(
        Player = stringr::str_extract(Batters,"(?<=\\s)(.*)(?=\\-)"),
        Team = stringr::str_sub(Player,-4,-1),
        Team = stringr::str_trim(Team),
        Player = stringr::str_sub(Player,1,-5),
        Player = stringr::str_trim(Player)) %>%
      select("Pos","Player","Team")
  )

#############
# F_Team Roster
#############
rost =
  F_Team_Roster[[1]] %>%
  as.data.table() 

rostBat <- as.data.table(cbind(rost[,1],
                               rost[,2],
                               rost[,7]))

rostBat <- rostBat[2:(nrow(rostBat)-1),]

names(rostBat) <- c("Pos","Batters","Opp")

rost =
  F_Team_Roster[[2]] %>%
  as.data.table() 

rostPitch <- as.data.table(cbind(rost[,1],
                                 rost[,2],
                                 rost[,7]))

rostPitch <- rostPitch[2:(nrow(rostPitch)-1),]

names(rostPitch) <- c("Pos","Batters","Opp")

F_Team_Rost =
  rbind(
    rostBat %>%
      mutate(
        Player = stringr::str_extract(Batters,"(?<=\\s)(.*)(?=\\-)"),
        Team = stringr::str_sub(Player,-4,-1),
        Team = stringr::str_trim(Team),
        Player = stringr::str_sub(Player,1,-5),
        Player = stringr::str_trim(Player)) %>%
      select("Pos","Player","Team"),
    
    rostPitch %>%
      mutate(
        Player = stringr::str_extract(Batters,"(?<=\\s)(.*)(?=\\-)"),
        Team = stringr::str_sub(Player,-4,-1),
        Team = stringr::str_trim(Team),
        Player = stringr::str_sub(Player,1,-5),
        Player = stringr::str_trim(Player)) %>%
      select("Pos","Player","Team")
  )

#####