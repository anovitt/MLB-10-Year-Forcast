library(data.table)
library(tidyverse)
#library(Lahman)
library(lubridate)
library(broom)
library(mlbgameday)
#library(XML2R)
#library(doParallel)
#library(foreach)

# load Lahman data including 2019

path <- c("R/MLB_Vault/baseballdatabank-master/core/")

Master <- fread(paste(path,'people.csv',sep = "")) # player names and ages

# build the pitcher data set. 
# load the downloaded data for 2015 - 2018
# use mlbgameday to scrap 2019

path <- c("R/MLB_Vault/mlb-pitch-data-20152018/")

PitchData <- fread(paste(path,'pitches.csv',sep = ""))
pitchData2019 <- fread(paste0(path,'pitchData2019.csv'))

atbats <- fread(paste(path,'atbats.csv',sep = ""))
atbats2019 <- fread(paste(path,'atbatData2019.csv',sep = ""))

player_names <- fread(paste(path,'player_names.csv',sep = ""))

games <- fread(paste(path,'games.csv',sep = ""))

games <-
games %>%
  select(g_id,date)

###
# clean up the 2019 data  , get speed information and pitcher id and pitcher name from the atbats2019.
###

pitchData2019 <-
pitchData2019 %>%
   select(px,pz,start_speed,end_speed,break_angle,break_length,pitch_type,gameday_link,num)

atbats2019 <-
atbats2019 %>%
  select(pitcher_id = pitcher,pitcher_name,event,gameday_link,num)

pitchData2019 <-
pitchData2019 %>%
  left_join(atbats2019, by = c('gameday_link','num')) %>%
  mutate(date = ymd(stringr::str_sub(gameday_link, 5, 14))) %>%
  mutate(year = year(date)) %>%
  mutate(pitcher_name = gsub(" ","_",pitcher_name)) %>%
  select(pitcher_id ,pitcher_name,px,pz,start_speed,end_speed,break_angle,break_length,pitch_type,event,year)

atbats <-
atbats %>%
  select(ab_id,batter_id,g_id,pitcher_id, event) 

player_names <-
  player_names %>%
  mutate(pitcher_name = paste(first_name,last_name,sep = "_")) %>%
  select(id,pitcher_name) 

#### bind the 2015 - 2018 data with 2019.

PitchData <-
PitchData %>%
  select(px,pz,start_speed,end_speed,break_angle,break_length,pitch_type,ab_id) %>%
  left_join(atbats, by = 'ab_id') %>%
  left_join(games, by = 'g_id' ) %>%
  mutate(date = ymd(date)) %>%
  mutate(year = year(date)) %>%
  left_join(player_names, by = c('pitcher_id' = 'id')) %>%
  select(pitcher_id ,pitcher_name,px,pz,start_speed,end_speed,break_angle,break_length,pitch_type,event,year) %>%
  rbind(pitchData2019) %>%
  as.data.table()


#  join the player master file with Pitch Data to get playerId numbers (need to calculate the players age for each year)
Master =
Master %>%
  mutate(pitcher_name = paste(nameFirst,nameLast,sep="_")) %>%
  select(pitcher_name,birthYear,birthMonth,throws) %>%
  filter(birthYear > 1975)

PitchData = 
PitchData %>%
  left_join(Master, by = c('pitcher_name')) 

write.csv(PitchData, file = paste0(path,"PitchDataAll.csv"),row.names = FALSE)

#### Clear data and then
#### run Marcel_Projection_Pitching.R
  
# calculate the the top 20 stikeouts per year

StrikeOuts <- 
PitchData %>%
  filter(event == "Strikeout") %>%
  select(event,pitcher_id,year,pitcher_name) %>%
  group_by(year,pitcher_id,pitcher_name)%>%
  summarise(so = n()) %>%
  group_by(year) %>%
  arrange(desc(so)) %>%
  group_by(year) %>%
  top_n(10,so) %>%
  arrange(year) %>%
  as.data.table()

Pitch <-
PitchData %>%
  filter(pitch_type == 'FF' | pitch_type == 'FT' | pitch_type == 'FS') %>%
  group_by(pitcher_id,pitcher_name,pitch_type,year) %>%
  summarise(so = n(),
            avg_start_speed = mean(start_speed,na.rm = TRUE),
            max_start_speed = max(start_speed, na.rm = TRUE),
            avg_end_speed = mean(end_speed,na.rm = TRUE),
            max_end_speed = max(end_speed, na.rm = TRUE),
            avg_break_length = mean(break_length,na.rm = TRUE),
            max_break_length = max(break_length, na.rm = TRUE),
            avg_break_angle = mean(abs(break_angle),na.rm = TRUE),
            max_break_angle = max(abs(break_angle), na.rm = TRUE))


PitchData %>%
  filter(pitcher_id %in% unique(StrikeOuts$pitcher_id)) %>%
  filter(pitch_type == 'FF' | pitch_type == 'FT' |pitch_type == 'FC') %>%
  group_by(pitcher_id,pitcher_name,pitch_type,year) %>%
  summarise(so = n(),
            avg_start_speed = mean(start_speed,na.rm = TRUE),
            max_start_speed = max(start_speed, na.rm = TRUE),
            avg_end_speed = mean(end_speed,na.rm = TRUE),
            max_end_speed = max(end_speed, na.rm = TRUE),
            avg_break_length = mean(break_length,na.rm = TRUE),
            max_break_length = max(break_length, na.rm = TRUE),
            avg_break_angle = mean(abs(break_angle),na.rm = TRUE),
            max_break_angle = max(abs(break_angle), na.rm = TRUE)) %>%
  ggplot(aes(x=max_start_speed,y=so))+
     geom_point()+
     geom_smooth(method = 'lm', formula = y~x,se=FALSE)+
     facet_grid(year~pitch_type)

PitchData %>%
  filter(pitcher_id %in% unique(StrikeOuts$pitcher_id)) %>%
  filter(pitch_type == 'FF' | pitch_type == 'FT' |pitch_type == 'FC') %>%
  group_by(pitcher_id,pitcher_name,pitch_type,year) %>%
  summarise(so = n(),
            avg_start_speed = mean(start_speed,na.rm = TRUE),
            max_start_speed = max(start_speed, na.rm = TRUE),
            avg_end_speed = mean(end_speed,na.rm = TRUE),
            max_end_speed = max(end_speed, na.rm = TRUE),
            avg_break_length = mean(break_length,na.rm = TRUE),
            max_break_length = max(break_length, na.rm = TRUE),
            avg_break_angle = mean(abs(break_angle),na.rm = TRUE),
            max_break_angle = max(abs(break_angle), na.rm = TRUE)) %>%
  ggplot(aes(x=avg_start_speed,y=so))+
  geom_point()+
  geom_smooth(method = 'lm', formula = y~x,se=FALSE)+
  facet_grid(year~pitch_type)

PitchData %>%
  filter(pitcher_name %in% unique(StrikeOuts$pitcher_name)& !is.na(pitcher_name)) %>%
  filter(pitch_type == 'FF' | pitch_type == 'FT' ) %>%
  group_by(pitcher_id,pitcher_name,pitch_type,year) %>%
  summarise(so = n(),
            avg_start_speed = mean(start_speed,na.rm = TRUE),
            max_start_speed = max(start_speed, na.rm = TRUE),
            avg_end_speed = mean(end_speed,na.rm = TRUE),
            max_end_speed = max(end_speed, na.rm = TRUE),
            avg_break_length = mean(break_length,na.rm = TRUE),
            max_break_length = max(break_length, na.rm = TRUE),
            avg_break_angle = mean(abs(break_angle),na.rm = TRUE),
            max_break_angle = max(abs(break_angle), na.rm = TRUE)) %>%
  ggplot(aes(x=year,y=max_start_speed,color = pitcher_name))+
  geom_point()+
  geom_line()+
  facet_grid(.~pitch_type)

PitchData %>%
  filter(pitcher_name %in% unique(StrikeOuts$pitcher_name)& !is.na(pitcher_name)) %>%
  filter(pitch_type == 'FF' | pitch_type == 'FT' ) %>%
  group_by(pitcher_id,pitcher_name,pitch_type,year) %>%
  summarise(so = n(),
            avg_start_speed = mean(start_speed,na.rm = TRUE),
            max_start_speed = max(start_speed, na.rm = TRUE),
            avg_end_speed = mean(end_speed,na.rm = TRUE),
            max_end_speed = max(end_speed, na.rm = TRUE),
            avg_break_length = mean(break_length,na.rm = TRUE),
            max_break_length = max(break_length, na.rm = TRUE),
            avg_break_angle = mean(abs(break_angle),na.rm = TRUE),
            max_break_angle = max(abs(break_angle), na.rm = TRUE)) %>%
  ggplot(aes(x=year,y=avg_start_speed,color = pitcher_name))+
  geom_point()+
  geom_line()+
  facet_grid(.~pitch_type)


PitchData %>%
  #filter(pitcher_id %in% unique(StrikeOuts$pitcher_id)) %>%
  filter(pitch_type == 'FF' | pitch_type == 'FT' ) %>%
  group_by(pitcher_id,pitcher_name,pitch_type,year) %>%
  summarise(so = n(),
            avg_start_speed = mean(start_speed,na.rm = TRUE),
            max_start_speed = max(start_speed, na.rm = TRUE),
            avg_end_speed = mean(end_speed,na.rm = TRUE),
            max_end_speed = max(end_speed, na.rm = TRUE),
            avg_break_length = mean(break_length,na.rm = TRUE),
            max_break_length = max(break_length, na.rm = TRUE),
            avg_break_angle = mean(abs(break_angle),na.rm = TRUE),
            max_break_angle = max(abs(break_angle), na.rm = TRUE)) %>%
  ggplot(aes(x=avg_start_speed,y=so))+
  geom_point()+
  facet_grid(year~pitch_type)
