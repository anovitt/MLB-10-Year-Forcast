library(data.table)
library(tidyverse)
#library(Lahman)
library(lubridate)
library(broom)
library(mlbgameday)

path <- c("R/MLB_Vault/mlb-pitch-data-20152018/")

PitchData <- fread(paste0(path,"PitchDataAll.csv"))

# Add age to the PitchData

PitchData <-
PitchData %>%
  mutate(birthYear = ifelse(birthMonth >= 7, birthYear,birthYear +1)) %>%
  mutate(age = year - birthYear) 

PitchData %>%
  filter(pitcher_id == 621111)

pitcherID <- 
  PitchData %>%
  select(pitcher_id,age,throws,pitcher_name) %>%
  group_by(pitcher_id,pitcher_name) %>%
  summarise(age = max(age)) %>%
  mutate(age = ifelse(is.na(age),0,age)) %>%
  as.data.table()

pred.year <- 2020

# number of pitches thrown per year

numPitches =
PitchData %>%
  group_by(year, pitcher_id) %>%
  summarise(nPitch = n()) %>%
  as.data.table()

PitchData =  
PitchData %>%
  left_join(numPitches, by = c('year','pitcher_id')) %>%
  filter(nPitch > 200)

names(PitchData)
# league averages

numPitchYear=
  PitchData%>%
  filter(year >= pred.year - 3 & year < pred.year ) %>%
  group_by(year) %>%
  summarise(lgNumPitch = n())

leagueAvg <-
  PitchData %>%
  filter(year >= pred.year - 3 & year < pred.year ) %>%
  group_by(year) %>%
  summarise(lgNumPitch = n(), 
            lgSO = sum(grepl('Strikeout',event),na.rm=TRUE)/n(), 
            lgGrndOut = sum(grepl('Groundout',event),na.rm=TRUE)/n(),
            lgH = sum(grepl('Single',event),na.rm=TRUE)/n(), 
            lgX2B = sum(grepl('Double',event),na.rm=TRUE)/n(), 
            lgX3B = sum(grepl('Triple',event),na.rm=TRUE)/n(), 
            lgHR = sum(grepl('Home Run',event),na.rm=TRUE)/n())

leagueAvgPitchSpeed =
  PitchData %>%
  filter( pitch_type == "FF" |  pitch_type == "FT") %>%
  filter(year >= pred.year - 3 & year < pred.year ) %>% 
  group_by(year,pitch_type) %>%
  summarise(lgSp = mean(start_speed,na.rm = TRUE)) %>%
  pivot_wider(names_from = pitch_type, values_from = lgSp) %>%
  as.data.table()

leagueAvg <-    
  leagueAvg %>%
  left_join(leagueAvgPitchSpeed, by = 'year') %>%
  rename(lgFF = FF,lgFT = FT)


##########################


pitchDat <-
  PitchData %>%
  filter(year >= pred.year - 3 & year < pred.year ) %>%
  group_by(pitcher_id, year) %>%
  summarise(NumPitch = n(), 
            SO = sum(grepl('Strikeout',event)), 
            GrndOut = sum(grepl('Groundout',event)),
            H = sum(grepl('Single',event)), 
            X2B = sum(grepl('Double',event)), 
            X3B = sum(grepl('Triple',event)), 
            HR = sum(grepl('Home Run',event)), 
            Out = sum(grepl('out',event)))
 
names(PitchData)
 
AvgPitchSpeed =
  PitchData %>%
  filter( pitch_type == "FF" |  pitch_type == "FT") %>%
  filter(year >= pred.year - 3 & year < pred.year ) %>% 
  group_by(pitcher_id,year,pitch_type) %>%
  summarise(Sp = mean(start_speed,na.rm = TRUE)) %>%
  mutate(Sp = ifelse(is.na(Sp),0,Sp)) %>%
  pivot_wider(names_from = c(pitch_type), values_from = Sp) %>%
  mutate(FF = ifelse(is.na(FF),0,FF),
         FT = ifelse(is.na(FT),0,FT)) %>%
  as.data.table()  

pitchDat =  
pitchDat %>%
  left_join(AvgPitchSpeed, by = c('year','pitcher_id'))

names(PitchData)
 
pitchDat = 
pitchDat %>%
  mutate(t = ifelse(pred.year - year == 1, 5, ifelse(pred.year - year == 2, 4, 3))) %>% 
  #mutate(inProd = t %*% HR ) %>%
  left_join(leagueAvg, by = c('year'))

names(PitchData)

stats = c("SO", "GrndOut", "H", "X2B", "X3B", "HR",  "FF", "FT")
stats.lg = paste0("lg", stats)

t.X <- pitchDat$t * select(pitchDat,stats)[,-1]
t.n <- pitchDat$t * pitchDat$NumPitch
t.n0 = pitchDat$t * 100
t.P0 = pitchDat$t * pitchDat$NumPitch *select(pitchDat,stats.lg)[,-1] 

mNumPitch = with(pitchDat, ifelse(pred.year - year == 1, 0.5 * NumPitch, ifelse(pred.year - year == 2, 0.1 * NumPitch, 200)))


Q =
  pitchDat %>%
  select(pitcher_id,year) %>%
  add_column(t.X = pitchDat$t * select(pitchDat,stats)[,-1],
             t.n = pitchDat$t * pitchDat$NumPitch,
             t.n0 = pitchDat$t * 100,
             t.P0 = pitchDat$t * pitchDat$NumPitch *select(pitchDat,stats.lg)[,-1] ,
             mNumPitch = with(pitchDat, ifelse(pred.year - year == 1, 0.5 * NumPitch, ifelse(pred.year - year == 2, 0.1 * NumPitch, 200)))) %>%
  as.data.table()

res = 
  Q %>%
  group_by(pitcher_id) %>%
  summarise(numSeasons = length(t.n), 
            reliability = sum(t.n)/(sum(t.n) + sum(t.n0)),
            tn = sum(t.n),
            #NumPitch = sum(t.X.NumPitch),
            #lgNumPitch = sum(t.P0.lgNumPitch), 
            mNumPitch = sum(mNumPitch),
            SO = sum(t.X.SO), 
            lgSO = sum(t.P0.lgSO), 
            GrndOut = sum(t.X.GrndOut), 
            lgGrndOut = sum(t.P0.lgGrndOut),
            H = sum(t.X.H), 
            lgH = sum(t.P0.lgH), 
            X2B = sum(t.X.X2B), 
            lgX2B = sum(t.P0.lgX2B),
            X3B = sum(t.X.X3B), 
            lgX3B = sum(t.P0.lgX3B), 
            HR = sum(t.X.HR),  
            lgHR = sum(t.P0.lgHR),
            #Out = sum(t.X.Out), 
            #lgOut = sum(t.P0.lgOut), 
            FF = sum(t.X.FF),  
            lgFF = sum(t.P0.lgFF),
            FT = sum(t.X.FT), 
            lgFT = sum(t.P0.lgFT), 
            ) %>% 
  inner_join(pitcherID,by= c('pitcher_id')) 

head(as.data.table(res))
 
stats.proj = setdiff(stats, "NumPitch")
stats.m = paste("m", stats.proj, sep="")
stats.lg.proj = paste("lg", stats.proj, sep="")
res[, stats.m] = with(res, (reliability * res[, stats.proj]) / tn + (1 - reliability) * res[, stats.lg.proj] / tn)

res$age.adj = with(res, ifelse(age > 29, 0.003 * (age - 29), 0.006 * (age - 29)))
res[, stats.m] = res[, stats.m] * (1 + res$age.adj)
res[, stats.m] = res[, stats.m] * res$mNumPitch

res<- as.data.table(res)
res

top_20_SO =
  res %>%
  select(pitcher_id, SO, mSO, age,pitcher_name) %>%
  filter(age < 26) %>%
  top_n(20,mSO) %>%
  arrange(desc(mSO))

top_20_FF =
  res %>%
  select(pitcher_id, SO, mSO, FF, mFF, age,pitcher_name) %>%
  filter(age < 26) %>%
  top_n(20,mFF) %>%
  arrange(desc(mFF))


PitchSpeedplayer <- 
  PitchData %>%
  filter(pitch_type == "FF") %>%
  select(start_speed,pitcher_id,year) %>%
  group_by(year,pitcher_id)%>%
  summarise(start_speed = mean(start_speed)) 


StrikeOuts <- 
  PitchData %>%
  filter(event == "Strikeout") %>%
  select(event,pitcher_id,year,pitcher_name) %>%
  group_by(year,pitcher_id,pitcher_name)%>%
  summarise(so = n()) %>%
  left_join(select(pitcherID,pitcher_id,age), by = 'pitcher_id') %>%
  filter(age < 26) %>%
  group_by(year) %>%
  arrange(desc(so)) %>%
  group_by(year) %>%
  top_n(10,so) %>%
  arrange(year) %>% 
  rename(strikeout = so) 

StrikeOuts <-  
StrikeOuts %>%  
left_join(select(PitchSpeedplayer,pitcher_id,year,start_speed), by = c('pitcher_id','year'))


StrikeOuts %>%
  full_join(select(top_20_SO,pitcher_id,mSO), by = 'pitcher_id') %>%
  filter(year == 2019 & !is.na(pitcher_name))
  