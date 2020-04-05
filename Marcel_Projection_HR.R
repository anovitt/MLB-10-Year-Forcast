library(data.table)
library(tidyverse)
#library(Lahman)
library(lubridate)
library(broom)

# load Lahman data including 2019

path <- c("R/MLB_Vault/baseballdatabank-master/core/")

Batting <- fread(paste(path,'Batting.csv',sep = ""))
Master <- fread(paste(path,'people.csv',sep = ""))
Pitching <- fread(paste(path,'Pitching.csv',sep = ""))
# Enter the year to prdict

pred.year <- 2020

# league averages

leagueAvg <-
  Batting %>%
  filter(yearID >= pred.year - 3 & yearID < pred.year ) %>%
  left_join(select(Pitching,playerID,yearID,BFP), by = c('playerID','yearID')) %>%
  mutate(PA = AB + BB + HBP + SF + SH) %>%
  filter(PA > BFP | is.na(BFP)) %>%
  group_by(yearID) %>%
  summarise(lgPA = sum(PA), lgAB = sum(AB)/sum(PA), lgR = sum(R)/sum(PA)
            , lgH = sum(H)/sum(PA), lgX2B = sum(`2B`)/sum(PA), lgX3B = sum(`3B`)/sum(PA), lgHR = sum(HR)/sum(PA)
            , lgRBI = sum(RBI)/sum(PA), lgSB = sum(SB)/sum(PA), lgCS = sum(CS)/sum(PA), lgBB = sum(BB)/sum(PA)
            , lgSO = sum(SO)/sum(PA), lgIBB = sum(IBB)/sum(PA), lgHBP = sum(HBP)/sum(PA), lgSH = sum(SH)/sum(PA)
            , lgSF = sum(SF)/sum(PA), lgGIDP = sum(GIDP)/sum(PA)) 


# subset the batting data, remove pitchers based on PA, t is the age weight

batDat <-
Batting %>%
  filter(yearID >= pred.year - 3 & yearID < pred.year ) %>%
  left_join(select(Pitching,playerID,yearID,BFP), by = c('playerID','yearID')) %>%
  mutate(PA = AB + BB + HBP + SF + SH) %>%
  filter(PA > BFP | is.na(BFP)) %>%
  group_by(playerID, yearID) %>%
  summarise(PA = sum(PA), AB = sum(AB), R = sum(R)
            , H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR)
            , RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB)
            , SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH)
            , SF = sum(SF), GIDP = sum(GIDP)) %>%
  mutate(t = ifelse(pred.year - yearID == 1, 5, ifelse(pred.year - yearID == 2, 4, 3))) %>% 
  mutate(inProd = t %*% HR ) %>%
  left_join(leagueAvg, by = c('yearID'))

stats = c("PA", "AB", "R", "H", "X2B", "X3B", "HR", "RBI", "SB", "CS", "BB", "SO", "IBB", "HBP", "SH", "SF", "GIDP")
stats.lg = paste("lg", stats, sep="")

t.X <- batDat$t * select(batDat,stats)[,-1]
t.n <- batDat$t * batDat$PA
t.n0 = batDat$t * 100
t.P0 = batDat$t * batDat$PA *select(batDat,stats.lg)[,-1] 

mPA = with(batDat, ifelse(pred.year - yearID == 1, 0.5 * PA, ifelse(pred.year - yearID == 2, 0.1 * PA, 200)))

Q =
batDat %>%
  select(playerID,yearID) %>%
  add_column(t.X = batDat$t * select(batDat,stats)[,-1],
         t.n = batDat$t * batDat$PA,
         t.n0 = batDat$t * 100,
         t.P0 = batDat$t * batDat$PA *select(batDat,stats.lg)[,-1] ,
         mPA = with(batDat, ifelse(pred.year - yearID == 1, 0.5 * PA, ifelse(pred.year - yearID == 2, 0.1 * PA, 200)))
  ) %>%
  as.data.table()

res = 
Q %>%
  group_by(playerID) %>%
  summarise(numSeasons = length(t.n), 
            reliability = sum(t.n)/(sum(t.n) + sum(t.n0)),
            tn = sum(t.n),
            PA = sum(t.X.PA),
            lgPA = sum(t.P0.lgPA), 
            mPA = sum(mPA),
            AB = sum(t.X.AB), 
            lgAB = sum(t.P0.lgAB), 
            R = sum(t.X.R), 
            lgR = sum(t.P0.lgR),
            H = sum(t.X.H), 
            lgH = sum(t.P0.lgH), 
            X2B = sum(t.X.X2B), 
            lgX2B = sum(t.P0.lgX2B),
            X3B = sum(t.X.X3B), 
            lgX3B = sum(t.P0.lgX3B), 
            HR = sum(t.X.HR),  
            lgHR = sum(t.P0.lgHR),
            RBI = sum(t.X.RBI), 
            lgRBI = sum(t.P0.lgRBI), 
            SB = sum(t.X.SB),  
            lgSB = sum(t.P0.lgSB),
            CS = sum(t.X.CS), 
            lgCS = sum(t.P0.lgCS), 
            BB = sum(t.X.BB),  
            lgBB = sum(t.P0.lgBB),
            SO = sum(t.X.SO), 
            lgSO = sum(t.P0.lgSO), 
            IBB = sum(t.X.IBB),  
            lgIBB = sum(t.P0.lgIBB),
            HBP = sum(t.X.HBP), 
            lgHBP = sum(t.P0.lgHBP), 
            SH = sum(t.X.SH),  
            lgSH = sum(t.P0.lgSH),
            SF = sum(t.X.SF), 
            lgSF = sum(t.P0.lgSF), 
            GIDP = sum(t.X.GIDP),  
            lgGIDP = sum(t.P0.lgGIDP)) %>%
  left_join(select(Master,playerID, birthYear),by= c('playerID')) %>%
  mutate(age = pred.year - birthYear)

stats.proj = setdiff(stats, "PA")
stats.m = paste("m", stats.proj, sep="")
stats.lg.proj = paste("lg", stats.proj, sep="")
res[, stats.m] = with(res, (reliability * res[, stats.proj]) / tn + (1 - reliability) * res[, stats.lg.proj] / tn)

res$age.adj = with(res, ifelse(age > 29, 0.003 * (age - 29), 0.006 * (age - 29)))
res[, stats.m] = res[, stats.m] * (1 + res$age.adj)
res[, stats.m] = res[, stats.m] * res$mPA

res %>%
  filter(playerID == 'cruzne02') %>%
  select("reliability", "age.adj", "tn", "mPA", "HR", "lgHR", "mHR","age")

top_20 =
res %>%
  select(playerID, HR, mHR, age) %>%
  filter(age < 31) %>%
  top_n(20,mHR) %>%
  arrange(desc(mHR)) %>%
  pull(playerID) 

top_20_Hits =
  res %>%
  select(playerID, H, mH, age) %>%
  filter(age < 30) %>%
  top_n(20,mH) %>%
  arrange(desc(mH)) %>%
  pull(playerID) 

Master %>% 
  filter(playerID %in% top_20) %>%
  select('playerID','nameFirst', 'nameLast') %>%
  arrange(nameLast)

Master %>% 
  filter(playerID %in% top_20_Hits) %>%
  select('playerID','nameFirst', 'nameLast') %>%
  arrange(nameLast)

# build data table from sums from for the decade 
# trying regression data from the top 25 player for the decade actual

# get the names of the top 25 players from the 1990 to 2000 projection, add the age
  
battingDatReg_top_25 =
Batting %>%
    filter(yearID >= 1990 & yearID < 2010) %>%
    mutate(PA = AB + BB + HBP + SF + SH) %>%
    group_by(playerID) %>%
    summarise(PA = sum(PA), AB = sum(AB), R = sum(R)
              , H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR)
              , RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB)
              , SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH)
              , SF = sum(SF), GIDP = sum(GIDP)) %>%
  top_n(25, HR) %>%
  pull(playerID)

battingDatReg_top_25_Hits =
  Batting %>%
  filter(yearID >= 1990 & yearID < 2010) %>%
  mutate(PA = AB + BB + HBP + SF + SH) %>%
  group_by(playerID) %>%
  summarise(PA = sum(PA), AB = sum(AB), R = sum(R)
            , H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR)
            , RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB)
            , SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH)
            , SF = sum(SF), GIDP = sum(GIDP)) %>%
  top_n(25, H) %>%
  pull(playerID)

# regression of prediction year

battingDatRegHR =
Batting %>%
  filter(playerID %in% battingDatReg_top_25) %>%
  mutate(PA = AB + BB + HBP + SF + SH) %>%
  group_by(playerID, yearID) %>%
  summarise(PA = sum(PA), AB = sum(AB), R = sum(R)
            , H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR)
            , RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB)
            , SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH)
            , SF = sum(SF), GIDP = sum(GIDP)) %>%
  left_join(select(Master,playerID,birthYear,birthMonth, bats),by=c('playerID')) %>%
  mutate(birthYear = ifelse(birthMonth >= 7, birthYear,birthYear +1)) %>%
  mutate(age = yearID - birthYear) %>%
  select(playerID, age, HR, bats)


modHR <- lm(HR ~ I(age ) + I((age )^2)  , data = battingDatRegHR)
summary(modHR)
b <- coef(modHR)

battingDatRegH =
  Batting %>%
  filter(playerID %in% battingDatReg_top_25_Hits) %>%
  mutate(PA = AB + BB + HBP + SF + SH) %>%
  group_by(playerID, yearID) %>%
  summarise(PA = sum(PA), AB = sum(AB), R = sum(R)
            , H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR)
            , RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB)
            , SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH)
            , SF = sum(SF), GIDP = sum(GIDP)) %>%
  left_join(select(Master,playerID,birthYear,birthMonth, bats),by=c('playerID')) %>%
  mutate(birthYear = ifelse(birthMonth >= 7, birthYear,birthYear +1)) %>%
  mutate(age = yearID - birthYear) %>%
  select(playerID, age, H, bats)


modH <- lm(H ~ I(age ) + I((age )^2)  , data = battingDatRegH)
summary(modH)
bH <- coef(modH)

# All players over 400 HR and 2500 hits
#  get the players names
totalHR <-
  Batting %>%
  mutate(IBB = ifelse(is.na(IBB),0,IBB),
         SF = ifelse(is.na(SF),0,SF),
         CS = ifelse(is.na(CS),0,CS),
         GIDP = ifelse(is.na(GIDP),0,GIDP)) %>%
  group_by(playerID) %>%
  mutate(PA = AB + BB + HBP + SF + SH) %>%
  summarise(PA = sum(PA), AB = sum(AB), R = sum(R)
            , H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR)
            , RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB)
            , SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH)
            , SF = sum(SF), GIDP = sum(GIDP)) %>%
  filter(HR > 400) %>%
  #top_n(50,HomeRuns) %>%
  arrange(desc(HR)) %>%
  left_join(select(Master, playerID, birthMonth,birthYear,bats), by = 'playerID') %>%
  mutate(birthYear = ifelse(birthMonth >= 7, birthYear,birthYear +1)) %>%
  pull(playerID)


totalHits <-
  Batting %>%
  mutate(IBB = ifelse(is.na(IBB),0,IBB),
         SF = ifelse(is.na(SF),0,SF),
         CS = ifelse(is.na(CS),0,CS),
         GIDP = ifelse(is.na(GIDP),0,GIDP)) %>%
  group_by(playerID) %>%
  mutate(PA = AB + BB + HBP + SF + SH) %>%
  summarise(PA = sum(PA), AB = sum(AB), R = sum(R)
            , H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR)
            , RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB)
            , SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH)
            , SF = sum(SF), GIDP = sum(GIDP)) %>%
  filter(H > 2500) %>%
  #top_n(50,HomeRuns) %>%
  arrange(desc(H)) %>%
  left_join(select(Master, playerID, birthMonth,birthYear,bats), by = 'playerID') %>%
  mutate(birthYear = ifelse(birthMonth >= 7, birthYear,birthYear +1)) %>%
  pull(playerID)

# regression for players over 400 carrer HR

totalHRReg =
Batting %>%
  filter(playerID %in% totalHR) %>%
  mutate(IBB = ifelse(is.na(IBB),0,IBB),
         SF = ifelse(is.na(SF),0,SF),
         CS = ifelse(is.na(CS),0,CS),
         GIDP = ifelse(is.na(GIDP),0,GIDP)) %>%
  mutate(PA = AB + BB + HBP + SF + SH) %>%
  group_by(playerID, yearID) %>%
  summarise(PA = sum(PA), AB = sum(AB), R = sum(R)
            , H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR)
            , RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB)
            , SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH)
            , SF = sum(SF), GIDP = sum(GIDP)) %>%
  left_join(select(Master,playerID,birthYear,birthMonth, bats),by=c('playerID')) %>%
  mutate(birthYear = ifelse(birthMonth >= 7, birthYear,birthYear +1)) %>%
  mutate(age = yearID - birthYear) %>%
  select(playerID, age, HR, bats)

modHR <- lm(HR ~ I(age ) + I((age )^2)  , data = totalHRReg)
summary(modHR)
bAll <- coef(modHR)


totalHitsReg =
  Batting %>%
  filter(playerID %in% totalHits) %>%
  mutate(IBB = ifelse(is.na(IBB),0,IBB),
         SF = ifelse(is.na(SF),0,SF),
         CS = ifelse(is.na(CS),0,CS),
         GIDP = ifelse(is.na(GIDP),0,GIDP)) %>%
  mutate(PA = AB + BB + HBP + SF + SH) %>%
  group_by(playerID, yearID) %>%
  summarise(PA = sum(PA), AB = sum(AB), R = sum(R)
            , H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR)
            , RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB)
            , SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH)
            , SF = sum(SF), GIDP = sum(GIDP)) %>%
  left_join(select(Master,playerID,birthYear,birthMonth, bats),by=c('playerID')) %>%
  mutate(birthYear = ifelse(birthMonth >= 7, birthYear,birthYear +1)) %>%
  mutate(age = yearID - birthYear) %>%
  select(playerID, age, H, bats)

modH <- lm(H ~ I(age ) + I((age )^2)  , data = totalHitsReg)
summary(modHR)
bAllHits <- coef(modH)

datPred =
Batting %>%
  filter(playerID %in% top_20) %>% 
  filter(yearID <= pred.year) %>%
  mutate(IBB = ifelse(is.na(IBB),0,IBB),
         SF = ifelse(is.na(SF),0,SF),
         CS = ifelse(is.na(CS),0,CS), 
         GIDP = ifelse(is.na(GIDP),0,GIDP)) %>%
  mutate(PA = AB + BB + HBP + SF + SH) %>%
  group_by(playerID, yearID) %>%
  summarise(PA = sum(PA), AB = sum(AB), R = sum(R)
            , H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR)
            , RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB)
            , SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH)
            , SF = sum(SF), GIDP = sum(GIDP)) %>%
  left_join(select(Master,playerID,birthYear,birthMonth, nameFirst, nameLast),by=c('playerID')) %>%
  mutate(birthYear = ifelse(birthMonth >= 7, birthYear,birthYear +1),
         name = paste(nameFirst, nameLast, sep = "_")) %>%
  mutate(age = yearID - birthYear) %>%
  select(playerID, age, HR, name) 

datPredHits =
  Batting %>%
  filter(playerID %in% top_20_Hits) %>% 
  filter(yearID <= pred.year) %>%
  mutate(IBB = ifelse(is.na(IBB),0,IBB),
         SF = ifelse(is.na(SF),0,SF),
         CS = ifelse(is.na(CS),0,CS), 
         GIDP = ifelse(is.na(GIDP),0,GIDP)) %>%
  mutate(PA = AB + BB + HBP + SF + SH) %>%
  group_by(playerID, yearID) %>%
  summarise(PA = sum(PA), AB = sum(AB), R = sum(R)
            , H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR)
            , RBI = sum(RBI), SB = sum(SB), CS = sum(CS), BB = sum(BB)
            , SO = sum(SO), IBB = sum(IBB), HBP = sum(HBP), SH = sum(SH)
            , SF = sum(SF), GIDP = sum(GIDP)) %>%
  left_join(select(Master,playerID,birthYear,birthMonth, nameFirst, nameLast),by=c('playerID')) %>%
  mutate(birthYear = ifelse(birthMonth >= 7, birthYear,birthYear +1),
         name = paste(nameFirst, nameLast, sep = "_")) %>%
  mutate(age = yearID - birthYear) %>%
  select(playerID, age, H, name) 


battingLm = 
  datPred %>% 
  group_by(playerID) %>%
  do(modHR = lm(HR ~ I(age ) + I((age )^2), data = .))

battingLmCoef = tidy(battingLm, modHR)

battingLmHits = 
  datPredHits %>% 
  group_by(playerID) %>%
  do(modH = lm(H ~ I(age ) + I((age )^2), data = .))

battingLmHitsCoef = tidy(battingLmHits, modH)

topPlayers =
battingLmCoef %>%
  select(playerID,term,estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  filter(`I((age)^2)` > 3 * bAll[3]) %>%
  pull(playerID)

topPlayersHits =
  battingLmHitsCoef %>%
  select(playerID,term,estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  filter(`I((age)^2)` > 4 * bAllHits[3]) %>%
  pull(playerID)

datPred =
datPred %>%
  filter(playerID %in% topPlayers)

datPredHits =
  datPredHits %>%
  filter(playerID %in% topPlayersHits)

datPred_mHR =
res %>%
  filter(age < 31) %>%
  top_n(20,mHR) %>%
  arrange(desc(mHR)) %>%
  left_join(select(Master,playerID, nameFirst, nameLast),by=c('playerID')) %>%
  mutate(name = paste(nameFirst, nameLast, sep = "_"),
         mHR = round(mHR,0)) %>%
  select(playerID, mHR, name) %>%
  as.data.table()

datPred_mHits =
  res %>%
  filter(age < 31) %>%
  top_n(20,mH) %>%
  arrange(desc(mH)) %>%
  left_join(select(Master,playerID, nameFirst, nameLast),by=c('playerID')) %>%
  mutate(name = paste(nameFirst, nameLast, sep = "_"),
         mHR = round(mH,0)) %>%
  select(playerID, mH, name) %>%
  as.data.table()

class(datPred_mHR)

datPred_mHR =
datPred_mHR %>%
  filter(playerID %in% topPlayers) %>%
  mutate(yearID = pred.year) %>%
  left_join(select(Master,playerID,birthYear,birthMonth),by=c('playerID')) %>%
  mutate(birthYear = ifelse(birthMonth >= 7, birthYear,birthYear +1),
         age = yearID - birthYear + 1) 

datPred_mHits =
  datPred_mHits %>%
  filter(playerID %in% topPlayersHits) %>%
  mutate(yearID = pred.year) %>%
  left_join(select(Master,playerID,birthYear,birthMonth),by=c('playerID')) %>%
  mutate(birthYear = ifelse(birthMonth >= 7, birthYear,birthYear +1),
         age = yearID - birthYear + 1) 
  
  
datCruz =
  datPred %>%
  filter(name == 'Nelson_Cruz')

datRegRev <- data.table(age = seq(from = 15, to = 45, by = 1))
datRegRev %>%
  mutate(HR_allTime = bAll[1] + bAll[2] * age + bAll[3] * age *age,
         HR_1990_2000 = b[1] + b[2] * age + b[3] *age*age) %>%
  pivot_longer(-age, names_to = 'variable', values_to = 'HR') %>%
  filter(HR >= 0) %>%
  ggplot()+
    geom_line(aes(x = age, y = HR, color = variable)) +
    geom_line(data = datPred, aes(x=age, y = HR,color = name)) +
    geom_point(data = datPred, aes(x=age, y = HR,color = name)) +
    geom_point(size = I(4),data = datPred_mHR, aes(x=age, y = mHR,color = name)) +
    geom_point(shape = 3, size = I(5), data = datCruz, aes(x= age,y=HR))
  
datRegRevHits <- data.table(age = seq(from = 15, to = 45, by = 1))
datRegRevHits %>%
  mutate(Hits_allTime = bAllHits[1] + bAllHits[2] * age + bAllHits[3] * age *age,
         Hits_1990_2000 = bH[1] + bH[2] * age + bH[3] *age*age) %>%
  pivot_longer(-age, names_to = 'variable', values_to = 'Hits') %>%
  filter(Hits >= 0) %>%
  ggplot()+
  geom_line(aes(x = age, y = Hits, color = variable)) +
  geom_line(data = datPredHits, aes(x=age, y = H,color = name)) +
  geom_point(data = datPredHits, aes(x=age, y = H,color = name)) +
  geom_point(size = I(4),data = datPred_mHits, aes(x=age, y = mH,color = name)) 

Pichers
