
source(file = 'Baseball_Fantasy_Data_Load.R')

#####

datBatDT =
datBat %>%
  #filter(Name == 'Nelson Cruz'|Name == 'C.J. Cron') %>%
  mutate(HRpct = HRpct(.),
         Kpct = Kpct(.)) %>%
  arrange(Name,Date) %>%
  group_by(Name) %>%
    mutate(rate_HR = HR-lag(HR,default = NA),
           rate_RBI = RBI-lag(RBI,default = NA),
           rate_PA = PA-lag(PA,default = NA),
           rate_AB = AB-lag(AB,default = NA),
           rate_R = R-lag(R,default = NA),
           rate_H = H-lag(H,default = NA),
           rate_BA = BA-lag(BA,default = NA),
           rate_OBP = OBP-lag(OBP,default = NA),
           rate_SLG = SLG-lag(SLG,default = NA),
           rate_OPS = OPS-lag(OPS,default = NA),
           rate_SB = SB-lag(SB,default = NA)) %>%
     replace_na(list(rate_HR = 0,rate_RBI =0 ,rate_PA =0,rate_AB=0,rate_R=0, rate_H=0,
                     rate_BA=0,rate_OBP=0,rate_SLG=0,
                     rate_OPS=0,  rate_SB=0)) %>%
  mutate(
           roll_rate_HR = zoo::rollapply(rate_HR,4,mean,align='right',fill=0),
           roll_rate_RBI = zoo::rollapply(rate_RBI,4,mean,align='right',fill=0),
           roll_rate_PA = zoo::rollapply(rate_PA,4,mean,align='right',fill=0),
           roll_rate_AB = zoo::rollapply(rate_AB,4,mean,align='right',fill=0),
           roll_rate_R = zoo::rollapply(rate_R,4,mean,align='right',fill=0),
           roll_rate_H = zoo::rollapply(rate_H,4,mean,align='right',fill=0),
           roll_rate_BA = zoo::rollapply(rate_BA,4,mean,align='right',fill=0),
           roll_rate_OBP = zoo::rollapply(rate_OBP,4,mean,align='right',fill=0),
           roll_rate_SLG = zoo::rollapply(rate_SLG,4,mean,align='right',fill=0),
           roll_rate_OPS = zoo::rollapply(rate_OPS,4,mean,align='right',fill=0),
           roll_rate_SB = zoo::rollapply(rate_SB,4,mean,align='right',fill=0),
            ) %>%
  ungroup() %>%
  as.data.table()
  
##### inner join HR
datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(HR,0.75)<HR & quantile(roll_rate_HR,0.50)<roll_rate_HR) %>%
  arrange(desc(HR)) %>%
  as.data.table() %>%
  inner_join(B_Team_Avail_Bat, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(HR,0.75)<HR & quantile(roll_rate_HR,0.50)<roll_rate_HR) %>%
  arrange(desc(HR)) %>%
  as.data.table() %>%
  inner_join(F_Team_Avail_Bat, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(HR,0.75)<HR & quantile(roll_rate_HR,0.50)<roll_rate_HR) %>%
  arrange(desc(HR)) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

# Roster good Peformance

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(HR,0.75)<HR & quantile(roll_rate_HR,0.50)<roll_rate_HR) %>%
  arrange(desc(HR)) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(HR,0.75)<HR & quantile(roll_rate_HR,0.50)<roll_rate_HR) %>%
  arrange(desc(HR)) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))
# Roster Under Peforming

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(HR,0.50)>HR & quantile(roll_rate_HR,0.50)>roll_rate_HR) %>%
  arrange(desc(HR)) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(HR,0.50)>HR & quantile(roll_rate_HR,0.50)>roll_rate_HR) %>%
  arrange(desc(HR)) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))

#### inner join RBI  
datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(RBI,0.75)<RBI & quantile(roll_rate_RBI,0.50)<roll_rate_RBI) %>%
  arrange(desc(RBI)) %>%
  as.data.table()  %>%
  inner_join(B_Team_Avail_Bat, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(RBI,0.75)<RBI & quantile(roll_rate_RBI,0.50)<roll_rate_RBI) %>%
  arrange(desc(RBI)) %>%
  as.data.table()  %>%
  inner_join(F_Team_Avail_Bat, by = c("Name" = "Player"))

# Roster Good Perf
datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(RBI,0.75)<RBI & quantile(roll_rate_RBI,0.50)<roll_rate_RBI) %>%
  arrange(desc(RBI)) %>%
  as.data.table()  %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(RBI,0.75)<RBI & quantile(roll_rate_RBI,0.50)<roll_rate_RBI) %>%
  arrange(desc(RBI)) %>%
  as.data.table()  %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))

# Roster Under Perf
datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(RBI,0.50)>RBI & quantile(roll_rate_RBI,0.50)>roll_rate_RBI) %>%
  arrange(desc(RBI)) %>%
  as.data.table()  %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(RBI,0.50)>RBI & quantile(roll_rate_RBI,0.50)>roll_rate_RBI) %>%
  arrange(desc(RBI)) %>%
  as.data.table()  %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))

#inner join hits

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(H,0.75)<H & quantile(roll_rate_H,0.50)<roll_rate_H) %>%
  arrange(desc(H)) %>%
  as.data.table() %>%
  inner_join(B_Team_Avail_Bat, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(H,0.75)<H & quantile(roll_rate_H,0.50)<roll_rate_H) %>%
  arrange(desc(H)) %>%
  as.data.table() %>%
  inner_join(F_Team_Avail_Bat, by = c("Name" = "Player"))
 
# inner join stolen base
datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SB,0.75)<SB & quantile(roll_rate_SB,0.50)<roll_rate_SB) %>%
  arrange(desc(SB)) %>%
  as.data.table()  %>%
  inner_join(B_Team_Avail_Bat, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SB,0.75)<SB & quantile(roll_rate_SB,0.50)<roll_rate_SB) %>%
  arrange(desc(SB)) %>%
  as.data.table()  %>%
  inner_join(F_Team_Avail_Bat, by = c("Name" = "Player"))


# roster Good Perf
datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SB,0.75)<SB & quantile(roll_rate_SB,0.50)<roll_rate_SB) %>%
  arrange(desc(SB)) %>%
  as.data.table()  %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SB,0.75)<SB & quantile(roll_rate_SB,0.50)<roll_rate_SB) %>%
  arrange(desc(SB)) %>%
  as.data.table()  %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))

# Roster underperf
datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SB,0.50)>SB & quantile(roll_rate_SB,0.50)>roll_rate_SB) %>%
  arrange(desc(SB)) %>%
  as.data.table()  %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SB,0.50)>SB & quantile(roll_rate_SB,0.50)>roll_rate_SB) %>%
  arrange(desc(SB)) %>%
  as.data.table()  %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))

#### inner join BA

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(BA,0.75,na.rm = TRUE)<BA & quantile(roll_rate_BA,0.50)<roll_rate_BA) %>%
  arrange(desc(BA)) %>%
  as.data.table() %>%
  inner_join(B_Team_Avail_Bat, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(BA,0.75,na.rm = TRUE)<BA & quantile(roll_rate_BA,0.50)<roll_rate_BA) %>%
  arrange(desc(BA)) %>%
  as.data.table() %>%
  inner_join(F_Team_Avail_Bat, by = c("Name" = "Player"))

# Roster Good Perf
datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(BA,0.75,na.rm = TRUE)<BA & quantile(roll_rate_BA,0.50)<roll_rate_BA) %>%
  arrange(desc(BA)) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(BA,0.75,na.rm = TRUE)<BA & quantile(roll_rate_BA,0.50)<roll_rate_BA) %>%
  arrange(desc(BA)) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(BA,0.75,na.rm = TRUE)>BA & quantile(roll_rate_BA,0.50)>roll_rate_BA) %>%
  arrange(desc(BA)) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datBatDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(BA,0.75,na.rm = TRUE)>BA & quantile(roll_rate_BA,0.50)>roll_rate_BA) %>%
  arrange(desc(BA)) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))


##########
# Pitching
##########

datPitchDT =
  datPitch %>%
  #filter(Name == 'Nelson Cruz'|Name == 'C.J. Cron') %>%
  #mutate(HRpct = HRpct(.),
  #       Kpct = Kpct(.)) %>%
  arrange(Name,Date) %>%
  group_by(Name) %>%
  mutate(rate_W = W-lag(W,default = NA),
         rate_L = L-lag(L,default = NA),
         rate_SV = SV-lag(SV,default = NA),
         rate_IP = IP-lag(IP,default = NA),
         rate_H = H-lag(H,default = NA),
         rate_R = R-lag(R,default = NA),
         rate_ER = ER-lag(ER,default = NA),
         rate_ERA = ERA-lag(ERA,default = NA),
         rate_WHIP = WHIP-lag(WHIP,default = NA),
         rate_SO = SO-lag(SO,default = NA)) %>%
  replace_na(list(rate_W = 0,rate_L =0 ,rate_SV =0,rate_IP=0,rate_R=0, rate_H=0,
                  rate_ER=0,rate_ERA=0,rate_WHIP=0,
                  rate_SO=0)) %>%
  mutate(
    roll_rate_W = zoo::rollapply(rate_W,4,mean,align='right',fill=0),
    roll_rate_L = zoo::rollapply(rate_L,4,mean,align='right',fill=0),
    roll_rate_SV = zoo::rollapply(rate_SV,4,mean,align='right',fill=0),
    roll_rate_IP = zoo::rollapply(rate_IP,4,mean,align='right',fill=0),
    roll_rate_R = zoo::rollapply(rate_R,4,mean,align='right',fill=0),
    roll_rate_H = zoo::rollapply(rate_H,4,mean,align='right',fill=0),
    roll_rate_ER = zoo::rollapply(rate_ER,4,mean,align='right',fill=0),
    roll_rate_ERA = zoo::rollapply(rate_ERA,4,mean,align='right',fill=0),
    roll_rate_WHIP = zoo::rollapply(rate_WHIP,4,mean,align='right',fill=0),
    roll_rate_SO = zoo::rollapply(rate_SO,4,mean,align='right',fill=0)
  ) %>%
  ungroup() %>% 
   replace(is.na(.),0) %>%
  as.data.table()

## inner join wins
datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(W,0.75)<W & quantile(roll_rate_W,0.50)<roll_rate_W) %>%
  arrange(desc(W)) %>%
  as.data.table() %>%
  inner_join(B_Team_Avail_Pitch, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(W,0.75)<W & quantile(roll_rate_W,0.50)<roll_rate_W) %>%
  arrange(desc(W)) %>%
  as.data.table() %>%
  inner_join(F_Team_Avail_Pitch, by = c("Name" = "Player"))


## Good Perf
datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(W,0.75)<W & quantile(roll_rate_W,0.50)<roll_rate_W) %>%
  arrange(desc(W)) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(W,0.75)<W & quantile(roll_rate_W,0.50)<roll_rate_W) %>%
  arrange(desc(W)) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))

## Poor Perf
datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(W,0.75)>W & quantile(roll_rate_W,0.50)>roll_rate_W) %>%
  arrange(desc(W)) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(W,0.75)>W & quantile(roll_rate_W,0.50)>roll_rate_W) %>%
  arrange(desc(W)) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))





# inner join strike outs
datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SO,0.75)< SO & quantile(roll_rate_SO,0.50)<roll_rate_SO) %>%
  arrange(desc(L)) %>%
  as.data.table() %>%
  inner_join(B_Team_Avail_Pitch, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SO,0.75)< SO & quantile(roll_rate_SO,0.50)<roll_rate_SO) %>%
  arrange(desc(L)) %>%
  as.data.table() %>%
  inner_join(F_Team_Avail_Pitch, by = c("Name" = "Player"))

# Good Perf

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SO,0.75)< SO & quantile(roll_rate_SO,0.50)<roll_rate_SO) %>%
  arrange(desc(L)) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SO,0.75)< SO & quantile(roll_rate_SO,0.50)<roll_rate_SO) %>%
  arrange(desc(L)) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))

# Poor Perf
datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SO,0.75)> SO & quantile(roll_rate_SO,0.50)>roll_rate_SO) %>%
  arrange(desc(L)) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SO,0.75)> SO & quantile(roll_rate_SO,0.50)>roll_rate_SO) %>%
  arrange(desc(L)) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))



# inner join saves
datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SV,0.75)< SV & quantile(roll_rate_SV,0.50)<roll_rate_SV) %>%
  arrange(desc(SV)) %>%
  as.data.table() %>%
  inner_join(B_Team_Avail_Pitch_RP, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SV,0.75)< SV & quantile(roll_rate_SV,0.50)<roll_rate_SV) %>%
  arrange(desc(SV)) %>%
  as.data.table() %>%
  inner_join(F_Team_Avail_Pitch_RP, by = c("Name" = "Player"))

# good Perf

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SV,0.75)< SV & quantile(roll_rate_SV,0.50)<roll_rate_SV) %>%
  arrange(desc(SV)) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SV,0.75)< SV & quantile(roll_rate_SV,0.50)<roll_rate_SV) %>%
  arrange(desc(SV)) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))

# Poor Perf
datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SV,0.75)> SV & quantile(roll_rate_SV,0.50)>roll_rate_SV) %>%
  arrange(desc(SV)) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(SV,0.75)> SV & quantile(roll_rate_SV,0.50)>roll_rate_SV) %>%
  arrange(desc(SV)) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))




datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(IP,0.75)< IP & quantile(roll_rate_IP,0.50)<roll_rate_IP) %>%
  arrange(desc(IP)) %>%
  as.data.table()

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(R,0.75)< R & quantile(roll_rate_R,0.50)<roll_rate_R) %>%
  arrange(desc(R)) %>%
  as.data.table()

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(H,0.75)< H & quantile(roll_rate_H,0.50)<roll_rate_H) %>%
  arrange(desc(H)) %>%
  as.data.table()

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(ER,0.75)< ER & quantile(roll_rate_ER,0.50)<roll_rate_ER) %>%
  arrange(desc(ER)) %>%
  as.data.table()

# inner join ERA

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(ERA,0.5) > ERA & IP > quantile(IP,0.75) ) %>%
  arrange(ERA) %>%
  as.data.table() %>%
  inner_join(B_Team_Avail_Pitch, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(ERA,0.5) > ERA & IP > quantile(IP,0.75) ) %>%
  arrange(ERA) %>%
  as.data.table() %>%
  inner_join(F_Team_Avail_Pitch, by = c("Name" = "Player"))

# Good Perf
datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(ERA,0.5) > ERA & IP > quantile(IP,0.75) ) %>%
  arrange(ERA) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(ERA,0.5) > ERA & IP > quantile(IP,0.75) ) %>%
  arrange(ERA) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))
# Poor Perf

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(ERA,0.5) < ERA & IP > quantile(IP,0.75) ) %>%
  arrange(ERA) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(ERA,0.5) < ERA & IP > quantile(IP,0.75) ) %>%
  arrange(ERA) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))







# inner join WHIP
datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(WHIP,0.5) > WHIP & IP > quantile(IP,0.75) ) %>%
  arrange(WHIP) %>%
  as.data.table() %>%
  inner_join(B_Team_Avail_Pitch, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(WHIP,0.5) > WHIP & IP > quantile(IP,0.75) ) %>%
  arrange(WHIP) %>%
  as.data.table() %>%
  inner_join(F_Team_Avail_Pitch, by = c("Name" = "Player"))

# Good Perf
datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(WHIP,0.5) > WHIP & IP > quantile(IP,0.75) ) %>%
  arrange(WHIP) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(WHIP,0.5) > WHIP & IP > quantile(IP,0.75) ) %>%
  arrange(WHIP) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))

#Poor Perf
datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(WHIP,0.5) < WHIP & IP > quantile(IP,0.75) ) %>%
  arrange(WHIP) %>%
  as.data.table() %>%
  inner_join(B_Team_Rost, by = c("Name" = "Player"))

datPitchDT %>%
  filter(Date == max(Date)) %>%
  #group_by(Date) %>%
  filter(quantile(WHIP,0.5) < WHIP & IP > quantile(IP,0.75) ) %>%
  arrange(WHIP) %>%
  as.data.table() %>%
  inner_join(F_Team_Rost, by = c("Name" = "Player"))
