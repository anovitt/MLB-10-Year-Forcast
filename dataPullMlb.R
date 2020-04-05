library(data.table)
library(tidyverse)
#library(Lahman)
library(lubridate)
library(broom)
library(mlbgameday)
library(XML2R)
library(doParallel)
library(foreach)


##########################################################
#  File takes and mlbgameday package and does a "manual" data pull for 2019, due to errors in the mlbgameday R package

games <- read.csv(file='games.csv')
league <- "mlb"
database <- "innings_all"

head(games$id)

root <- paste0("http://gd2.mlb.com/components/game/", league, "/")

mygids <- 
games %>% 
  select(id) %>%
  mutate(id = paste0("gid_",id)) %>%
  mutate(id = gsub("-","_",id)) %>%
  mutate(id = gsub("/","_",id)) %>%
  mutate(idx = paste0(root, "year_", stringr::str_sub(id, 5, 8), "/month_", stringr::str_sub(id, 10, 11), 
                      "/day_", stringr::str_sub(id, 13, 14), "/", id)) %>%
  select(id = idx)
  

game_ids <- mygids$id %>% as.list  #ID's to feed into get.payload

class(game_ids)

made_gids <- game_ids %>% purrr::map_chr(~ paste0(., "/inning/inning_all.xml"))
urlz <- as.list(made_gids)

atbat <- list(); action <- list(); pitch <- list(); runner <- list(); po <- list()
lnames <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)

out <- foreach::foreach(i = seq_along(urlz), .combine="comb_pload", .multicombine=T, .inorder=TRUE,
                        .final = function(x) stats::setNames(x, names(lnames)),
                        .init=list(list(), list(), list(), list(), list())) %dopar% {
                          file <- tryCatch(xml2::read_xml(urlz[[i]][[1]], n=256), error=function(e) NULL)
                          if(!isTRUE(is.null(file))){
                            atbat_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat"), 
                                             xml2::xml_find_all(file, "./inning/bottom/atbat"))
                            
                            action_nodes <- c(xml2::xml_find_all(file, "./inning/top/action"), 
                                              xml2::xml_find_all(file, "./inning/bottom/action"))
                            
                            pitch_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/pitch"),
                                             xml2::xml_find_all(file, "./inning/bottom/atbat/pitch"))
                            
                            runner_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/runner"), 
                                              xml2::xml_find_all(file, "./inning/bottom/atbat/runner"))
                            
                            po_nodes <- c(xml2::xml_find_all(file, "./inning/top/atbat/po"), 
                                          xml2::xml_find_all(file, "./inning/bottom/atbat/po"))
                            
                            url <- urlz[[i]]
                            
                            date_dt <- stringr::str_sub(urlz[[i]], 70, 81) %>% stringr::str_replace_all("_", "-") %>%
                              as.Date(format = "%Y-%m-%d")
                            gameday_link <- stringr::str_sub(urlz[[i]], 66, -23)
                            
                            list(                        
                              atbat <- purrr::map_dfr(atbat_nodes, function(x) {
                                out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num"))
                                out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next"))
                                out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(x)))
                                out$url <- url
                                out$date <- date_dt
                                out$gameday_link <- gameday_link
                                out
                              }),
                              
                              action <- purrr::map_dfr(action_nodes, function(x) {
                                out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("num"))
                                out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(x)) %>% xml2::xml_attr("next"))
                                #out$num <- as.numeric(xml2::xml_parent(x) %>% xml2::xml_attr("num"))
                                out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(x)))
                                out$url <- url
                                out$gameday_link <- gameday_link
                                out
                              }),
                              
                              pitch <- purrr::map_dfr(pitch_nodes, function(x) {
                                out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num"))
                                out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next"))
                                out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x))))
                                out$url <- url
                                out$gameday_link <- gameday_link
                                out$num <- as.numeric(xml2::xml_parent(x) %>% xml2::xml_attr("num"))
                                out
                              }),
                              
                              runner <- purrr::map_dfr(runner_nodes, function(x) {
                                out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                out$inning <- as.numeric(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num"))
                                out$next_ <- as.character(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next"))
                                out$num <- as.character(xml2::xml_parent(x) %>% xml2::xml_attr("num"))
                                out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x))))
                                out$url <- url
                                out$gameday_link <- gameday_link
                                out
                              }),
                              
                              po <- purrr::map_dfr(po_nodes, function(x) {
                                out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
                                out$inning <- as.numeric( xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("num"))
                                out$next_ <-  as.character(xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(x))) %>% xml2::xml_attr("next"))
                                out$inning_side <- as.character(xml2::xml_name(xml2::xml_parent(xml2::xml_parent(x))))
                                out$num <- as.numeric(xml2::xml_parent(x) %>% xml2::xml_attr("num"))
                                out$url <- url
                                out$gameday_link <- gameday_link
                                out
                              })
                            )
                          }
                        }

# The foreach loop returns a named list of nested data frames. We need to bind the dfs under 
# each name and pack the binded dfs back into a list that can be returned.
atbat <- dplyr::bind_rows(out$atbat)
action <- dplyr::bind_rows(out$action)
pitch <- dplyr::bind_rows(out$pitch)
runner <- dplyr::bind_rows(out$runner)
po <- dplyr::bind_rows(out$po)

write.csv(atbat , file = 'atbat.csv')
write.csv(action , file = 'action.csv')
write.csv(pitch , file = 'pitch.csv')
write.csv(runner , file = 'runner.csv')
write.csv(po , file = 'po.csv')

atbat <- dplyr::bind_rows(out$atbat)
action <- dplyr::bind_rows(out$action)
pitch <- dplyr::bind_rows(out$pitch)
runner <- dplyr::bind_rows(out$runner)
po <- dplyr::bind_rows(out$po)

# Make of game timeline of atbat and action so we know which atbat to assign an action to.
acts <- action %>% dplyr::select(tfs_zulu, inning, inning_side, des)
bats <- atbat %>% dplyr::select(start_tfs_zulu, num, inning, inning_side) %>% dplyr::rename(tfs_zulu = start_tfs_zulu)
events <- dplyr::bind_rows(acts, bats) %>%
  dplyr::arrange(tfs_zulu) %>% dplyr::mutate(num = as.numeric(num)) %>%
  tidyr::fill(num, .direction = "up") %>% na.omit()

action <- dplyr::left_join(action, events, by = c("tfs_zulu", "inning", "inning_side", "des"))

# Calculate the pitch count for the pitching table.
pitch <- pitch_count(dat=pitch)

innings_df <- list(atbat=atbat, action=action, pitch=pitch, runner=runner, po=po)
# Add batter and pitcher names to the atbat data frame
player.env <- environment()
data(player_ids, package="mlbgameday", envir=player.env)
player_ids$id <- as.character(player_ids$id)

innings_df$atbat %<>% dplyr::left_join(player_ids, by = c("batter" = "id")) %>% 
  dplyr::left_join(player_ids, by = c("pitcher" = "id")) %>% 
  dplyr::rename(batter_name=full_name.x, pitcher_name=full_name.y)

innings_df <- structure(innings_df, class="list_inning_all") %>%
  transform_pload()
innings_df_2019 <- innings_df

pitchData2019 <- innings_df_2019$pitch
write.csv(pitchData2019, file = 'pitchData2019.csv',row.names = FALSE)

atbatData2019 <- innings_df_2019$atbat
write.csv(atbatData2019, file = 'atbatData2019.csv',row.names = FALSE)

actionData2019 <- innings_df_2019$action
write.csv(actionData2019, file = 'actionData2019.csv',row.names = FALSE)

runnerData2019 <- innings_df_2019$runner
write.csv(runnerData2019, file = 'runnerData2019.csv',row.names = FALSE)

poData2019 <- innings_df_2019$po
write.csv(poData2019, file = 'poData2019.csv',row.names = FALSE)
