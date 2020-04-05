### from the mlbgameday package for pulling 2019 data.
# column names do not match data prior to 2019.
#
# run the dataPullMlb.R program for the actual data scrap.


library(doParallel)

validate_gids <- function(gidslist=NULL, league="mlb", ...) {
  
  gidslist = newdates
  # Rename league because it conflicts with some downloaded xml values.
  lg <- league
  # Create an emply list to hold the results of the loop.
  gidslist_dt=i=str_length=NULL
  root <- "http://gd2.mlb.com/components/game/"
  
  # Get start and end dates from gids.
  gidslist_dt <- as.data.frame(gidslist)
  gidslist_dt <- rename(gidslist_dt, gid = gidslist)
  gidslist_dt$link <- paste0(root, league, "/" ,gidslist_dt$gid)
  
  # Use miniscorboard to validate urls. Set class to mini so we can use the payload function.
  minilist <- gidslist_dt$link %>% purrr::map_chr(~ paste0(., "/miniscoreboard.xml"))
  
  # Do a tryCatch along the dates. If the url exists, get the payload from miniscorboard.
  out <- foreach::foreach(i = seq_along(minilist), .inorder=FALSE) %dopar% {
    file <- tryCatch(xml2::read_xml(minilist[[i]]), error=function(e) NULL)
    if(!is.null(file)){
      mini_nodes <- xml2::xml_find_all(file, "./game")
      mini <- purrr::map_dfr(mini_nodes, function(x) {
        out <- data.frame(t(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
        out
      })
    }
  }
  str_sub(games$calendar_event_id, 19, 20)
  
  
  
  games <- dplyr::bind_rows(out)
 ####################################################################### 
  gidz <- games %>%
    dplyr::mutate(url = paste0(root, lg, "/", "year_", str_sub(gameday_link, 1, 4), "/", "month_",
                               str_sub(gameday_link, 6, 7), "/", "day_", str_sub(gameday_link, 9, 10), 
                               "/gid_", gameday_link)) %>% 
    select(url)
  
  
  gidz <- games %>%
    dplyr::mutate(url = paste0(root, lg, "/", "year_", str_sub(calendar_event_id, 11, 14), "/", "month_",
                               str_sub(calendar_event_id, 16, 17), "/", "day_", str_sub(calendar_event_id, 19, 20), 
                               "/gid_",calendar_event_id,"/inning/inning_all.xml")) %>% 
    select(url)
  
  gidzz <- gidz %>%
    write.csv(file = 'game_id.csv')
  # Needs to be a list so payload will read it correct
  gidz <- gidz$url %>% as.list
  
  return(gidz)
}

gids <- make_gids(start = "2016-06-01", end = "2016-06-01", dataset="inning_all")
urls <- game_urls(gids)

games$id

mygids <- search_gids(team = "indians", start = "2016-05-01", end = "2016-05-01")

write.csv(games,file='games.csv')

# Create urls based on game_ids
game_urls <- function(url_gids=NULL, dataset = NULL, ...) {
  if(is.null(dataset)) dataset <- "inning_all"
  # Make sure input is a list, so the payload function can read the output correctly.
  if(length(dataset) > 1) stop("Please specify a single data set. Due to conflicting table names, scrapes are limieted to a single set.")
  
  if(dataset=="bis_boxscore") glist <- url_gids %>% purrr::map_chr(~ paste0(., "/bis_boxscore.xml"))
  
  if(dataset=="game") glist <- url_gids %>% purrr::map_chr(~ paste0(., "/game.xml"))
  
  if(dataset=="game_events") glist <- url_gids %>% purrr::map_chr(~ paste0(., "/game_events.xml"))
  
  if(dataset=="inning_all") glist <- url_gids %>% purrr::map_chr(~ paste0(., "/inning/inning_all.xml"))
  
  if(dataset=="inning_hit") glist <- url_gids %>% purrr::map_chr(~ paste0(., "/inning/inning_hit.xml"))
  
  if(dataset=="linescore") glist <- url_gids %>% purrr::map_chr(~ paste0(., "/linescore.xml"))
  
  if(tolower(dataset)=="openwar") glist <- url_gids
  
  return(glist)
}


transform_pload <- function(payload_obj, ...) UseMethod("transform_pload", payload_obj)

#' @rdname transform_pload
#' @importFrom dplyr mutate select
#' @method transform_pload list_bis_boxscore
#' @export

transform_pload.list_bis_boxscore <- function(payload_obj, ...) {
  payload_obj$batting %<>% dplyr::mutate(id=as.numeric(id), bo=as.numeric(bo), ab=as.numeric(ab), po=as.numeric(po),
                                         r=as.numeric(r), a=as.numeric(a), bb=as.numeric(bb), sac=as.numeric(sac),
                                         t=as.numeric(t), sf=as.numeric(sf), h=as.numeric(h), e=as.numeric(e),
                                         d=as.numeric(d), hbp=as.numeric(hbp), so=as.numeric(so), hr=as.numeric(hr),
                                         rbi=as.numeric(rbi), lob=as.numeric(lob), fldg=as.double(fldg), avg=as.double(avg),
                                         go=as.numeric(go), ao=as.numeric(ao), gidp=as.numeric(gidp))
  
  payload_obj$pitching %<>% dplyr::mutate(id=as.numeric(id), out=as.numeric(out), bf=as.numeric(bf), er=as.numeric(er),
                                          r=as.numeric(r), h=as.numeric(h), so=as.numeric(so), hr=as.numeric(hr),
                                          bb=as.numeric(bb), np=as.numeric(np), s=as.numeric(s), w=as.numeric(w),
                                          era=as.double(era))
  
  return(payload_obj)
}

#' @rdname transform_pload
#' @importFrom dplyr mutate
#' @method transform_pload df_game_events
#' @export

transform_pload.df_game_events <- function(payload_obj, ...) {
  # Hack to get dplyr to mutate a generic object class.
  payload_obj <- structure(payload_obj, class="data.frame")
  
  payload_obj %<>% dplyr::mutate(start_speed=as.double(start_speed), num=as.numeric(num),
                                 b=as.numeric(b), s=as.numeric(s), o=as.numeric(o), start_tfs=as.numeric(start_tfs),
                                 batter=as.numeric(batter), pitcher=as.numeric(pitcher), event_num=as.numeric(event_num),
                                 home_team_runs=as.numeric(home_team_runs), away_team_runs=as.numeric(away_team_runs),
                                 inning=as.numeric(inning))
  # Revert object to the old class for the load functions.
  return(payload_obj)
}

#' @rdname transform_pload
#' @importFrom dplyr mutate
#' @method transform_pload df_inning_hit
#' @export

transform_pload.df_inning_hit <- function(payload_obj, ...) {
  # Hack to get dplyr to mutate a generic object class.
  payload_obj <- structure(payload_obj, class="data.frame")
  payload_obj %<>% dplyr::mutate(x=as.double(x), y=as.double(y), batter=as.numeric(batter), pitcher=as.numeric(pitcher),
                                 inning=as.numeric(inning))
  
  return(payload_obj)
}


#' @rdname transform_pload
#' @importFrom dplyr mutate rename
#' @method transform_pload list_inning_all
#' @export

transform_pload.list_inning_all <- function(payload_obj, ...) {
  payload_obj$atbat %<>%
    # Data prior to 2015 is missing several fields. Add those as null so the database is consistant.
    dplyr::mutate(play_guid = if (exists('play_guid', where = payload_obj$atbat)) play_guid else NA,
                  event2 = if (exists('event2', where = payload_obj$atbat)) event2 else NA,
                  event2_es = if (exists('event2_es', where = payload_obj$atbat)) event2_es else NA,
                  event3 = if (exists('event3', where = payload_obj$atbat)) event3 else NA,
                  event4 = if (exists('event4', where = payload_obj$atbat)) event4 else NA,
                  end_tfs_zulu = if (exists('end_tfs_zulu', where = payload_obj$atbat)) end_tfs_zulu else NA,
                  score = if(exists('score',  where = payload_obj$atbat)) score else NA,
                  des_es = if (exists('des_es', where = payload_obj$atbat)) des_es else NA) %>%
    
    dplyr::mutate(num=as.numeric(num), b=as.numeric(b), s=as.numeric(s), o=as.numeric(o),
                  batter=as.numeric(batter), pitcher=as.numeric(pitcher), date=as.factor(date)) %>%
    # Rename a couple columns to fit with the pitchRx schema.
    dplyr::rename(atbat_des = des, atbat_des_es = des_es) %>%
    
    dplyr::select(pitcher, batter, num, b, s, o, start_tfs, start_tfs_zulu, stand, b_height, p_throws, atbat_des, 
                  atbat_des_es, event, home_team_runs, away_team_runs, url, inning_side, inning, next_, event2, event3,
                  batter_name, pitcher_name, event4, gameday_link, date, end_tfs_zulu, event_num, event_es, play_guid, event2_es)
  
  payload_obj$action %<>% 
    # Add columns that may not exist.
    dplyr::mutate(play_guid = if (exists('play_guid', where = payload_obj$action)) play_guid else NA,
                  event2 = if (exists('event2', where = payload_obj$action)) event2 else NA,
                  event2_es = if (exists('event2_es', where = payload_obj$action)) event2_es else NA,
                  des_es = if (exists('des_es', where = payload_obj$action)) des_es else NA,
                  score = if (exists('score', where = payload_obj$action)) score else NA) %>%
    
    dplyr::mutate(b=as.numeric(b), s=as.numeric(s), o=as.numeric(o), player=as.numeric(player), pitch=as.numeric(pitch),
                  num=as.character(num)) %>%
    
    dplyr::select(b, s, o, des, des_es, event, tfs, tfs_zulu, player, pitch, url, inning_side, inning, next_, num, score,
                  home_team_runs, away_team_runs, event2, gameday_link, event_es, event_num, play_guid, event2_es)
  
  payload_obj$pitch %<>%
    # Add columns that may not exist.
    dplyr::mutate(play_guid = if (exists('play_guid', where = payload_obj$pitch)) play_guid else NA,
                  des_es = if (exists('des_es', where = payload_obj$pitch)) des_es else NA,
                  event2 = if (exists('event2', where = payload_obj$pitch)) event2 else NA,
                  event2_es = if (exists('event2_es', where = payload_obj$pitch)) event2_es else NA,
                  on_1b = if (exists('on_1b', where = payload_obj$pitch)) on_1b else NA,
                  on_2b = if (exists('on_2b', where = payload_obj$pitch)) on_2b else NA,
                  on_3b = if (exists('on_3b', where = payload_obj$pitch)) on_3b else NA,
                  code = if (exists('code', where = payload_obj$pitch)) code else NA,
                  # tfs and tfs_zulu columns may be blank for older data sets. If blank, set them to NA.
                  tfs = ifelse(tfs == "", NA, tfs), tfs_zulu = ifelse(tfs_zulu == "", NA, tfs_zulu),
                  # Same with x and y for 2019 season and beyond.
                  x = ifelse(x == "None", NA, x), y = ifelse(y == "None", NA, tfs_zulu),
                  # Starting in 2019 zone and type_confidence sometimes comes through as 'placeholder'
                  zone = ifelse(zone == "placeholder", NA, zone), type_confidence = ifelse(type_confidence == "placeholder", NA, type_confidence),
                  # Same with spin_dir and spin_rate
                  spin_dir = ifelse(spin_dir == "placeholder", NA, spin_dir), spin_rate = ifelse(spin_rate == "placeholder", NA, spin_rate),
                  
                  # Some spring training and minor league games may be missing speed data.
                  start_speed = ifelse(start_speed == "", NA, as.numeric(start_speed))) %>%
    
    dplyr::mutate(id=as.numeric(id), x=as.numeric(x), 
                  end_speed=as.numeric(end_speed), sz_top=as.numeric(sz_top), sz_bot=as.numeric(sz_bot),
                  pfx_x=as.numeric(pfx_x), pfx_z=as.numeric(pfx_z), px=as.numeric(px), pz=as.numeric(pz),
                  x0=as.numeric(x0), y0=as.numeric(y0), z0=as.numeric(z0), vx0=as.numeric(vx0), 
                  vy0=as.numeric(vy0), vz0=as.numeric(vz0), ax=as.numeric(ax), ay=as.numeric(ay),
                  zone=as.numeric(zone), break_length=as.numeric(break_length), type_confidence=as.numeric(type_confidence),
                  nasty=as.numeric(nasty), spin_dir=as.numeric(spin_dir), spin_rate=as.numeric(spin_rate),
                  on_1b=as.numeric(on_1b), on_2b=as.numeric(on_2b), on_3b=as.numeric(on_3b), count=as.factor(count)) %>%
    
    dplyr::select(des, des_es, id, type, tfs, tfs_zulu, x, y, sv_id, start_speed, end_speed, sz_top, sz_bot, pfx_x, pfx_z,           
                  px, pz, x0, y0, z0, vx0, vy0, vz0, ax, ay, az, break_y, break_angle, break_length, pitch_type, type_confidence, zone,           
                  nasty, spin_dir, spin_rate, cc, mt, url, inning_side, inning, next_, num, on_1b, on_2b, on_3b, count, gameday_link,
                  code, event_num, play_guid)
  
  payload_obj$runner %<>% dplyr::mutate(id=as.numeric(id), num=as.numeric(num)) %>%
    dplyr::select(id, start, end, event, score, rbi, earned, url, inning_side, inning, next_, num, gameday_link, event_num)
  
  payload_obj$po %<>% 
    # Add columns that may not exist.
    dplyr::mutate(play_guid = if (exists('play_guid', where = payload_obj$po)) play_guid else NA,
                  catcher = if (exists('catcher', where = payload_obj$po)) catcher else NA,
                  des_es = if (exists('des_es', where = payload_obj$po)) des_es else NA,
                  event_num = if (exists('event_num', where = payload_obj$po)) event_num else NA) %>%
    
    dplyr::select(des, url, inning_side, inning, next_, num, gameday_link, des_es, event_num, play_guid, catcher)
  
  return(payload_obj)
}

ab_id

#' @rdname transform_pload
#' @importFrom dplyr mutate
#' @method transform_pload list_linescore
#' @export

transform_pload.list_linescore <- function(payload_obj, ...) {
  # TO DO: Need to convert the date/time fields to a 24-hour clock and to a date/time data format.
  # The way they are now are going to mess up any database we try to load them in to.
  
  payload_obj$game %<>% dplyr::mutate(game_pk=as.numeric(game_pk), original_date=as.Date(original_date, format="Y/m/d"),
                                      venue_id=as.numeric(venue_id), scheduled_innings=as.numeric(scheduled_innings),
                                      away_team_id=as.numeric(away_team_id), away_league_id=as.numeric(away_league_id),
                                      home_team_id=as.numeric(home_team_id), home_league_id=as.numeric(home_league_id),
                                      game_nbr=as.numeric(game_nbr), away_win=as.numeric(away_win), away_loss=as.numeric(away_loss),
                                      home_win=as.numeric(home_win), home_loss=as.numeric(home_loss),
                                      inning=as.numeric(inning), balls=as.numeric(balls), strikes=as.numeric(strikes),
                                      outs=as.numeric(outs), away_team_runs=as.numeric(away_team_runs), home_team_runs=as.numeric(home_team_runs),
                                      away_team_hits=as.numeric(away_team_hits), home_team_hits=as.numeric(home_team_hits), 
                                      away_team_errors=as.numeric(away_team_errors), home_team_errors=as.numeric(home_team_errors)) %>%
    
    dplyr::select(ampm, aw_lg_ampm, away_ampm, away_code, away_division, away_file_code,       
                  away_league_id, away_loss, away_name_abbrev, away_preview_link, away_recap_link, away_sport_code,      
                  away_team_city, away_team_errors, away_team_hits, away_team_id, away_team_link, away_team_name,       
                  away_team_runs, away_time, away_time_zone, away_win, balls, day,                  
                  description, double_header_sw, first_pitch_et, game_data_directory, game_nbr, game_pk,              
                  game_type, gameday_link, gameday_sw, highlights_available, hm_lg_ampm, home_ampm,           
                  home_code, home_division, home_file_code, home_league_id, home_loss, home_name_abbrev,     
                  home_preview_link, home_recap_link, home_sport_code, home_team_city, home_team_errors, home_team_hits,       
                  home_team_id, home_team_link, home_team_name, home_team_runs, home_time, home_time_zone,       
                  home_win, id, ind, inning, inning_break_length, inning_state,         
                  is_no_hitter, is_perfect_game, league, location, note, original_date,        
                  outs, pbp_last, preview, reason, runner_on_base_status, scheduled_innings,    
                  status, strikes, tbd_flag, tiebreaker_sw, time, time_aw_lg,           
                  time_date, time_date_aw_lg, time_date_hm_lg, time_hm_lg, time_zone, time_zone_aw_lg,      
                  time_zone_hm_lg, top_inning, tz_aw_lg_gen, tz_hm_lg_gen, venue, venue_id,             
                  venue_w_chan_loc, wrapup_link, xmlns.xs, date, runner_on_2b, runner_on_3b, runner_on_1b)
  
  return(payload_obj)
}


#' @rdname transform_pload
#' @importFrom dplyr mutate
#' @method transform_pload df_game
#' @export

transform_pload.df_game <- function(payload_obj, ...) {
  payload_obj <- structure(payload_obj, class="data.frame")
  
  payload_obj %<>% dplyr::mutate(id=as.numeric(id), w=as.numeric(w), l=as.numeric(l), league_id=as.numeric(league_id),
                                 game_pk=as.numeric(game_pk))
  
  return(payload_obj)
}

make_gids <- function(start=NULL, end=NULL, league="mlb", dataset=NULL, game_ids=NULL, ...) {
  
  start = '2019-04-01'
  end = '2019-10-31'
  league="mlb"
  dataset="innings_all"
  game_ids=NULL
  
  
  root <- paste0("http://gd2.mlb.com/components/game/", league, "/")
  
  if(!is.null(game_ids)){
    game_ids <- paste0(root, "year_", stringr::str_sub(game_ids, 5, 8), "/month_", stringr::str_sub(game_ids, 10, 11), 
                       "/day_", stringr::str_sub(game_ids, 13, 14), "/", game_ids)
    
    made_gids <- game_urls(game_ids, dataset = dataset)
  }
  
  if(!is.null(start) & !is.null(end)){
    if(as.Date(start) < as.Date("2008-02-26")) {
      warning("The mlbgameday package supports data beginning on '2008-03-26'. Please enter a valid start date")
    }
    #Format dates
    dateslist <- seq(as.Date(start), as.Date(end), by = "day")
    dates <- paste0("year_", format(dateslist, "%Y"), "/month_",
                    format(dateslist, "%m"), "/day_", format(dateslist, "%d"))
    
    # Check to see if gids within the start and end dates are in the internal dataset. If not, grab them.
    gidenv <- environment()
    data(game_ids, package = "mlbgameday", envir = gidenv)
    
    # Add a date column to gid data to make life easier.
    gid_dates <- dplyr::rename(game_ids, gid = gameday_link)
    last_date <- as.Date(tail(gid_dates$date_dt, 1))
    first_date <- as.Date(head(gid_dates$date_dt, 1))
    
    # If we've got the whole range of gids internally, just grab them and format.
    if(start >= first_date & end <= last_date){
      final_gids <- dplyr::filter(gid_dates, date_dt >= as.Date(start) & date_dt <= as.Date(end))
      final_gids$url <- paste0(root, "year_", stringr::str_sub(final_gids$date_dt, 1, 4), "/month_",
                               stringr::str_sub(final_gids$date_dt, 6,7), "/day_", 
                               stringr::str_sub(final_gids$date_dt, 9, 10),
                               "/", final_gids$gid)
      final_gids <- final_gids$url %>% as.list()
    }
    
    # If we have no internal gids, the start date is greater than the last date in the internal data.
    if(start > last_date){
      # Find gap between the last_date in the gids and the date the user input.
      newgidz <- seq(as.Date(start), as.Date(end), by = "day")
      newdates <- paste0("year_", format(newgidz, "%Y"), "/month_",
                         format(newgidz, "%m"), "/day_", format(newgidz, "%d"))
      
      
      # Scrape the miniscoreboard for that day so we can extract game_id.
      final_gids <- validate_gids(newdates)
    }
    
    # If we have some at the start internally, but are missing end, grab the gids we have and format and grab anything missing.
    if(start < last_date & end > last_date){
      # Find gap between the last_date in the gids and the date the user input.
      gaplist <- seq(as.Date(start), as.Date(end), by = "day")
      gapdates <- paste0("year_", format(gaplist, "%Y"), "/month_",
                         format(gaplist, "%m"), "/day_", format(gaplist, "%d"))
      
      # Veryify those gids were games played. If played, scrape the miniscoreboard for that day so we can extract game_id.
      # This piece takes a while. It has to tryCatch every url.
      gapgids <- validate_gids(gapdates)
      
      # Get the other gids not in the end window.
      startgids <- filter(gid_dates, date_dt >= as.Date(start) & date_dt <= as.Date(last_date)) %>%
        mutate(gid = as.character(gid), date_dt = as.Date(date_dt))
      
      startgids$url <- paste0(root, league, "/", "year", str_sub(startgids$gid, 4, 8), "/", "month_",
                              str_sub(startgids$gid, 10, 11), "/", "day_", str_sub(startgids$gid, 13, 14), 
                              "/", startgids$gid)
      
      startgids <- select(startgids, url)
      
      final_gids <- c(startgids$gid, gapgids)
    }
    
    made_gids <- game_urls(final_gids, dataset = dataset)
  }
  
  return(made_gids)
}

payload.gd_inning_all <- function(urlz, ...) {
  # Make some place-holders for the function.
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
  
  return(innings_df)
}

#' An internal function for inning_hit payload.
#' @param urlz An urlzect created from a urlz link
#' @param ... additional arguments
#' @keywords internal
#' @import xml2
#' @importFrom stringr str_sub str_replace_all
#' @importFrom purrr map_dfr
#' @importFrom stats setNames
#' @import foreach
#' @export
#' 
#' 

get_payload <- function(start=NULL, end=NULL, league="mlb", dataset = NULL, game_ids = NULL, db_con = NULL, overwrite = FALSE, ...) {
  if(is.null(dataset)) dataset <- "inning_all"
  message("Gathering Gameday data, please be patient...")
  
  if(dataset=="bis_boxscore" && as.Date(end) >= '2019-01-01'){
    stop("bis_boxscore dataset is only available prior to the 2019 season. Please select a different data set.")
  } 
  
  if(!is.null(game_ids)){
    urlz <- make_gids(game_ids = game_ids, dataset = dataset)
  }
  
  if(!is.null(start) & !is.null(end)){
    if(start < as.Date("2008-01-01")){
      stop("Please select a later start date. The data are not dependable prior to 2008.")
    }
    if(end >= Sys.Date()) stop("Please select an earlier end date.")
    
    if(start > end) stop("Your start date appears to occur after your end date.")
    start <- as.Date(as.character(start)); end <- as.Date(end); league <- tolower(league)
    # Get gids via internal function.
    urlz <- make_gids(start = start, end = end, dataset = dataset)
  }
  
  if(!is.null(db_con)){
    # Chunk out URLs in groups of 300 if a database connection is available.
    url_chunks <- split(urlz, ceiling(seq_along(urlz)/500))
    innings_df=NULL
    
    for(i in seq_along(url_chunks)){
      message(paste0("Processing data chunk ", i, " of ", length(url_chunks)))
      urlz <- unlist(url_chunks[i])
      # inning_all and linescore contain multiple tables, so those need to be written in a loop.
      if(dataset == "inning_all" | dataset=="linescore"){
        if(dataset == "inning_all") innings_df <- payload.gd_inning_all(urlz)
        if(dataset=="linescore") innings_df <- payload.gd_linescore(urlz)
        
        if(isTRUE(overwrite)){
          for (i in names(innings_df)) DBI::dbWriteTable(conn = db_con, value = innings_df[[i]], name = i, overwrite = TRUE)
        }
        if(!isTRUE(overwrite)){
          for (i in names(innings_df)) DBI::dbWriteTable(conn = db_con, value = innings_df[[i]], name = i, append = TRUE)
        }
        
      } else {
        if(dataset=="inning_hit"){
          innings_df <- payload.gd_inning_hit(urlz)
          if(isTRUE(overwrite)) DBI::dbWriteTable(conn = db_con, value = innings_df, name = "inning_hit", overwrite = TRUE)
          if(isTRUE(overwrite)) DBI::dbWriteTable(conn = db_con, value = innings_df, name = "inning_hit", append = TRUE)
        }
        if(dataset=="game_events"){
          innings_df <- payload.gd_inning_hit(urlz)
          if(isTRUE(overwrite)) DBI::dbWriteTable(conn = db_con, value = innings_df, name = "game_events", overwrite = TRUE)
          if(isTRUE(overwrite)) DBI::dbWriteTable(conn = db_con, value = innings_df, name = "game_events", append = TRUE)                    
        }
        if(dataset=="game"){
          innings_df <- payload.gd_inning_hit(urlz)
          if(isTRUE(overwrite)) DBI::dbWriteTable(conn = db_con, value = innings_df, name = "game", overwrite = TRUE)
          if(isTRUE(overwrite)) DBI::dbWriteTable(conn = db_con, value = innings_df, name = "game", append = TRUE)                         
        }
        if(dataset=="bis_boxscore"){
          innings_df <- payload.gd_inning_hit(urlz)
          if(isTRUE(overwrite)) DBI::dbWriteTable(conn = db_con, value = innings_df, name = "bis_boxscore", overwrite = TRUE)
          if(isTRUE(overwrite)) DBI::dbWriteTable(conn = db_con, value = innings_df, name = "bis_boxscore", append = TRUE)  
        } 
      }
      
      # Manual garbage collect after every loop of 500 games.
      rm(innings_df); gc()
    }
    
    DBI::dbDisconnect(db_con)
    message(paste0("Transaction complete, disconnecting from the database.", " ", Sys.time()))
  }
  
  if(is.null(db_con)){
    # If no database connection, just return a dataframe.
    # If the returned dataframe looks like it's going to be large, warn the user.
    if(length(urlz) > 3500) { # One full season including spring training and playoffs is around 3000 games.
      message("Woah, that's a lot of data! Think about using a Database Connection")
    }
    message("Starting download, this may take a while...")
    if(dataset == "bis_boxscore") innings_df <- payload.gd_bis_boxscore(urlz)
    if(dataset == "game_events") innings_df <- payload.gd_game_events(urlz)
    if(dataset == "inning_all") innings_df <- payload.gd_inning_all(urlz)
    if(dataset=="inning_hit") innings_df <- payload.gd_inning_hit(urlz)
    if(dataset=="linescore") innings_df <- payload.gd_linescore(urlz)
    if(dataset=="game") innings_df <- payload.gd_game(urlz)
    # Probably faster to do the transformation within the loop in cases where data gets very large.
    #innings_df <- transform_pload(innings_df)
    
    return(innings_df)
  }
}

make_gids(start = '2019-04-28', end = '2019-04-28', league = "mld", dataset = "inning_all")

gameIds<-make_gids(start = '2018-04-10', end = '2018-04-11', league = "mld", dataset = "inning_all")

df <- get_payload(start = '2018-04-01', end = '2018-04-11',dataset = "inning_all")
library(tidyverse)
library(mlbgameday)
