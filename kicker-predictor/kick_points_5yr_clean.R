#fantasy kickers
#what we'l need to predict the model:
# current task: split traaining/testing for svm to be like xgboost
# check wind NAs, weather NAs -- how to fill games without weather listed

#status: bring in red zone again?
#random questions: how does 4th down vars list off penalties

library(tidyverse)
library(nflverse)
library(fastDummies)
library(grf)
library(ggplot2)
library(yardstick)
library(magrittr)
library(gtsummary)
library(gt)

### ORIGINAL Play-by-play DATA RETRIEVAL from nflverse 

#data <- load_pbp(2020:2023)
#data_2019 <- load_pbp(2019)
#data_2017_2018 <- load_pbp(2017:2018)

# retrieved extra data in some missing roof status for retractables by looking at NFL game logs 
#(e.g. https://static.www.nfl.com/gamecenter/ba0fb3e6-d24c-11ec-b23d-d15a91047884.pdf)
# or Pro Football Reference
# (e.g. https://www.pro-football-reference.com/boxscores/202212040cin.htm)
# @ home: stored in "datasources"

retractroof_2021 <- read_csv("retractroof_2021.csv")

#env above saved as Rdata Workspaace
load("~/kickerspace.RData")

data %<>%
  bind_rows(data_2019) %>%
  arrange(posteam,season,week)

data %<>%
  bind_rows(data_2017_2018) %>% 
  arrange(posteam,season,week)

#SEE IF works on WHOLE DATASET
data %<>%
  bind_rows(data_test) %>% 
  arrange(posteam,season,week)

data %<>%
  left_join(retractroof_2021,by="game_id") %>% 
  mutate(roof = ifelse(is.na(roof) & season==2021,roof_update,roof))

# EXTRACT Weather data to pull precipitation and wind speed and do some extra cleaning
#ws_pattern <-  '(?<=Wind: [A-Za-z,\\\\]{0,20}\\ )[0-9]{0,2}'
ws_pattern <- "(?<=Wind:\\D{0,30})\\d{1,2}"
#ws_pattern <-  '(?<=Wind: [\\d]{0,20}\\ )[0-9]{0,2}'

#weather_pattern <- '^[A-Za-z\\s]+(?= Temp)'
weather_pattern <- '^[:print:]+(?= Temp)'

#trying to add some
data %<>% 
  mutate(wind_weather = str_extract(weather,ws_pattern),
         weather_desc = str_extract(weather,weather_pattern)) %>% 
  #fix calm days to have zero wind
  mutate(wind_weather = ifelse(str_detect(weather,"Calm"),"0",wind_weather)) %>% 
  mutate(weather_desc = ifelse(str_detect(weather_desc,"(N|n)/(A|a)")==TRUE,NA,weather_desc)) %>% 
  mutate(precipitation = case_when(str_detect(weather_desc,"(C|c)hance|(C|c)hange|Threat|Expected|likely|Forecasted")==TRUE & 
                                     str_detect(weather_desc,"Raining")==FALSE ~ NA,
                                    str_detect(weather_desc,"(R|r)ain|(S|s)now|(S|s)hower|(D|d)rizzle|storms")==TRUE ~ "yes",
                                   is.na(weather_desc) ~ NA,
                                   TRUE ~ "no")) %>% 
  mutate(precipitation = ifelse(roof %in% c("dome","closed") & is.na(precipitation),"no",precipitation),
         wind_weather = ifelse(roof %in% c("dome","closed") & is.na(wind_weather),0,wind_weather)) %>%
  #fix stray games with missing wind_weather
  mutate(precipitation = ifelse(game_id=="2022_21_CIN_KC","no",precipitation),
         wind_weather = ifelse(game_id=="2022_21_CIN_KC",13,wind_weather),
         wind_weather = ifelse(game_id=="2021_13_TB_ATL",0,wind_weather),
         roof = ifelse(game_id=="2021_13_TB_ATL","closed",roof),
         precipitation = ifelse(game_id=="2023_02_NYJ_DAL","no",precipitation),
         wind_weather = ifelse(game_id=="2023_02_NYJ_DAL",0,wind_weather),
         roof = ifelse(game_id=="2023_02_NYJ_DAL","closed",roof),
         precipitation = ifelse(game_id=="2023_03_DAL_ARI","no",precipitation),
         wind_weather = ifelse(game_id=="2023_03_DAL_ARI",0,wind_weather),
         roof = ifelse(game_id=="2023_03_DAL_ARI","closed",roof),
         wind_weather = ifelse(game_id=="2018_06_TB_ATL",5,wind_weather),
         wind_weather = ifelse(game_id=="2018_11_KC_LA",0,wind_weather),
         wind_weather = ifelse(game_id=="2019_11_CHI_LA",0,wind_weather),
         wind_weather = ifelse(game_id=="2017_10_HOU_LA",0,wind_weather),
         wind_weather = ifelse(game_id=="2017_14_PHI_LA",0,wind_weather)) %>% 
  mutate(wind_weather = ifelse(is.na(wind_weather) & home_team=="ATL",4.36,wind_weather)) %>% 
  #for precipitation, there's a fair number missing, so we'll do unk
  mutate(precipitation = ifelse(is.na(precipitation),"unknown",precipitation),
         wind_weather = as.numeric(wind_weather))

ww <- data %>% select(game_id,weather,weather_desc,wind,wind_weather,roof)
#check for precipitation descriptors to edit the data step above
#unique(ww$weather_desc)

#check 
nowind_weather <- ww %>% 
  filter(is.na(wind_weather)) %>% 
  count(game_id,weather,roof)
  #filter(str_detect(weather,"(?<=Wind:\\D{0,30})Calm"))

# precip_words = c("Rainy","Rain","Cloudy with showers","Cloudy with snow flurries",
#                  "Snow","Showers","Snow shower","Light Rain","Mostly Cloudy with 
#                  Light Rain Forecasted","Rain showers")
#precip_string = c("(R|r)ain|(S|s)now|(S|s)howers") 

# START data manipulation
# create full list of each week's teams so that 0 kick weeks aren't dropped later
game_posteams <- data %>% 
  filter(!is.na(posteam)) %>% 
  count(season,game_id,posteam,home_team) %>% 
  select(-n)

# FORK 1: CREATE AVERAGES USING THAT SEASON'S DATA
#create kick-level data and summarize to game_level
kicks <- data %>%
  select(season,game_id,field_goal_result,kick_distance,kicker_player_id,
         kicker_player_name,extra_point_result,home_team,posteam) %>% 
  filter(!is.na(field_goal_result) | !is.na(extra_point_result)) %>%
  mutate(kick_fp = case_when(
            field_goal_result=="made" & kick_distance < 40 ~ 3,
            field_goal_result=="made" & kick_distance < 50 ~ 4,
            field_goal_result=="made" ~ 5,
            field_goal_result %in% c("missed","blocked") ~ -1,
            extra_point_result=="good" ~ 1,
            extra_point_result %in% c("failed","blocked") ~ -1,
            TRUE ~ 999),
         home_kick = ifelse(posteam==home_team,TRUE,FALSE),
         kick_made_distance = ifelse(field_goal_result=="made" | extra_point_result=="good",
                                     kick_distance,NA)
            ) %>% 
  group_by(season,game_id,posteam,home_team) %>%
  summarize(
    total_points = sum(kick_fp),
    n_fg = sum(field_goal_result=="made",na.rm=T),
    n_fg_att = sum(!is.na(field_goal_result)),
    n_xp = sum(extra_point_result=="good",na.rm=T),
    n_xp_att = sum(!is.na(extra_point_result)),
    max_dist = max(kick_distance,na.rm=T),
    max_made_dist = max(kick_made_distance,na.rm=T),
    kickers_per_team = n_distinct(kicker_player_id),
    kicker_id = first(kicker_player_id),
    kicker_name = first(kicker_player_name)
    # ,
    # kicker_home = max(home_kick)
  ) %>%
  mutate(max_made_dist = ifelse(n_fg==0 & n_xp==0,NA,max_made_dist)) %>% 
  right_join(game_posteams,by=c("season","game_id","posteam","home_team")) %>%
  # clean out team-game combos where injury forced another kicker into the game
  filter(kickers_per_team == 1 | is.na(kickers_per_team)) %>% 
  mutate(n_fg = ifelse(is.na(n_fg),0,n_fg),
         n_fg_att = ifelse(is.na(n_fg_att),0,n_fg_att),
         n_xp = ifelse(is.na(n_xp),0,n_xp),
         n_xp_att = ifelse(is.na(n_xp_att),0,n_xp_att),
         total_points = ifelse(is.na(total_points),0,total_points),
         kicker_home = ifelse(posteam==home_team,1,0)
         ) %>% 
  group_by(posteam,season) %>% 
  arrange(posteam,season,game_id) %>% 
  fill(kicker_id,kicker_name,.direction="downup") %>% 
  group_by(season,kicker_name,kicker_id) %>% 
  mutate(kicker_szn_fg_made = sum(n_fg),
         kicker_szn_fg_att = sum(n_fg_att),
         kicker_szn_xp_made = sum(n_xp),
         kicker_szn_xp_att = sum(n_xp_att),
         kicker_szn_max_dist = max(max_dist,na.rm=TRUE),
         kicker_szn_max_made_dist = max(max_made_dist,na.rm=TRUE)) %>% 
  #filter(kicker_szn_fg_att > 0 & kicker_szn_xp_att > 0) %>% 
  mutate(kicker_szn_fg_pct = kicker_szn_fg_made/kicker_szn_fg_att,
         kicker_szn_xp_pct = kicker_szn_xp_made/kicker_szn_xp_att,
         kicker_szn_max_made_dist = ifelse(kicker_szn_fg_made == 0 &
                                             kicker_szn_xp_made ==0,
                                           NA,kicker_szn_max_made_dist),
         enough_kicks=ifelse(kicker_szn_fg_att >= 20 & 
                               kicker_szn_xp_att >= 20,TRUE,FALSE)) 

# calculate kicker/season-specific median max distance with enough attempts to 
# impute old data for a kicker when season data is unavailable
max_dist_impute <- kicks %>% 
  group_by(season,kicker_name,kicker_id) %>% 
  filter(enough_kicks==TRUE) %>% 
  summarize(kicker_szn_max_dist = mean(kicker_szn_max_dist),
            kicker_szn_max_made_dist = mean(kicker_szn_max_made_dist)) %>% 
  group_by(season) %>% 
  summarize(max_dist_to_impute = median(kicker_szn_max_dist),
            max_made_dist_to_impute = median(kicker_szn_max_made_dist)) 

# SIDEQUEST: quick plot to select a min attempt number
# kicks %>%
#   group_by(season,kicker_name,kicker_id) %>% 
#   summarize(kicker_szn_fg_att=mean(kicker_szn_fg_att),
#             kicker_szn_xp_att=mean(kicker_szn_xp_att)) %>% 
#   ggplot(aes(x=kicker_szn_fg_att)) + geom_histogram()
# 
# kicks %>%
#   group_by(season,kicker_name,kicker_id) %>% 
#   summarize(kicker_szn_fg_att=mean(kicker_szn_fg_att),
#             kicker_szn_xp_att=mean(kicker_szn_xp_att)) %>% 
#   ggplot(aes(x=kicker_szn_xp_att)) + geom_histogram()

# actually impute either kicker-specific or group values based on prior games kicked
kicks_impute <- kicks %>% 
  group_by(season) %>% 
  left_join(max_dist_impute,by="season") %>% 
  mutate(group_szn_fg_pct = sum(kicker_szn_fg_made)/sum(kicker_szn_fg_att),
         group_szn_xp_pct = sum(kicker_szn_xp_made)/sum(kicker_szn_xp_att)) %>% 
  mutate(kicker_szn_fg_pct = ifelse(enough_kicks == TRUE,kicker_szn_fg_pct,
                                    group_szn_fg_pct),
         kicker_szn_xp_pct = ifelse(enough_kicks == TRUE,kicker_szn_xp_pct,
                                    group_szn_xp_pct),
         kicker_szn_max_dist = ifelse(enough_kicks == TRUE,kicker_szn_max_dist,
                                      max_dist_to_impute),
         kicker_szn_max_made_dist = ifelse(enough_kicks == TRUE,kicker_szn_max_made_dist,
                                           max_made_dist_to_impute))

# create kicker season-level averages
kicker_szn <- kicks_impute %>% 
  group_by(kicker_id,kicker_name,season) %>%
  arrange(kicker_id,kicker_name,season) %>% 
  summarise(kicker_szn_fg_pct=mean(kicker_szn_fg_pct),
            kicker_szn_xp_pct=mean(kicker_szn_xp_pct),
            kicker_szn_max_dist=max(kicker_szn_max_dist),
            kicker_szn_max_made_dist = max(kicker_szn_max_made_dist),
            kicker_szn_games = n()) %>% 
  mutate(kicker_szn_fg_pct = ifelse(kicker_szn_games < 6,NA,kicker_szn_fg_pct),
         kicker_szn_xp_pct = ifelse(kicker_szn_games < 6,NA,kicker_szn_xp_pct),
         kicker_szn_max_dist = ifelse(kicker_szn_games < 6,NA,kicker_szn_max_dist),
         kicker_szn_max_made_dist = ifelse(kicker_szn_games < 6,NA,kicker_szn_max_made_dist)) %>% 
  mutate(sznlag_kicker_szn_fg_pct= lag(kicker_szn_fg_pct),
         sznlag_kicker_szn_xp_pct = lag(kicker_szn_xp_pct),
         sznlag_kicker_szn_max_dist = lag(kicker_szn_max_dist),
         sznlag_kicker_szn_max_made_dist = lag(kicker_szn_max_made_dist)) %>% 
  group_by(season) %>%
  # mutate(szn_max_dist = median(kicker_szn_max_dist,na.rm=T),
  #        szn_max_made_dist = median(kicker_szn_max_made_dist,na.rm=T)) %>%
  # group_by(kicker_id,kicker_name) %>%
  # arrange(kicker_id,kicker_name,season) %>%
  # mutate(lag_szn_max_dist = lag(szn_max_dist),
  #         lag_szn_max_made_dist = lag(szn_max_made_dist)) %>%
  select(kicker_id,kicker_name,season,sznlag_kicker_szn_fg_pct,sznlag_kicker_szn_xp_pct,
         sznlag_kicker_szn_max_dist,sznlag_kicker_szn_max_made_dist
         # ,
         # lag_szn_max_dist,lag_szn_max_made_dist
         )

# create lagged season values
all_kicker_szn <- kicks_impute %>%
  group_by(kicker_id,kicker_name,season) %>%
  arrange(kicker_id,kicker_name,season) %>% 
  mutate(kicker_szn_max_dist=max(kicker_szn_max_dist),
          kicker_szn_max_made_dist = max(kicker_szn_max_made_dist)) %>% 
group_by(season) %>% 
  summarize(szn_fg = sum(n_fg), szn_fg_att=sum(n_fg_att), 
         szn_xp=sum(n_xp), szn_xp_att=sum(n_xp_att),
         szn_fg_pct=szn_fg/szn_fg_att,
         szn_xp_pct=szn_xp/szn_xp_att,
         szn_max_dist=median(kicker_szn_max_dist,na.rm=TRUE),
         szn_max_made_dist = median(kicker_szn_max_made_dist,na.rm=TRUE)
         ) %>% 
  mutate(lag_szn_fg_pct = lag(szn_fg_pct),
         lag_szn_xp_pct = lag(szn_xp_pct),
         lag_szn_max_dist = lag(szn_max_dist),
         lag_szn_max_made_dist = lag(szn_max_made_dist)) %>% 
  select(season,lag_szn_fg_pct,lag_szn_xp_pct,lag_szn_max_dist,lag_szn_max_made_dist)

# FORK 2: CREATE CUMULATIVE AVERAGE VARIABLES
#calculate cumulative averages
kicks_mv <- kicks %>% 
  group_by(kicker_name,kicker_id,season) %>%
  arrange(kicker_name,kicker_id,season,game_id) %>% 
  select(kicker_name,kicker_id,season,game_id,
         n_fg,n_fg_att,n_xp,n_xp_att,max_dist,max_made_dist) %>%
  #fill NA max_dists for no-attempt games
  mutate(max_dist=ifelse(is.na(max_dist) & n_fg_att==0 & n_xp_att==0,
                           0,max_dist),
         max_made_dist= ifelse(is.na(max_made_dist) & n_fg==0 & n_xp==0,
                               0,max_made_dist)) %>% 
  mutate(fg_pct_mv = lag(cumsum(n_fg))/lag(cumsum(n_fg_att)),
         xp_pct_mv = lag(cumsum(n_xp))/lag(cumsum(n_xp_att)),
         max_dist_mv = lag(cummax(max_dist)),
         max_made_dist_mv = lag(cummax(max_made_dist)),
         n_kkrszn_mv = lag(cumsum(!is.na(kicker_name)))) %>% 
  left_join(kicker_szn, by=c("kicker_name","kicker_id","season")) %>% 
  left_join(all_kicker_szn, by=c("season")) %>% 
  arrange(kicker_name,kicker_id,season,game_id) %>% 
  mutate(fg_pct_mv = case_when((n_kkrszn_mv < 6 | is.na(n_kkrszn_mv)) &
                                 !is.na(sznlag_kicker_szn_fg_pct) ~ sznlag_kicker_szn_fg_pct,
                               (n_kkrszn_mv < 6 | is.na(n_kkrszn_mv)) & 
                                  is.na(sznlag_kicker_szn_fg_pct) ~ lag_szn_fg_pct,
                               TRUE ~ fg_pct_mv),
         xp_pct_mv = case_when((n_kkrszn_mv < 6 | is.na(n_kkrszn_mv)) &
                                 !is.na(sznlag_kicker_szn_xp_pct) ~ sznlag_kicker_szn_xp_pct,
                               (n_kkrszn_mv < 6 | is.na(n_kkrszn_mv)) & 
                                 is.na(sznlag_kicker_szn_xp_pct) ~ lag_szn_xp_pct,
                               TRUE ~ xp_pct_mv),
         max_dist_mv = case_when((n_kkrszn_mv < 6 | is.na(n_kkrszn_mv)) &
                                 !is.na(sznlag_kicker_szn_max_dist) ~ sznlag_kicker_szn_max_dist,
                               (n_kkrszn_mv < 6 | is.na(n_kkrszn_mv)) & 
                                 is.na(sznlag_kicker_szn_max_dist) ~ lag_szn_max_dist,
                               TRUE ~ max_dist_mv),
         max_made_dist_mv = case_when((n_kkrszn_mv < 6 | is.na(n_kkrszn_mv)) &
                                   !is.na(sznlag_kicker_szn_max_made_dist) ~ sznlag_kicker_szn_max_made_dist,
                                 (n_kkrszn_mv < 6 | is.na(n_kkrszn_mv)) & 
                                   is.na(sznlag_kicker_szn_max_made_dist) ~ lag_szn_max_made_dist,
                                 TRUE ~ max_made_dist_mv)) %>% 
  select(kicker_name,kicker_id,season,game_id,fg_pct_mv,xp_pct_mv,max_dist_mv,max_made_dist_mv)

# GATHER GAME-LEVEL VARIABLES
game_level <- data %>%
  select(season,game_id,week,home_team,posteam,defteam,away_team,roof,total_line,spread_line,
         field_goal_result,extra_point_result,wind_weather,precipitation,
         home_score,away_score) %>%
  filter(!is.na(posteam),!is.na(defteam)) %>% 
  mutate(home_imptot = (total_line/2) + (spread_line/2),
         away_imptot = (total_line/2) - (spread_line/2),
         pos_imptot = case_when(
           posteam == home_team ~ home_imptot,
           posteam == away_team ~ away_imptot,
            TRUE ~ NA),
         def_imptot = case_when(
           defteam == home_team ~ home_imptot,
           defteam == away_team ~ away_imptot,
           TRUE ~ NA),
         pos_imptotdiff = pos_imptot - def_imptot
         ) %>% 
  group_by(season,game_id,posteam,defteam,away_team,roof,wind_weather,precipitation,
           total_line,spread_line,pos_imptot,def_imptot,pos_imptotdiff,home_score,away_score) %>% 
  summarize(playcount = n(),fg_atts = sum(field_goal_result=="made",na.rm=TRUE),xp_atts=sum(extra_point_result=="good",na.rm=TRUE))

# FULL SEASON SEASON-LEVEL VARIABLES
#calculate season-level redzone efficiency, yellow zone efficiency
season_level <- data %>% 
  #dplyr::filter(season_type == "REG") %>%
  dplyr::group_by(posteam,season) %>% 
  #need to carry fourth_down N because we subset special teams away next
  mutate(fourth_down = ifelse(down==4,1,0),
         fourth_down_n = sum(fourth_down,na.rm=T)) %>% 
  dplyr::filter(!is.na(posteam),(rush == 1 | pass == 1 | penalty == 1),
                special_teams_play==0) 
#this includes penalties

zone_offense <- season_level %>%
  #play_level: identify relevant plays for specific instances
  mutate(redzone_epa = ifelse(yardline_100 < 20,epa,NA),
         pinkzone_epa = ifelse(yardline_100 < 40 & yardline_100 >= 20,epa,NA),
         big_play_20 = ifelse(yards_gained >= 20,1,0),
         neg_play_5 = ifelse(yards_gained <= -5,1,0),
         rushpass = case_when(
           rush == 1 ~ "rush",
           pass == 1 ~ "pass",
           TRUE ~ "other")
         ) %>% 
  #i'm leaving special teams plays in for now because penalties, punting?
  dplyr::mutate(off_rz_epa = mean(redzone_epa, na.rm = TRUE),
                   off_pz_epa = mean(pinkzone_epa, na.rm = TRUE),
                off_epa = mean(epa, na.rm=TRUE)
                ) %>% 
  #I was weeding out "other" before, but penalties should be informative
  #filter(rushpass != "other") %>% 
  #group_by(team,season,rushpass) %>% 
  mutate(off_rush_epa = ifelse(rushpass == "rush",epa,NA),
         off_pass_epa = ifelse(rushpass == "pass",epa,NA),
         fourth_down_att = fourth_down_converted + fourth_down_failed,
         nplays = n()) %>% 
  #epa means are just means of same number because already calc'd at season-level
  summarise(off_rz_epa = mean(off_rz_epa, na.rm = TRUE),
            off_pz_epa = mean(off_pz_epa, na.rm = TRUE),
            off_epa = mean(epa, na.rm = TRUE),
            off_rush_epa = mean(off_rush_epa, na.rm = TRUE),
            off_pass_epa = mean(off_pass_epa, na.rm = TRUE),
            off_success_rate = sum(success,na.rm=TRUE)/mean(nplays),
            big_play_20_rate = sum(big_play_20,na.rm = TRUE)/mean(nplays),
            neg_play_5_rate = sum(neg_play_5,na.rm= TRUE)/mean(nplays),
            mean_play_clock = mean(as.numeric(play_clock), na.rm = TRUE),
            fourth_down_n = mean(fourth_down_n, na.rm=T),
            fourth_down_att = sum(fourth_down_att,na.rm=TRUE),
            fourth_down_try_rate = sum(fourth_down_att,na.rm=TRUE)/mean(fourth_down_n,na.rm=TRUE)
            )

def_off <- zone_offense %>% 
  select(posteam,season,off_epa,off_success_rate) %>% 
  rename(def_off_epa = off_epa, defteam = posteam,def_off_success_rate = off_success_rate)

zone_defense <- season_level %>%
  dplyr::group_by(defteam,season) %>%
  mutate(redzone_epa = ifelse(yardline_100 < 20,epa,NA),
         pinkzone_epa = ifelse(yardline_100 < 40 & yardline_100 >= 20,epa,NA),
         rushpass = case_when(
           rush == 1 ~ "rush",
           pass == 1 ~ "pass",
           TRUE ~ "other")) %>% 
  arrange(season,defteam,week) %>%
  mutate(def_rush_epa = ifelse(rushpass == "rush",epa,NA),
         def_pass_epa = ifelse(rushpass == "pass",epa,NA)) %>% 
  # group_by(season,defteam) %>% 
  dplyr::summarize(def_rz_epa = mean(redzone_epa, na.rm = TRUE),
                   def_pz_epa = mean(pinkzone_epa, na.rm = TRUE),
                   def_epa = mean(epa, na.rm = TRUE),
                   # n_def_rz = sum(!is.na(redzone_epa)),
                   # n_def_pz = sum(!is.na(pinkzone_epa)),
                   n_plays = n(),
                   def_success_rate = sum(success,na.rm = TRUE)/n_plays,
                   def_rush_epa = mean(def_rush_epa, na.rm = TRUE),
                   def_pass_epa = mean(def_pass_epa, na.rm = TRUE)
                   # ,
                   # n_def_rush = sum(rushpass=="rush"),
                   # n_def_pass = sum(rushpass=="pass")
                   ) 
# %>% 
#   mutate(def_rz_epa_ma = lag(cummean(def_rz_epa))/lag(cummean(n_def_rz)),
#          def_pz_epa_ma = lag(cummean(def_pz_epa))/lag(cummean(n_def_pz)),
#          def_epa_ma = lag(cummean(def_epa))/lag(cummean(n_plays)),
#          def_rush_epa_ma = lag(cummean(def_rush_epa))/lag(cummean(n_def_rush)),
#          def_pass_epa_ma = lag(cummean(def_pass_epa))/lag(cummean(n_def_pass))
#   ) %>% 
#   group_by(defteam) %>% 
#   arrange(defteam,season,week) %>% 
#   fill(def_rz_epa_ma,def_pz_epa_ma,def_epa_ma,def_rush_epa_ma,def_pass_epa_ma) %>% 
#   select(defteam,season,week,
#          def_rz_epa_ma,def_pz_epa_ma,def_epa_ma,def_rush_epa_ma,def_pass_epa_ma)

pos_def <- zone_defense %>% 
  select(defteam,season,def_epa,def_success_rate) %>% 
  rename(pos_def_epa = def_epa,posteam = defteam,pos_def_success_rate = def_success_rate)

# FORK 2: Cumulative avgs to make predictors blind to future results
zone_offense_mvavg <- season_level %>%
  mutate(redzone_epa = ifelse(yardline_100 < 20,epa,NA),
         pinkzone_epa = ifelse(yardline_100 < 40 & yardline_100 >= 20,epa,NA),
         big_play_20 = ifelse(yards_gained >= 20,1,0),
         neg_play_5 = ifelse(yards_gained <= -5,1,0),
         rushpass = case_when(
           rush == 1 ~ "rush",
           pass == 1 ~ "pass",
           TRUE ~ "other")
  ) %>% 
  group_by(season,posteam,week) %>%
  arrange(season,posteam,week) %>% 
  #i'm leaving special teams plays in for now because penalties, punting?
  # dplyr::mutate(off_rz_epa = sum(redzone_epa, na.rm = TRUE),
  #                  off_pz_epa = sum(pinkzone_epa, na.rm = TRUE),
  #               n_off_rz = sum(!is.na(redzone_epa)),
  #               n_off_pz = sum(!is.na(pinkzone_epa))
  #               ) %>% 
  #I was weeding out "other" before, but penalties should be informative
  #filter(rushpass != "other") %>% 
  #group_by(team,season,rushpass) %>% 
  mutate(off_rush_epa = ifelse(rushpass == "rush",epa,NA),
         off_pass_epa = ifelse(rushpass == "pass",epa,NA),
         fourth_down_att = fourth_down_converted + fourth_down_failed,
         fourth_down_n = sum(fourth_down,na.rm=T)
         # ,
         # nplays = n()
         ) %>% 
  #i'm leaving special teams plays in for now because penalties, punting?
  dplyr::summarize(off_rz_epa = sum(redzone_epa, na.rm = TRUE),
                off_pz_epa = sum(pinkzone_epa, na.rm = TRUE),
                n_off_rz = sum(!is.na(redzone_epa)),
                n_off_pz = sum(!is.na(pinkzone_epa)),
                off_rush_epa = sum(redzone_epa, na.rm = TRUE),
                off_pass_epa = sum(pinkzone_epa, na.rm = TRUE),
                off_epa = sum(epa, na.rm=TRUE),
                success_plays = sum(success, na.rm=TRUE),
                n_plays = n(),
                n_plays_20 = sum(yardline_100 >= 20),
                n_plays_5 = sum(yardline_100 <= 95),
                n_off_rush = sum(rushpass == "rush"),
                n_off_pass = sum(rushpass == "pass"),
                big_play_20_n = sum(big_play_20,na.rm = TRUE),
                neg_play_5_n = sum(neg_play_5,na.rm= TRUE),
                sum_play_clock = sum(as.numeric(play_clock), na.rm = TRUE),
                fourth_down_n = mean(fourth_down_n, na.rm=T),
                fourth_down_att = sum(fourth_down_att,na.rm=TRUE)
  ) %>% 
    mutate(off_rz_epa_ma = lag(cumsum(off_rz_epa))/lag(cumsum(n_off_rz)),
              off_pz_epa_ma = lag(cumsum(off_pz_epa))/lag(cumsum(n_off_pz)),
              off_rush_epa_ma = lag(cumsum(off_rush_epa))/lag(cumsum(n_off_rush)),
              off_pass_epa_ma = lag(cumsum(off_pass_epa))/lag(cumsum(n_off_pass)),
              off_epa_ma = lag(cumsum(off_epa))/lag(cumsum(n_plays)),
              off_success_rate_ma = lag(cumsum(success_plays)/lag(cumsum(n_plays))),
              big_play_20_rate_ma = lag(cumsum(big_play_20_n))/lag(cumsum(n_plays_20)),
              neg_play_5_rate_ma = lag(cumsum(neg_play_5_n))/lag(cumsum(n_plays_5)),
              mean_play_clock_ma = lag(cumsum((sum_play_clock)))/lag(cumsum(n_plays)),
              fourth_down_try_rate_ma = lag(cumsum(fourth_down_att))/lag(cumsum(fourth_down_n))
    ) %>% 
  group_by(posteam) %>% 
  arrange(posteam,season,week) %>% 
  fill(off_rz_epa_ma,off_pz_epa_ma,off_epa_ma,off_rush_epa_ma,off_pass_epa_ma,
       big_play_20_rate_ma,neg_play_5_rate_ma,mean_play_clock_ma,fourth_down_try_rate_ma,
       off_success_rate_ma) %>% 
  select(posteam,season,week,
         off_rz_epa_ma,off_pz_epa_ma,off_epa_ma,off_rush_epa_ma,off_pass_epa_ma,
         big_play_20_rate_ma,neg_play_5_rate_ma,mean_play_clock_ma,fourth_down_try_rate_ma,
         off_success_rate_ma)

def_off_mvavg <- zone_offense_mvavg %>% 
  select(posteam,season,week,off_epa_ma,off_success_rate_ma) %>% 
  rename(def_off_epa_ma = off_epa_ma, defteam = posteam,def_off_success_rate_ma = off_success_rate_ma)

zone_defense_mvavg <- season_level %>%
  dplyr::group_by(team = defteam,season) %>%
  mutate(redzone_epa = ifelse(yardline_100 < 20,epa,NA),
         pinkzone_epa = ifelse(yardline_100 < 40 & yardline_100 >= 20,epa,NA),
         rushpass = case_when(
           rush == 1 ~ "rush",
           pass == 1 ~ "pass",
           TRUE ~ "other")) %>% 
  mutate(def_rush_epa = ifelse(rushpass == "rush",epa,NA),
         def_pass_epa = ifelse(rushpass == "pass",epa,NA)) %>% 
  arrange(season,defteam,week) %>% 
  group_by(season,defteam,week) %>% 
  dplyr::summarize(def_rz_epa = sum(redzone_epa, na.rm = TRUE),
                def_pz_epa = sum(pinkzone_epa, na.rm = TRUE),
                def_epa = sum(epa, na.rm = TRUE),
                n_def_rz = sum(!is.na(redzone_epa)),
                n_def_pz = sum(!is.na(pinkzone_epa)),
                n_plays = n(),
                def_rush_epa = sum(def_rush_epa, na.rm = TRUE),
                def_pass_epa = sum(def_pass_epa, na.rm = TRUE),
                n_def_rush = sum(rushpass=="rush"),
                n_def_pass = sum(rushpass=="pass"),
                success_plays = sum(success,na.rm=TRUE)) %>% 
  mutate(def_rz_epa_ma = lag(cumsum(def_rz_epa))/lag(cumsum(n_def_rz)),
            def_pz_epa_ma = lag(cumsum(def_pz_epa))/lag(cumsum(n_def_pz)),
            def_epa_ma = lag(cumsum(def_epa))/lag(cumsum(n_plays)),
            def_rush_epa_ma = lag(cumsum(def_rush_epa))/lag(cumsum(n_def_rush)),
            def_pass_epa_ma = lag(cumsum(def_pass_epa))/lag(cumsum(n_def_pass)),
            def_success_rate_ma = lag(cumsum(success_plays))/lag(cummean(n_plays))
         ) %>% 
  group_by(defteam) %>% 
  arrange(defteam,season,week) %>% 
    fill(def_rz_epa_ma,def_pz_epa_ma,def_epa_ma,def_rush_epa_ma,def_pass_epa_ma,def_success_rate_ma) %>% 
  select(defteam,season,week,
         def_rz_epa_ma,def_pz_epa_ma,def_epa_ma,def_rush_epa_ma,def_pass_epa_ma,def_success_rate_ma)

pos_def_mvavg <- zone_defense_mvavg %>% 
  select(defteam,season,week,def_epa_ma,def_success_rate_ma) %>% 
  rename(pos_def_epa_ma = def_epa_ma,posteam = defteam,pos_def_success_rate_ma = def_success_rate_ma)

#call DVOAs
defdvoa2020 <- read_csv("C:\\Users\\chknd\\Documents\\total_team_dvoa_2020.csv") %>% 
  select(-Rank) %>% 
  rename_with(str_to_title) %>% 
  mutate(season=2020) %>% 
  #rename(c("Team"="?..Team")) %>% 
  rename(c("Total.dvoa" = "Total Dvoa","Offense.dvoa" = "Offense Dvoa",
           "Defense.dvoa" = "Defense Dvoa", "Weighted.dvoa" = "Weighted Dvoa")) %>% 
  select(season, Team, Total.dvoa, Offense.dvoa, Defense.dvoa, Weighted.dvoa)

defdvoa2021 <- read_csv("C:\\Users\\chknd\\Documents\\total_team_dvoa_2021.csv") %>% 
  select(-Rank) %>% 
  rename_with(str_to_title) %>% 
  mutate(season=2021) %>% 
  #rename(c("Team"="?..Team")) %>% 
  rename(c("Total.dvoa" = "Total Dvoa","Offense.dvoa" = "Offense Dvoa",
           "Defense.dvoa" = "Defense Dvoa", "Weighted.dvoa" = "Weighted Dvoa")) %>% 
  select(season, Team, Total.dvoa, Offense.dvoa, Defense.dvoa, Weighted.dvoa)

defdvoa2022 <- read_csv("C:\\Users\\chknd\\Documents\\total_team_dvoa_2022.csv") %>% 
  select(-Rank) %>% 
  rename_with(str_to_title) %>% 
  mutate(season=2022) %>% 
  #rename(c("Team"="?..Team")) %>%   
  rename(c("Total.dvoa" = "Total Dvoa","Offense.dvoa" = "Offense Dvoa",
  "Defense.dvoa" = "Defense Dvoa", "Weighted.dvoa" = "Weighted Dvoa")) %>% 
  select(season, Team, Total.dvoa, Offense.dvoa, Defense.dvoa, Weighted.dvoa)

defdvoa2023 <- read_csv("C:\\Users\\chknd\\Documents\\total_team_dvoa_2023full.csv") %>% 
  select(-Rank) %>% 
  rename_with(str_to_title) %>% 
  mutate(season=2023) %>% 
  #rename(c("Team"="?..Team")) %>% 
  rename(c("Total.dvoa" = "Total Dvoa","Offense.dvoa" = "Offense Dvoa",
           "Defense.dvoa" = "Defense Dvoa", "Weighted.dvoa" = "Weighted Dvoa")) %>% 
  select(season, Team, Total.dvoa, Offense.dvoa, Defense.dvoa, Weighted.dvoa) %>% 
  mutate(Team=ifelse(Team=="JAC","JAX",Team))

defdvoa <- defdvoa2020 %>% 
  bind_rows(defdvoa2021,defdvoa2022,defdvoa2023) %>% 
  mutate(Team=ifelse(Team=="JAC","JAX",Team))

#overall_kkr_mean = mean(kicks_impute$total_points,na.rm=T)
overall_kkr_mean <- kicks_impute %>% 
  filter(season > 2017) %>% 
  ungroup() %>% 
  summarize(overall_kkr_mean=mean(total_points,na.rm=T)) %>%
  pull(overall_kkr_mean)

# Combine data 
final_data <- game_level %>% 
  right_join(kicks_impute,by=c("season","game_id","posteam")) %>% 
  left_join(kicks_mv, by= c("season","kicker_id","kicker_name","game_id")) %>% 
  group_by(season,posteam) %>% 
  arrange(kicker_id,kicker_name,game_id) %>% 
  #to fill if first kicker 
  # fill(c("kicker_home","kicker_szn_fg_pct","kicker_szn_xp_pct","kicker_szn_max_dist"), .direction = "downup") %>% 
  #next, get cumulative means for a team given up to kickers, and specifically for kickers
  group_by(season,defteam) %>%
  arrange(season,defteam,game_id) %>% 
  mutate(szn_def_kkr_mean_pts = lag(cummean(total_points))) %>% 
  group_by(season,posteam,kicker_id,kicker_name) %>%
  arrange(season,posteam,kicker_id,kicker_name,game_id) %>% 
  mutate(szn_kkr_mean_pts = lag(cummean(total_points))) %>%
  mutate(szn_def_kkr_mean_pts = ifelse(is.na(szn_def_kkr_mean_pts),
                                       overall_kkr_mean,szn_def_kkr_mean_pts),
         szn_kkr_mean_pts = ifelse(is.na(szn_kkr_mean_pts),
                                       overall_kkr_mean,szn_kkr_mean_pts),
         week = as.numeric(str_sub(game_id,6,7))) %>% 
  #back to combining 
  group_by(season,posteam) %>% 
  arrange(season,posteam,week) %>% 
  #had "posteam" = "team" here for some reason?
  left_join(zone_offense,by=c("season","posteam")) %>% 
  left_join(zone_defense,by=c("season", "defteam")) %>%
  left_join(zone_offense_mvavg,by=c("season","posteam" ,"week")) %>% 
  left_join(zone_defense_mvavg,by=c("season", "defteam" ,"week")) %>%
  left_join(def_off,by=c("season","defteam")) %>% 
  left_join(pos_def,by=c("season","posteam")) %>% 
  left_join(def_off_mvavg,by=c("season","defteam","week")) %>% 
  left_join(pos_def_mvavg,by=c("season","posteam","week")) %>%  
  ungroup() %>% 
  full_join(defdvoa,by=c("season","posteam" = "Team")) %>% 
  rename("Pos_Total_DVOA"="Total.dvoa","Pos_Offense_DVOA"="Offense.dvoa","Pos_Defense_DVOA"=
           "Defense.dvoa","Pos_Wtd_DVOA"="Weighted.dvoa") %>% 
  full_join(defdvoa,by=c("season","defteam" = "Team")) %>% 
  rename("Def_Total_DVOA"="Total.dvoa","Def_Offense_DVOA"="Offense.dvoa","Def_Defense_DVOA"=
           "Defense.dvoa","Def_Wtd_DVOA"="Weighted.dvoa") %>% 
  mutate(roof=ifelse(is.na(roof),"retractable",roof),
         total_points = ifelse(fg_atts==0 & xp_atts==0,0,total_points)) %>% 
  dummy_cols(select_columns=c("roof","precipitation"),remove_most_frequent_dummy = TRUE) %>% 
  mutate(posteam_seas = paste(posteam,season,sep="_")) %>% 
  arrange(season,posteam,week) %>% 
  mutate(pos_score = case_when(posteam==home_team ~ home_score,
                               posteam==away_team ~ away_score,
                               TRUE ~ NA),
         def_score = case_when(defteam==home_team ~ home_score,
                               defteam==away_team ~ away_score,
                               TRUE ~ NA)) %>% 
  filter(season > 2017) %>% 
  filter(!((week == 18 & season > 2020) | (week == 17 & season == 2020)))

#modeling
outcome_name = "total_points"
Cov_old_set = c("pos_imptot","def_imptot",
            "pos_imptotdiff","week","season",
            "kicker_szn_max_dist","kicker_home","kicker_szn_fg_pct","kicker_szn_xp_pct",
            "off_epa","off_rz_epa","off_pz_epa","off_rush_epa","off_pass_epa",
            "def_epa",
            #"def_rz_epa","def_pz_epa","def_rush_epa","def_pass_epa",
            "big_play_20_rate","neg_play_5_rate",
            #"fourth_down_try_rate",
            "off_success_rate","def_success_rate","pos_def_success_rate","def_off_success_rate",
            #"mean_play_clock","mean_play_clock_ma",
            "szn_def_kkr_mean_pts","szn_kkr_mean_pts",
            #"Pos_Offense_DVOA","Pos_Defense_DVOA","Def_Offense_DVOA","Def_Defense_DVOA",
            "fourth_down_try_rate","big_play_20_rate","neg_play_5_rate",
            "roof_closed","roof_dome","roof_open",
            "wind_weather","precipitation_unknown","precipitation_yes")

Cov_ma_set = c("pos_imptot","def_imptot",
            "pos_imptotdiff","week","season",
            "max_dist_mv","kicker_home","fg_pct_mv","xp_pct_mv",
            "off_epa_ma","off_rz_epa_ma","off_pz_epa_ma","off_rush_epa_ma","off_pass_epa_ma",
            "def_epa_ma","def_rz_epa_ma","def_pz_epa_ma","def_rush_epa_ma","def_pass_epa_ma",
            "pos_def_epa_ma","def_off_epa_ma",
            "off_success_rate_ma","def_success_rate_ma","pos_def_success_rate_ma","def_off_success_rate_ma",
            "big_play_20_rate_ma","neg_play_5_rate_ma","fourth_down_try_rate_ma",
            #"mean_play_clock","mean_play_clock_ma",
            "szn_def_kkr_mean_pts","szn_kkr_mean_pts",
            #"Pos_Offense_DVOA","Pos_Defense_DVOA","Def_Offense_DVOA","Def_Defense_DVOA",
            "roof_closed","roof_dome","roof_open",
            "wind_weather","precipitation_unknown","precipitation_yes")

Cov_ma_small = c("pos_imptot","def_imptot",
               "week","season",
               "max_dist_mv","kicker_home","fg_pct_mv","xp_pct_mv",
               #"off_epa_ma","off_rz_epa_ma","off_pz_epa_ma",
               #"off_rush_epa_ma","off_pass_epa_ma",
               #"def_epa_ma","def_rz_epa_ma","def_pz_epa_ma",
               #"def_rush_epa_ma","def_pass_epa_ma",
               #"pos_def_epa_ma","def_off_epa_ma",
               #"off_success_rate_ma","def_success_rate_ma","pos_def_success_rate_ma","def_off_success_rate_ma",
               #"big_play_20_rate_ma","neg_play_5_rate_ma",
               "fourth_down_try_rate_ma",
               #"mean_play_clock","mean_play_clock_ma",
               "szn_def_kkr_mean_pts","szn_kkr_mean_pts",
               #"Pos_Offense_DVOA","Pos_Defense_DVOA","Def_Offense_DVOA","Def_Defense_DVOA",
               "roof_closed","roof_dome","roof_open",
               "wind_weather","precipitation_unknown","precipitation_yes")

Cov_ma_smaller = c("pos_imptot","def_imptot",
                 "week","season",
                 #"max_dist_mv","fg_pct_mv",
                 #"kicker_home","xp_pct_mv",
                 #"off_epa_ma","off_rz_epa_ma",
                 "off_pz_epa_ma",
                 #"off_rush_epa_ma","off_pass_epa_ma",
                 #"def_epa_ma","def_rz_epa_ma",
                 "def_pz_epa_ma",
                 #"def_rush_epa_ma","def_pass_epa_ma",
                 #"pos_def_epa_ma","def_off_epa_ma",
                 #"off_success_rate_ma","def_success_rate_ma","pos_def_success_rate_ma","def_off_success_rate_ma",
                 #"big_play_20_rate_ma","neg_play_5_rate_ma",
                 "fourth_down_try_rate_ma",
                 #"mean_play_clock","mean_play_clock_ma",
                 #"szn_def_kkr_mean_pts","szn_kkr_mean_pts",
                 #"Pos_Offense_DVOA","Pos_Defense_DVOA","Def_Offense_DVOA","Def_Defense_DVOA",
                 "roof_closed","roof_dome","roof_open",
                 "wind_weather","precipitation_unknown","precipitation_yes")

Cov_ma_finalscore = c("pos_score","def_score",
                 "week","season",
                 "max_dist_mv","kicker_home","fg_pct_mv","xp_pct_mv",
                 #"off_epa_ma","off_rz_epa_ma","off_pz_epa_ma",
                 #"off_rush_epa_ma","off_pass_epa_ma",
                 #"def_epa_ma","def_rz_epa_ma","def_pz_epa_ma",
                 #"def_rush_epa_ma","def_pass_epa_ma",
                 #"pos_def_epa_ma","def_off_epa_ma",
                 #"off_success_rate_ma","def_success_rate_ma","pos_def_success_rate_ma","def_off_success_rate_ma",
                 #"big_play_20_rate_ma","neg_play_5_rate_ma",
                 "fourth_down_try_rate_ma",
                 #"mean_play_clock","mean_play_clock_ma",
                 "szn_def_kkr_mean_pts","szn_kkr_mean_pts",
                 #"Pos_Offense_DVOA","Pos_Defense_DVOA","Def_Offense_DVOA","Def_Defense_DVOA",
                 "roof_closed","roof_dome","roof_open",
                 "wind_weather","precipitation_unknown","precipitation_yes")

Cov_set = Cov_ma_small

# Cov_set = c("pos_imptot","def_imptot","pos_imptotdiff","kicker_szn_max_dist","kicker_home","kicker_szn_fg_pct",
#             "kicker_szn_xp_pct",
#             "roof_closed","roof_dome","roof_open",
#             "roof_retractable")

cluster_id <- final_data %>%
  select(posteam_seas) %>% 
  group_by(posteam_seas) %>%
  dplyr::mutate(pos_seas = cur_group_id()) %>% 
  pull(pos_seas)

final_train <- final_data %>% 
  filter(season < 2023)
#the only missings are for max_dist, which we don't use in model
#extra arrangement for final dataset

Y <- pull(final_train[,outcome_name])

X_final <- final_train %>% 
  select(-all_of(outcome_name)) %>% 
  mutate_at(c("roof_dome", "roof_open", "roof_closed", 
              "precipitation_unknown","precipitation_yes"),as.logical) 

#quick function to check missing variables each columns
chkna <- function(x){sum(is.na(x))}
na_chking <- X_final %>% summarise_all(chkna)
#no NAs

# slightly annoyed it appends the value for logical variables
X <- model.matrix(formula(paste0("~", paste0(c(0,Cov_set), collapse="+"))), data=X_final)
X <- X[,colnames(X) != "roof_closedFALSE"]

XY <- cbind(Y,X) 
XY_clust <- as.data.frame(XY)

### CREATE TEST SET

final_test <- final_data %>% 
  filter(season == 2023)

X_test_df<- final_test %>% 
  select(-all_of(outcome_name)) %>% 
  mutate_at(c("roof_dome", "roof_open", "roof_closed", 
              "precipitation_unknown","precipitation_yes"),as.logical) 

#WHY DOES IT CREATE ROOF_CLOSEDFALSE??????
X_test <- model.matrix(formula(paste0("~", paste0(c(0,Cov_set), collapse="+"))), data=X_test_df)
X_test <- X_test[,colnames(X_test) != "roof_closedFALSE"]

Y_test <- pull(final_test[,outcome_name])

XY_test <- X_test %>% 
  bind_cols(total_points = Y_test)

library(corrplot)
m <- cor(X)
corrplot(m)

### Cescriptives

final_train %>%
  select(total_points, pos_imptot, def_imptot, pos_imptotdiff,week,
         max_dist_mv,max_made_dist_mv,kicker_home,fg_pct_mv,xp_pct_mv,
         off_epa_ma,off_success_rate_ma,off_rz_epa_ma,off_pz_epa_ma,
         off_rush_epa_ma,off_pass_epa_ma,pos_def_epa_ma,def_off_epa_ma,
         def_epa_ma,def_success_rate_ma,def_rz_epa_ma,def_pz_epa_ma,
         def_rush_epa_ma, def_pass_epa_ma,
         pos_def_success_rate,def_off_success_rate,
         big_play_20_rate_ma,neg_play_5_rate_ma,fourth_down_try_rate_ma,
         Pos_Offense_DVOA,Pos_Defense_DVOA,Def_Offense_DVOA,Def_Defense_DVOA,
         szn_def_kkr_mean_pts,szn_kkr_mean_pts,
         roof_closed,roof_open,roof_dome,
         precipitation_unknown,precipitation_yes,
         wind_weather,week,season
         ) %>%
  tbl_uvregression(
    method = glm,
    y = total_points,
    method.args = list(family = gaussian),
    #method.args = list(family = binomial),
    #exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) 
  
  
final_train %>%
select(total_points, pos_imptot, def_imptot, pos_imptotdiff,week,
         max_dist,kicker_home,kicker_szn_fg_pct,kicker_szn_xp_pct,
         off_epa,off_success_rate,off_rz_epa,off_pz_epa,
         off_rush_epa,off_pass_epa,pos_def_epa,def_off_epa,
         def_epa,def_success_rate,def_rz_epa,def_pz_epa,
       def_rush_epa_ma, def_pass_epa_ma,
         big_play_20_rate,neg_play_5_rate,fourth_down_try_rate,
         Pos_Offense_DVOA,Pos_Defense_DVOA,Def_Offense_DVOA,Def_Defense_DVOA,
         roof_closed,roof_open,roof_dome,
       precipitation_unknown,precipitation_yes,
         wind_weather
  ) %>%
  tbl_uvregression(
    method = glm,
    y = total_points,
    method.args = list(family = gaussian),
    #exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) 

#### OTHER METHODOLOGIES USING TIDYMODELS

#set this up? https://optimumsportsperformance.com/blog/tidymodels-train-test-set-without-initial_split/
library(tidymodels)
library(stacks)

#this is what I did initially
# XY_clust <- initial_split(XY_clust, prop=.75)
# XY_train <- training(XY_clust)
# XY_folds <- vfold_cv(XY_train,v=10) 

set.seed(567)
XY_folds <- vfold_cv(data = XY_clust, v = 10,strata=season)

#normalization is used mainly in linear models/KNN/neural networks because 
#they're affected by absolute values taken by features
simple_prep_rec <-
  recipe(Y ~ ., data = XY_clust)  %>%
  # remove any zero variance predictors
  step_zv(all_predictors()) %>% 
  step_corr(all_numeric_predictors()) %>% 
  # remove any linear combinations
  step_lincomb(all_numeric_predictors())

ns_prep_rec <- simple_prep_rec %>% 
  step_ns(pos_imptot, def_imptot)

norm_prep_rec <- simple_prep_rec %>% 
  #step_normalize(all_numeric_predictors())
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) 

cookie <- simple_prep_rec %>% prep() %>% 
  bake(X_test) 

# new_train <- prep_rec %>% 
#   prep() %>% 
#   bake(prep_rec, new_data=NULL)

#rmse_vals <- metric_set(mae)
rmse_vals <- metric_set(rmse,mae,rsq)
rsq_vals <- metric_set(rsq,rmse,mae)

#set up parameter tuning
ctrl_grid <- control_stack_bayes()
ctrl_res <- control_stack_resamples()
ctrl_lh <- control_stack_grid()

#should help with speed for some methods
doParallel::registerDoParallel()

### SVM
#Tidyversion - Resource: https://www.tidymodels.org/learn/work/tune-svm/
#Note: Tried SVM_poly but it seems brutally slow
library(tidymodels)
svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("regression") %>%
  set_engine("kernlab")

svm_grid <- grid_latin_hypercube(
  cost(),
  rbf_sigma(),
  #margin(),
  size = 20
)

svm_grid

svm_wf <- workflow() %>%
  add_recipe(norm_prep_rec) %>% 
  add_model(svm_mod) 

set.seed(888)
print(Sys.time())
starttime = Sys.time()
svmrbf_res <-
  tune_bayes(
    svm_wf,
resamples = XY_folds,
    metrics = rmse_vals,
  control = ctrl_grid,
  initial = 10, iter = 20
  )
svmrbf_res
print(Sys.time()-starttime)

svmrbf_res %>% 
  select(.metrics) %>% 
  slice(1) %>% 
  pull(1)

estimates <- collect_metrics(svmrbf_res)
estimates

show_best(svmrbf_res, metric = c("rmse","mae"))

best_rmse  <- select_best(svmrbf_res, c("rmse"))
best_rmse

final_svm <- finalize_workflow(
  svm_wf,
  best_rmse
)
final_svm

svm_final <- final_svm %>%
  fit(data = XY_clust)

svm_finalpred <- predict(svm_final, XY_clust)

svm_rmse_vals_train <- predict(svm_final,as.data.frame(X)) %>% 
  rmse_vals(Y, .pred) %>% 
  mutate(model = "svm")
svm_rmse_vals_train

svm_rmse_vals_test <- predict(svm_final,as.data.frame(X_test)) %>% 
  rmse_vals(Y_test,.pred) %>% 
  mutate(model = "svm")
svm_rmse_vals_test

predict(svm_final,as.data.frame(X)) %>%
  ggplot(aes(x = .pred, y = Y)) +
  geom_point(size = 1.5, color = "midnightblue") +
  geom_smooth(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2, method="loess"
  ) + xlim(-5,25) + ylim(-5,25) + labs(title="SVM regression",
                                       subtitle = "Training Data")

predict(svm_final,as.data.frame(X_test)) %>%
  ggplot(aes(x = .pred, y = Y_test)) +
  geom_point(alpha=.1, size = 1.5, color = "midnightblue") +
  geom_smooth(method="loess",
    alpha = 0.5,
    color = "red",
    size = 1.2
  ) + xlim(-5,25) + ylim(-5,25) + labs(title="SVM regression",
                                       subtitle = "Test Data")

### XGBOOST
#xgboost: https://juliasilge.com/blog/xgboost-tune-volleyball/
#1.875915 hours for 10000 trees but very small improvement
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), 
  mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), XY),
  learn_rate(),
  size = 30
)

xgb_grid

xgb_wf <- workflow() %>%
  add_recipe(simple_prep_rec) %>% 
  add_model(xgb_spec)

xgb_wf

bayes_param <- xgb_wf %>% 
  extract_parameter_set_dials() %>% 
  update(mtry = finalize(mtry(), X))

doParallel::registerDoParallel()

set.seed(888)
starttime = Sys.time()
xgb_res <- tune_bayes(
  xgb_wf,
  resamples = XY_folds,
  param_info = bayes_param,
  metrics=rmse_vals,
  control=ctrl_grid,
  initial = 18, iter = 18
)
print(Sys.time()-starttime)
#28.67968 mins for 18/18 bayes

xgb_res

collect_metrics(xgb_res)

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "mae")

show_mae <- show_best(xgb_res, "rmse")
best_rmse <- select_best(xgb_res, "rmse") 

final_xgb <- finalize_workflow(
  xgb_wf,
  best_rmse
)

xgb_final <- final_xgb %>%
  fit(data = XY_clust)

xgb_rmse_vals_train <- predict(xgb_final,as.data.frame(X)) %>% 
  rmse_vals(Y,.pred) %>% 
  mutate(model = "xgb")
xgb_rmse_vals_train

xgb_rmse_vals_test <- predict(xgb_final,as.data.frame(X_test)) %>% 
  rmse_vals(Y_test,.pred) %>% 
  mutate(model = "xgb")
xgb_rmse_vals_test

library(vip)

final_xgb %>%
  fit(data = XY) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")

predict(xgb_final,as.data.frame(X)) %>%
  ggplot(aes(x = .pred, y = Y)) +
  geom_point(size = 1.5, color = "midnightblue") +
  geom_smooth(method="loess",
    lty = 2, alpha = 0.5,
    color = "red",
    size = 1.2
  ) + xlim(-5,25) + ylim(-5,25) + labs(title="XGB",subtitle="Training Data")

predict(xgb_final,as.data.frame(X_test)) %>%
  ggplot(aes(x = .pred, y = Y_test)) +
  geom_point(alpha=.1,size = 1.5, color = "midnightblue") +
  geom_smooth(method="loess",
    alpha = 0.5,
    color = "red",
    size = 1.2
  ) + xlim(-5,25) + ylim(-5,25) + labs(title="XGB",subtitle="Test Data")

### GLMNET LIN

glmn_spec <- linear_reg(
  penalty = tune(),
  mixture = 0.5                         ## step size
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

glmn_grid <- grid_latin_hypercube(
  penalty(),
  size = 15
)

glmn_wf <- workflow() %>%
  add_recipe(norm_prep_rec) %>% 
  add_model(glmn_spec)

doParallel::registerDoParallel()

set.seed(888)
glmn_res <- tune_grid(
  glmn_wf,
  resamples = XY_folds,
  grid = glmn_grid,
  metrics=rmse_vals,
  control = ctrl_lh
)

collect_metrics(glmn_res)

show_best(glmn_res, "mae")

best_rmse <- select_best(glmn_res, "rmse")

final_glmn <- finalize_workflow(
  glmn_wf,
  best_rmse
)

final_glmn

library(vip)

final_glmn %>%
  fit(data = XY) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")

final_glmn %>%
  fit(data = XY) %>%
  tidy()

glmn_final <- final_glmn %>%
  fit(XY_clust)

predict(glmn_final,as.data.frame(X)) %>%
  ggplot(aes(x = .pred, y = Y)) +
  geom_point(size = 1.5, color = "midnightblue") +
  geom_smooth(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  ) + xlim(-5,25) + ylim(-5,25) + labs(title="Elastic Net Regression",subtitle="Training Data")

glmn_rmse_vals_train <- predict(glmn_final,as.data.frame(X)) %>% 
  rmse_vals(Y,.pred) %>% 
  mutate(model = "glmn")
glmn_rmse_vals_train

glmn_rmse_vals_test <- predict(glmn_final,as.data.frame(X_test)) %>% 
  rmse_vals(Y_test,.pred) %>% 
  mutate(model = "glmn")
glmn_rmse_vals_test

predict(glmn_final,as.data.frame(X_test)) %>% 
  ggplot(aes(x=.pred,y=Y_test)) + geom_point(alpha=.1) + geom_smooth(method="loess",color="red") + xlim(c(-2,25)) + 
  labs(title="Elastic Net Regression",subtitle="Test Data")

glmn_final %>%
  extract_fit_parsnip() %>%
  vip::vi() %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)

### RIDGE REG

ridge_spec <- linear_reg(
  penalty = tune(),
  mixture = 1                         ## step size
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

ridge_grid <- grid_latin_hypercube(
  penalty(),
  size = 15
)

ridge_wf <- workflow() %>%
  add_recipe(norm_prep_rec) %>% 
  add_model(ridge_spec)

doParallel::registerDoParallel()

set.seed(888)
ridge_res <- tune_grid(
  ridge_wf,
  resamples = XY_folds,
  grid = ridge_grid,
  metrics=rmse_vals,
  control = ctrl_lh
)

collect_metrics(ridge_res)

show_best(ridge_res, "mae")

best_rmse <- select_best(ridge_res, "rmse")

final_ridge <- finalize_workflow(
  ridge_wf,
  best_rmse
)

final_ridge

library(vip)

final_ridge %>%
  fit(data = XY) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")

final_ridge %>%
  fit(data = XY) %>%
  tidy()

ridge_final <- final_ridge %>%
  fit(XY_clust)

ridge_rmse_vals_train <- predict(ridge_final,as.data.frame(X)) %>% 
  rmse_vals(Y,.pred) %>% 
  mutate(model = "ridge")
ridge_rmse_vals_train

predict(ridge_final,as.data.frame(X)) %>% 
  ggplot(aes(x=.pred,y=Y)) + geom_point() + geom_smooth() + xlim(c(-2,25)) +
  labs(title="Ridge Regression",subtitle="Training Data")

ridge_rmse_vals_test <- predict(ridge_final,as.data.frame(X_test)) %>% 
  rmse_vals(Y_test,.pred) %>% 
  mutate(model = "ridge")
ridge_rmse_vals_test
# 1 rmse    standard      4.60   ridge
# 2 mae     standard      3.62   ridge
# 3 rsq     standard      0.0276 ridge

predict(ridge_final,as.data.frame(X_test)) %>% 
  ggplot(aes(x=.pred,y=Y_test)) + geom_hex() + geom_smooth(method="loess",color="red") + xlim(c(-2,25)) +
  scale_fill_viridis() + labs(title="Ridge Regression",subtitle="Test Data")

predict(ridge_final,as.data.frame(X_test)) %>% 
  ggplot(aes(x=.pred,y=Y_test)) + geom_point(alpha = .1) + geom_smooth(method="loess",color="red") + xlim(c(-2,25)) +
 labs(title="Ridge Regression",subtitle="Test Data")

library(vip)
library(forcats)
library(viridis)

## FIGURE 3. RIDGE REGRESSION VARIABLE IMPORTANCE
#I don't know why, but saving the environment breaks this
viplot <- ridge_final %>%
  extract_fit_parsnip() %>%
  vip::vi() %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  mutate(Variable = factor(Variable,
                    levels = c("pos_imptot","def_imptot","szn_def_kkr_mean_pts",
                               "wind_weather","roof_domeTRUE","roof_closedTRUE",
                               "szn_kkr_mean_pts","roof_openTRUE","season",
                               "precipitation_yesTRUE","kicker_home","fourth_down_try_rate_ma",
                               "max_dist_mv","xp_pct_mv","fg_pct_mv","week",
                               "precipitation_unknownTRUE"),
                    labels = c("Kicker Team Implied Score","Opposing Team Implied Score","STD Kicker Avg Points Allowed by Opposing Team",
                               "Wind","Dome Stadium","Stadium Retractable Roof Closed",
                               "STD Kicker Avg Points","Stadium Retractable Roof Open","Season",
                               "Precipitation Reported","Kicker Home Game","STD Kicker Team 4th Down Try Rate",
                               "STD Max Kick Distance Attempted","STD Extra Point Success (%)","STD FG Success (%)","Week of Season",
                               "Precipitation Data Missing"))) %>%
  mutate(Variable = fct_rev(Variable)) %>% 
  arrange(desc(Variable)) %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  theme_minimal() + 
  scale_x_continuous(expand = c(0, 0)) +
  #scale_fill_viridis(option = "mako") + 
  labs(y = NULL, title = "Ridge Regression - Variable Importance",
       caption = "STD = Season-to-Date, FG = Field Goal")
viplot

### Random forest

rf_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance = "permutation")

rf_wf <- workflow() %>%
  add_recipe(simple_prep_rec) %>%
  add_model(rf_spec)

bayes_param <- rf_wf %>% 
  extract_parameter_set_dials() %>% 
  update(mtry = finalize(mtry(), X))

set.seed(345)
starttime=Sys.time()
starttime
rf_res <- tune_bayes(
  rf_wf,
  resamples = XY_folds,
  control=ctrl_grid,
  param_info = bayes_param,
  metrics=rmse_vals,
  initial = 18, iter = 18
)
Sys.time()-starttime

best_rmse <- select_best(rf_res, "rmse")

final_rf <- finalize_workflow(
  rf_wf,
  best_rmse
)

library(vip)

final_rf %>%
fit(data = XY) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")

rf_final <- final_rf %>%
  fit(data = XY)

predict(rf_final,as.data.frame(X)) %>% 
  ggplot(aes(x=.pred,y=Y)) + geom_hex() + geom_smooth(color="red") + xlim(c(-2,25)) +
  geom_abline(linetype = 2) + 
  labs(title="Random Forest Regression",subtitle="Train Data")

rf_rmse_vals_train <- predict(rf_final,as.data.frame(X)) %>% 
  rmse_vals(Y,.pred) %>% 
  mutate(model = "rf")
rf_rmse_vals_train

rf_rmse_vals_test <- predict(rf_final,as.data.frame(X_test)) %>% 
  rmse_vals(Y_test,.pred) %>% 
  mutate(model = "rf")
rf_rmse_vals_test

predict(rf_final,as.data.frame(X_test)) %>% 
  ggplot(aes(x=.pred,y=Y_test)) + geom_hex() + geom_smooth(color="red") + xlim(c(-2,25)) +
  geom_abline(linetype = 2) + 
  labs(title="Random Forest Regression",subtitle="Test Data")

### KNN

knn_spec <- nearest_neighbor(
  neighbors = tune(),
  weight_func = tune(),
  dist_power = tune()                       ## step size
) %>%
  set_engine("kknn") %>%
  set_mode("regression")

knn_grid <- grid_latin_hypercube(
  neighbors(),
  weight_func(),
  dist_power(),
  size = 15
)

knn_wf <- workflow() %>%
  add_recipe(norm_prep_rec) %>% 
  add_model(knn_spec)

doParallel::registerDoParallel()

starttime=Sys.time()
set.seed(890)
knn_res <- tune_grid(
  knn_wf,
  resamples = XY_folds,
  grid = knn_grid,
  metrics=rmse_vals,
  control = ctrl_lh
)
Sys.time()-starttime
collect_metrics(knn_res)

show_best(knn_res, "mae")

best_rmse <- select_best(knn_res, "rmse")

final_knn <- finalize_workflow(
  knn_wf,
  best_rmse
)

final_knn

library(vip)

final_knn %>%
  fit(data = XY) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")

knn_final <- final_knn %>%
  fit(data = XY_clust)

knn_rmse_vals_train <- predict(knn_final,as.data.frame(X)) %>% 
  rmse_vals(Y,.pred) %>% 
  mutate(model = "knn")
knn_rmse_vals_train

knn_rmse_vals_test<- predict(knn_final,as.data.frame(X_test)) %>% 
  rmse_vals(Y_test,.pred) %>% 
  mutate(model = "knn")
knn_rmse_vals_test

predict(knn_final,as.data.frame(X)) %>%
  ggplot(aes(x = .pred, y = Y)) +
  geom_point(size = 1.5, color = "midnightblue") +
  geom_smooth(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  ) + xlim(-5,25) + ylim(-5,25) + labs(title="KNN",subtitle="Training Data")

predict(knn_final,as.data.frame(X_test)) %>%
  ggplot(aes(x = .pred, y = Y_test)) +
  geom_point(size = 1.5, color = "midnightblue") +
  geom_smooth(method="gam",
    lty = 2, alpha = 0.5,
    color = "red",
    size = 1.2
  ) + xlim(-5,25) + ylim(-5,25) + labs(title="KNN",subtitle="Test Data")

### wanna try a bagmlp?
#CROSSVALIDATED 
library(baguette)

bagmlp_spec <- bag_mlp(
  hidden_units = tune(),
  penalty = tune(),
  epochs = tune()                         ## step size
) %>%
  set_engine("nnet",times=10) %>%
  set_mode("regression")

bagmlp_grid <- grid_latin_hypercube(
  hidden_units(),
  penalty(),
  epochs(),
  size = 30
)

bagmlp_wf <- workflow() %>%
  add_recipe(norm_prep_rec) %>% 
  add_model(bagmlp_spec)

doParallel::registerDoParallel()

set.seed(888)
starttime=Sys.time()
bagmlp_res <- tune_bayes(
  bagmlp_wf,
  resamples = XY_folds,
  initial=5,iter=5,
  control=ctrl_grid,
  metrics=rmse_vals
)
Sys.time()-starttime

collect_metrics(bagmlp_res)

show_best(bagmlp_res, "mae")

best_rmse <- select_best(bagmlp_res, "rmse")

final_bagmlp <- finalize_workflow(
  bagmlp_wf,
  best_rmse
)

final_bagmlp

library(vip)

final_bagmlp %>%
  fit(data = XY_clust) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")

bagmlp_final <- final_bagmlp %>%
  fit(data = XY_clust)

bagmlp_rmse_vals_train <- predict(bagmlp_final,as.data.frame(X)) %>% 
  rmse_vals(Y,.pred) %>% 
  mutate(model = "bagmlp")
bagmlp_rmse_vals_train

predict(bagmlp_final,as.data.frame(X)) %>% 
  ggplot(aes(y=Y,x=.pred)) + geom_point() + geom_smooth() + xlim(c(-2,25)) + 
  labs(title="Bagged MLP",subtitle = "Training Data")

bagmlp_rmse_vals_test <- predict(bagmlp_final,as.data.frame(X_test)) %>% 
  rmse_vals(Y_test,.pred) %>% 
  mutate(model = "bagmlp")
bagmlp_rmse_vals_test

predict(bagmlp_final,as.data.frame(X_test)) %>% 
  ggplot(aes(y=Y_test,x=.pred)) + geom_hex() + geom_smooth(color="red") + xlim(c(-2,25)) + 
  labs(title="Bagged MLP",subtitle = "Test Data")

### STACKS 

metric <- metric_set(rmse)

my_stack_data_st <- 
  stacks() %>%
  #add_candidates(knn_res) %>%
  add_candidates(glmn_res) %>% 
  add_candidates(ridge_res) %>% 
  add_candidates(bagmlp_res) %>%
  add_candidates(svmrbf_res) %>% 
  #add_candidates(rf_res) %>% 
  add_candidates(xgb_res)

my_stack_model_st <-
  my_stack_data_st %>%
  blend_predictions(penalty = 10^seq(-2, -0.5, length = 20),
                    metric=rmse_vals)

autoplot(my_stack_model_st)

autoplot(my_stack_model_st, type = "weights")

my_stack_model_st <-
  my_stack_model_st %>%
  fit_members()

collect_parameters(my_stack_model_st)

my_stack_train <-
  XY %>% 
  bind_cols(predict(my_stack_model_st, .))

stack_rmse_vals_train <- my_stack_train %>% 
  rmse_vals(Y,.pred) %>% 
  mutate(model = "stack")
stack_rmse_vals_train

my_stack_train %>% ggplot(aes(x=.pred,y=Y)) + geom_point() + geom_smooth() +
  xlim(-2,25) + labs(title="Model Stack", subtitle = "Training Data")

my_stack_test <- 
  XY_test %>%
  bind_cols(predict(my_stack_model_st, .))
  
stack_rmse_vals_test <- my_stack_test %>% 
  rmse_vals(Y_test,.pred) %>% 
  mutate(model = "stack")
stack_rmse_vals_test

my_stack_test %>% ggplot(aes(x=.pred,y=Y_test)) + geom_hex() + 
  geom_smooth(color = "red") +
  xlim(-2,25) + labs(title="Model Stack", subtitle = "Test Data")

my_stack_test %>% 
  ggplot(aes(x=week,y=abs(.pred-Y_test),group=week)) + geom_boxplot()

my_stack_test %>% 
  ggplot(aes(x=week,y=(.pred-Y_test),group=week)) + geom_boxplot()

my_stack_test %>% 
  ggplot(aes(x=Y_test)) + geom_histogram()

### COMBINE RESULTS

rmse_vals_train_all <- svm_rmse_vals_train %>% 
  bind_rows(xgb_rmse_vals_train,
            glmn_rmse_vals_train,
            ridge_rmse_vals_train,
            rf_rmse_vals_train,
            knn_rmse_vals_train,
            bagmlp_rmse_vals_train,
            stack_rmse_vals_train) %>% 
  select(-.estimator) %>% 
  pivot_wider(names_from=.metric,names_prefix="train_",values_from=.estimate)

rmse_vals_test_all <- svm_rmse_vals_test %>% 
  bind_rows(xgb_rmse_vals_test,
            glmn_rmse_vals_test,
            ridge_rmse_vals_test,
            rf_rmse_vals_test,
            knn_rmse_vals_test,
            bagmlp_rmse_vals_test,
            stack_rmse_vals_test) %>% 
  select(-.estimator) %>% 
  pivot_wider(names_from=.metric,names_prefix="test_",values_from=.estimate)

rmse_vals_all <- rmse_vals_train_all %>% 
  left_join(rmse_vals_test_all,by="model")

rmse_vals_all %>% 
  select(model,train_rmse,test_rmse) %>% 
  mutate(train_rmse = round(train_rmse,3),test_rmse = round(test_rmse,3),
         model = factor(model,
                        levels=c("svm","xgb","glmn","ridge",
                                 "rf","knn","bagmlp",
                                 "stack"),
                        labels=c("SVM","XGBoost","Elastic Net","Ridge",
                                          "Random Forest","KNN","Bagged MLP",
                                          "Stacked"))) %>% 
  gt() %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>% 
  tab_header(
    title = "Model Fit Statistics",
    subtitle = "Training Set (2019-2022); Test Set (2023)"
  ) %>% 
  tab_spanner(
    label = "RMSE",
    columns = c(train_rmse,test_rmse)
  ) %>% 
  cols_align(
    align = c("left"),
    columns = c(model)
  ) %>% 
  cols_label(
    model = "Model",
    train_rmse = "Training Set",
    test_rmse = "Test Set"
  ) %>% 
  tab_footnote(
    footnote = "*Excludes last week of each season"
  ) %>% 
  tab_footnote(
    footnote = "Stacked model combines: Bagged MLP (2 members), XGBoost (1), 
    Ridge Regression (1)",
locations =cells_body(
    columns = model,
    rows = (model == "Stacked"))) %>% 
  tab_source_note(source_note = md(
    "Data from the *NFLverse/NFLreadR* R packages"
  )) %>% 
  gtsave("rmse_kicker_models.png")

# s <- chromote::default_chromote_object() #get the f object
# s$close()

### COMPARE TO ESPN/NFL

path="..\\kickerproj"
file_list = list.files("./kickerproj")
kicker_proj = readr::read_csv(paste0(getwd(),"/kickerproj/",file_list), id = "file_name") %>% 
filter(!is.na(Player),FPTS != 0.0) %>% 
  separate(Player,c('First_Name', 'Last_Name'),sep=" ") %>% 
  mutate(week=as.numeric(str_extract(file_name,"\\d+")),
         kicker_name = paste0(str_sub(First_Name,1,1),".",Last_Name),
         Team = clean_team_abbrs(Team),
         Team = ifelse(kicker_name == "R.Patterson" & week < 17, "DET",Team),
         season = 2023) %>% 
  arrange(week,Team) %>% 
  select(season,week,Team,kicker_name,FPTS) %>% 
  rename(fpts_site = FPTS)

kicker_proj_all <- kicker_proj %>%
  full_join(kicker_proj2,by=c("season","week","Team","kicker_name"))

median(final_train$total_points)
mean(final_train$total_points)

real_and_proj <- final_test %>% 
  inner_join(kicker_proj,by=c("season","week","posteam"="Team","kicker_name")) %>% 
  mutate_at(c("roof_dome", "roof_open","roof_closed", 
              "precipitation_unknown","precipitation_yes"),as.logical) 
real_and_proj_y <- real_and_proj %>% 
  pull(total_points)

real_and_proj_m <- model.matrix(formula(paste0("~", paste0(c(0,c(Cov_ma_small,
                                                                 "fpts_site","total_points")), 
                                                           collapse="+"))), data=real_and_proj)
real_and_proj_m <- real_and_proj_m[,colnames(X) != "roof_closedFALSE"]

library(viridis)
## FIGURE 1A
#kicker mean
kicker_cm_plot <- ggplot(data=real_and_proj,aes(real_and_proj,x=szn_kkr_mean_pts,y=total_points)) + 
  geom_hex(binwidth=1) +geom_smooth(color="red") +xlim(-2,25) + geom_abline(linetype=2) + 
  scale_fill_viridis() + labs(title = "(A) Kicker-Centered Estimate", x="Kicker Cumulative Average",y="True Fantasy Points") + 
  theme_minimal()
kicker_cm_plot

kicker_cm_rmse <- real_and_proj %>% ungroup() %>% rmse_vals(total_points,szn_kkr_mean_pts) %>% 
  mutate(model="kkr_cm")
kicker_cm_rmse

## FIGURE 1B
#kicker defense mean
kicker_def_plot <- ggplot(data=real_and_proj,aes(real_and_proj,x=szn_def_kkr_mean_pts,y=total_points)) + 
  geom_hex(binwidth=1) +geom_smooth(color="red") +xlim(-2,25) + geom_abline(linetype=2) + 
  scale_fill_viridis() + labs(title = "(B) Defense-Centered Estimate", x="Defense Cumulative Average vs. Kickers",y="True Fantasy Points") + 
  theme_minimal()
kicker_def_plot
  
kicker_def_cm_rmse <- real_and_proj %>% ungroup() %>% rmse_vals(total_points,szn_def_kkr_mean_pts) %>% 
  mutate(model="kkr_def_cm")
kicker_def_cm_rmse

#peek at how 
cor(real_and_proj$fpts_site,real_and_proj$total_points,use="pairwise.complete.obs")
ggplot(data=real_and_proj,aes(real_and_proj,x=fpts_site,y=fpts_indep)) + geom_point() +geom_smooth() +xlim(-2,25)

ggplot(data=real_and_proj,aes(real_and_proj,x=fpts_site,y=total_points)) + geom_hex() + 
  scale_fill_viridis() + geom_smooth(color="red") +xlim(-2,25) + geom_abline(linetype=2)

## FIGURE 1C
#Site Projections Plot
site_proj_plot <- real_and_proj %>% 
ggplot(aes(real_and_proj,x=fpts_site,y=total_points)) + geom_hex(binwidth=1) + 
  geom_smooth(color="red") +xlim(-2,25) + geom_abline(linetype=2) + theme_minimal() +
  scale_fill_viridis() + labs(title="(C) Published Projections vs. True Fantasy Points",x="Site Projections",y="Fantasy Points")
site_proj_plot

fptssite_test_rmse <- real_and_proj %>% 
  rmse_vals(total_points,fpts_site) %>% 
  mutate(model = "site proj")
fptssite_test_rmse

kkr_premodel_rmse <- kicker_cm_rmse %>% 
  bind_rows(kicker_def_cm_rmse,
            fptssite_test_rmse) %>% 
  select(-.estimator) %>% 
  pivot_wider(names_from=.metric,names_prefix="test_",values_from=.estimate)

## Figure  
kkr_premodel_rmse %>% 
  select(model,test_rmse) %>% 
  mutate(test_rmse = round(test_rmse,3),
         model = factor(model,
                        levels=c("kkr_cm","kkr_def_cm","site proj"),
                        labels=c("Kicker Cumulative Mean",
                                 "Kicker Opposition Cumulative Mean",
                                 "Fantasy Site Projections"))) %>% 
  gt() %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>% 
  tab_header(
    title = "Model Fit Statistics",
    subtitle = "2023 Season"
  ) %>% 
  cols_align(
    align = c("left"),
    columns = c(model)
  ) %>% 
  cols_label(
    model = "Model",
    test_rmse = "RMSE"
  ) %>% 
  tab_footnote(
    footnote = "*Excludes last week of season; Values imputed for Week 1 and when missing kicker data"
  ) %>% 
  tab_source_note(source_note = md(
    "Data from the *NFLverse/NFLreadR* R packages,
    Projections Aggregated From ESPN & CBS Sports by FantasyPros"
  )) %>% 
  gtsave("rmse_premodel_proj.png") 

stacked_test <- 
  real_and_proj_m %>%
  predict(my_stack_model_st, .) %>% 
  bind_cols(real_and_proj_m)  
  
stacked_test_rmse <- 
  stacked_test %>% 
  rmse_vals(total_points,.pred) %>% 
  mutate(model = 'stack')
stacked_test_rmse

stacked_test %>% 
  ggplot(aes(x=.pred,y=total_points)) + geom_point() + geom_smooth() + xlim(-2,25)

member_preds <- 
  my_stack_test %>%
  select(total_points) %>%
  bind_cols(predict(my_stack_model_st, my_stack_test, members = TRUE))

stack_comp <- map(member_preds, rmse_vec, truth = member_preds$total_points) %>%
  as_tibble()

#indivs 

ridge_test <- 
  real_and_proj_m %>%
  predict(ridge_final, .) %>% 
  bind_cols(real_and_proj_m)  

ridge_test_rmse <- ridge_test %>% 
  rmse_vals(total_points,.pred) %>% 
  mutate(model = 'ridge')
ridge_test_rmse

## Figure 2
# Ridge Regression Estimates vs. Actual Points
ridge_vs_true <- ridge_test %>% 
  ggplot(aes(x=.pred,y=total_points)) + geom_point(alpha=.2,color="darkslateblue") + 
  geom_smooth(color="red") + 
  geom_abline(linetype=2) + 
  xlim(-2,25) + labs(x="Ridge Regression Projections",y="True Fantasy Points") + theme_minimal()
ridge_vs_true

glmn_test <- 
  real_and_proj_m %>%
  predict(glmn_final, .) %>% 
  bind_cols(real_and_proj_m) 

glmn_test_rmse <- glmn_test %>% rmse_vals(total_points,.pred) %>% 
  mutate(model = 'glmn')
glmn_test_rmse

glmn_test %>% 
  ggplot(aes(x=.pred,y=total_points)) + geom_point(alpha=.2,color="darkslateblue") + geom_smooth(color="red") + 
  geom_abline(linetype=2) + 
  xlim(-2,25) + labs(x="XGB projections",y="True Fantasy Points") + theme_minimal()

svm_test <- 
  real_and_proj_m %>%
  predict(svm_final, .) %>% 
  bind_cols(real_and_proj_m) 

svm_test_rmse <- svm_test %>% rmse_vals(total_points,.pred) %>% 
  mutate(model = 'svm')
svm_test_rmse

svm_test %>% 
  ggplot(aes(x=.pred,y=total_points)) + geom_hex() + geom_smooth(color="red") + 
  geom_abline(linetype=2) + scale_fill_viridis() + 
  xlim(-2,25) + labs(x="SVM Projections",y="True Fantasy Points") + theme_minimal()

rf_test <- 
  real_and_proj_m %>%
  predict(rf_final, .) %>% 
  bind_cols(real_and_proj_m) 

rf_test_rmse <- rf_test %>% rmse_vals(total_points,.pred) %>% 
  mutate(model = 'rf')
rf_test_rmse

rf_test %>% 
  ggplot(aes(x=.pred,y=total_points)) + geom_hex() + geom_smooth(color="red") + 
  geom_abline(linetype=2) + scale_fill_viridis() + 
  xlim(-2,25) + labs(x="RF projections",y="True Fantasy Points") + theme_minimal()

xgb_test <- 
  real_and_proj_m %>%
  predict(xgb_final, .) %>% 
  bind_cols(real_and_proj_m) 

xgb_test_rmse <- xgb_test %>% 
  rmse_vals(total_points,.pred) %>% 
  mutate(model = 'xgb')
xgb_test_rmse

xgb_test %>% 
  ggplot(aes(x=.pred,y=total_points)) + geom_hex() + geom_smooth(color="red") +
  geom_abline(linetype=2) + scale_fill_viridis() + 
  xlim(-2,25) + labs(x="XGB Projections",y="True Fantasy Points") + theme_minimal()

bagmlp_test <- 
  real_and_proj_m %>%
  predict(bagmlp_final, .) %>% 
  bind_cols(real_and_proj_m) 

bagmlp_test_rmse <- bagmlp_test %>% rmse_vals(total_points,.pred) %>% 
  mutate(model = 'bagmlp')
bagmlp_test_rmse

bagmlp_test %>% 
  ggplot(aes(x=.pred,y=total_points)) + geom_hex() + geom_smooth(color="red") + 
  geom_abline(linetype=2) + scale_fill_viridis() + 
  xlim(-2,25) + labs(x="XGB Projections",y="True Fantasy Points") + theme_minimal()

kknn_test <- 
  real_and_proj_m %>%
  predict(knn_final, .) %>% 
  bind_cols(real_and_proj_m) 

kknn_test_rmse <- kknn_test %>% rmse_vals(total_points,.pred) %>% mutate(model="knn") 
kknn_test_rmse

kknn_test %>% 
  ggplot(aes(x=.pred,y=total_points)) + geom_hex() + geom_smooth(color="red") + 
  xlim(-2,25) + geom_abline(linetype=2) + scale_fill_viridis() + theme_minimal() + 
  labs(x="XGB Projections",y="True Fantasy Points")

test_all_rmse <- svm_test_rmse %>% 
  bind_rows(xgb_test_rmse,
            glmn_test_rmse,
            ridge_test_rmse,
            rf_test_rmse,
            kknn_test_rmse,
            bagmlp_test_rmse,
            stacked_test_rmse,
            fptssite_test_rmse) %>% 
  select(-.estimator) %>% 
  pivot_wider(names_from=.metric,names_prefix="test_",values_from=.estimate)

## TABLE 3. MODEL FIT STATISTICS: TEST SET
test_proj_all_rmse <- test_all_rmse %>% 
  select(model,test_rmse) %>% 
  mutate(test_rmse = round(test_rmse,3),
         model = factor(model,
                        levels=c("svm","xgb","glmn","ridge",
                                 "rf","knn","bagmlp",
                                 "stack","site proj"),
                        labels=c("SVM","XGBoost","Elastic Net","Ridge",
                                 "Random Forest","KNN","Bagged MLP",
                                 "Stacked","Fantasy League Projections"))) %>% 
  gt() %>% 
  tab_header(
    title = "Model Fit Statistics",
    subtitle = "Test Set (2023)"
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>% 
  tab_spanner(
    label = "RMSE",
    columns = c(test_rmse)
  ) %>% 
  cols_align(
    align = c("left"),
    columns = c(model)
  ) %>% 
  cols_label(
    model = "Model",
    test_rmse = "Test Set"
  ) %>% 
  tab_footnote(
    footnote = "*Excludes last week of each season"
  ) %>% 
  tab_source_note(source_note = md(
    "Data from the *NFLverse/NFLreadR* R packages"
  )) %>% 
  gtsave("rmse_proj_kicker_models.png")

# TIDYPOSTERIOR

bagmlp_rmse <- 
  bagmlp_final %>% 
  fit_resamples(XY_folds) %>% 
  collect_metrics(bagmlp_res, summarize = FALSE) %>% 
  dplyr::filter(.metric == "rmse") %>% 
  dplyr::select(id, bagmlp = .estimate)

xgb_rmse <- 
  xgb_final %>% 
  fit_resamples(XY_folds) %>% 
  collect_metrics(xgb_res, summarize = FALSE) %>% 
  dplyr::filter(.metric == "rmse") %>% 
  dplyr::select(id, xgb = .estimate)

stack_rmse <-
  my_stack_model_st %>% 
  predict(XY_folds)

resamples_df <- full_join(xgb_rmse, bagmlp_rmse, by = "id")
resamples_df

set.seed(101)
rmse_model_via_df <- perf_mod(resamples_df, iter = 2000)

rmse_model_via_df %>% 
  tidy() %>% 
  ggplot(aes(x = posterior)) + 
  geom_histogram(bins = 40, col = "blue", fill = "blue", alpha = .4) + 
  facet_wrap(~ model, ncol = 1) + 
  xlab("RMSE")

contrast_models(rmse_model_via_df, seed = sample.int(10000, 1)) %>% 
  summary() 

rmse_model_via_df %>% autoplot()

### Extra descriptives for webppage

final_avg <- final_data %>% 
  group_by(kicker_name,kicker_id,season) %>% 
  mutate(games_kicked = n()) %>% 
  slice(c(n())) %>% 
  select(season,week,game_id,kicker_name,kicker_id,games_kicked,fg_pct_mv,xp_pct_mv) %>% 
  filter(games_kicked > 5) %>% 
  ungroup() %>% 
  summarize(fg_pct_mv_mean = mean(fg_pct_mv),fg_pct_mv_sd = sd(fg_pct_mv),
            xp_pct_mv_median=median(xp_pct_mv),xp_pct_mv_iqr = IQR(xp_pct_mv))

### EXTRA NON-TIDYMODEL FUN
# START GRF MODELS

#original 5000 trees
Y.forest = regression_forest(X, Y, num.trees=20000,clusters=cluster_id[1:dim(X)[1]],tune.parameters="all",seed=888)
Y.hat = predict(Y.forest)$predictions

Y_and_Yhat <- bind_cols(Y=Y,Y.hat=Y.hat) 
#mutate(res = Y.hat-Y, sqres = res*res)

ggplot(data=Y_and_Yhat,aes(x=Y.hat,y=Y)) + geom_point() +
  geom_smooth() + xlim(-5,30) + ylim(-5,30)

summary(Y.forest)
variable_importance(Y.forest)
r.pred <- predict(Y.forest, estimate.variance=TRUE)
standard.error = sqrt(r.pred$variance.estimates)
Y.forest

test_pred <- predict(Y.forest,X_test,clusters=posteam_seas)

#fit metrics
test_pred_comp <- bind_cols(Y_2023=Y_test,Y_pred=test_pred$prediction) 

ggplot(data=test_pred_comp,aes(x=Y_pred,y=Y_2023)) + geom_point() + geom_smooth() +
  xlim(0,25)

test_pred_comp %>% rmse_vals(as.numeric(Y_2023),as.numeric(Y_pred))
#1 rmse    standard        4.59
test_pred_comp %>% rsq(Y_test,Y_pred)
#1 rsq     standard     0.00301
test_pred_comp %>% mae(Y_test,Y_pred)
#1 mae     standard        3.59

#compare to mean of Y_2023 only
test_pred_comp %>% rmse(as.numeric(Y_2023),rep(mean(Y_2023),length(Y_2023)))
test_pred_comp %>% rsq(Y_2023,Y_pred)
test_pred_comp %>% mae(Y_2023,rep(mean(Y_2023),length(Y_2023)))
#1 mae     standard        3.55

#compare to team_season means
XY_2023 <- final_test_2023 %>% 
  group_by(posteam) %>% 
  mutate(Y_2023_teammean = mean(total_points,na.rm=T))

test_pred_comp %>% rmse(as.numeric(Y_2023),XY_2023$Y_2023_teammean)
test_pred_comp %>% rsq(as.numeric(Y_2023),XY_2023$Y_2023_teammean)
test_pred_comp %>% mae(as.numeric(Y_2023),XY_2023$Y_2023_teammean)
#1 mae     standard        3.47

#compare to running means
test_pred_comp %>% rmse(as.numeric(Y_2023),final_test$szn_kkr_mean_pts)
test_pred_comp %>% rsq(as.numeric(Y_2023),final_test$szn_kkr_mean_pts)
test_pred_comp %>% mae(as.numeric(Y_2023),final_test$szn_kkr_mean_pts)
#1 mae     standard        3.94

#compare to running def means
test_pred_comp %>% rmse(as.numeric(Y_2023),final_test$szn_def_kkr_mean_pts)
test_pred_comp %>% rsq(as.numeric(Y_2023),final_test$szn_def_kkr_mean_pts)
test_pred_comp %>% mae(as.numeric(Y_2023),final_test$szn_def_kkr_mean_pts)
#1 mae     standard        3.94


# ggplot(data=test_pred_comp,aes(x=Y_pred,y=Y_2023)) + geom_point() +
#   geom_smooth() + xlim(-5,30) + ylim(-5,30)

### pared down model
varimp <- variable_importance(Y.forest)
selected.idx = which ( varimp > mean ( varimp ))

Y.forest = regression_forest(X, Y, num.trees=20000,clusters=cluster_id[1:dim(X)[1]],
                             tune.parameters="all",seed=888)
Y.hat = predict(Y.forest)$predictions

Y.forest.small = regression_forest(X[, selected.idx],Y,
                                   clusters = cluster_id[1:dim(X)[1]],
                                   tune.parameters = "all" , seed=888,num.trees=20000)

summary(Y.forest.small)
variable_importance(Y.forest.small)

Y.hat = predict(Y.forest.small)$predictions

test_pred.small <- predict(Y.forest.small,X_test[, selected.idx],clusters=posteam_seas[cluster_id[(dim(X)[1]+1)]:length(cluster_id)])
test_pred_comp <- bind_cols(Y_2023=Y_test,Y_pred=test_pred.small$prediction) 
test_pred_comp %>% rmse_vals(Y_2023,Y_pred)

test_pred_comp %>% rmse_vals(as.numeric(Y_2023),rep(mean(Y_2023),length(Y_2023)))
#1 rmse    standard        4.59
test_pred_comp %>% rsq(Y_2023,Y_pred)
#1 rsq     standard    0.000529
test_pred_comp %>% mae(Y_2023,rep(mean(Y_2023),length(Y_2023)))
#1 mae     standard        3.60

### LLR
c.forest.ll <- ll_regression_forest(X = X, 
                                    Y = Y, 
                                    enable.ll.split = TRUE,
                                    num.trees = 20000,
                                    # tune.num.trees = 100,
                                    # tune.num.reps = 200,
                                    # tune.num.draws = 2000,
                                    clusters=cluster_id[1:dim(X)[1]],
                                    seed = 888) 

ll_preds <- predict(c.forest.ll)
plot(x = ll_preds$predictions, y = Y)

summary(c.forest.ll)
variable_importance(c.forest.ll)

ll_preds_2023 <- predict(c.forest.ll,X_test)
plot(x = ll_preds_2023$predictions, y = Y_test)

test_pred_comp <- bind_cols(Y_2023=Y_test,Y_pred=ll_preds_2023$prediction)

test_pred_comp %>% rmse_vals(as.numeric(Y_2023),as.numeric(Y_pred))
#1 rmse    standard        4.57
test_pred_comp %>% rsq(Y_2023,Y_pred)
#1 rsq     standard     0.00875
test_pred_comp %>% mae(Y_2023,Y_pred)
#1 mae     standard        3.58

ggplot(data=test_pred_comp,aes(x=Y_pred,y=Y_2023)) + geom_point() + geom_smooth() +
  xlim(0,25)


### SCRATCH

levis <- pbp %>% filter(game_stadium %in% c("Levi's Stadium","Empower Field at Mile High",
                                            "Sports Authority Field at Mile High",
                                            "CenturyLink Field","Lumen Field"),
                        season < 2023)

levis2 <- load_pbp(2023) %>% 
  filter(game_stadium %in% c("Levi's Stadium","Empower Field at Mile High",
                             "Sports Authority Field at Mile High",
                             "CenturyLink Field","Lumen Field"))

#ws_pattern <-  '(?<=Wind: [A-Za-z,\\\\]{0,20}\\ )[0-9]{0,2}'
ws_pattern <- "(?<=Wind:\\D{0,30})\\d{1,2}"
#ws_pattern <-  '(?<=Wind: [\\d]{0,20}\\ )[0-9]{0,2}'

#weather_pattern <- '^[A-Za-z\\s]+(?= Temp)'
weather_pattern <- '^[:print:]+(?= Temp)'

weathered <- levis %>% 
  bind_rows(levis2) %>% 
  mutate(wind_weather = str_extract(weather,ws_pattern),
         weather_desc = str_extract(weather,weather_pattern)) %>% 
  #fix calm days to have zero wind
  mutate(wind_weather = ifelse(str_detect(weather,"Calm"),"0",wind_weather)) %>% 
  mutate(weather_desc = ifelse(str_detect(weather_desc,"(N|n)/(A|a)")==TRUE,NA,weather_desc)) %>% 
  mutate(precipitation = case_when(str_detect(weather_desc,"Chance|Change")==TRUE ~ "maybe",
                                   str_detect(weather_desc,"(R|r)ain|(S|s)now|(S|s)howers|(D|d)rizzle")==TRUE ~ "yes",
                                   is.na(weather_desc) ~ NA,
                                   TRUE ~ "no")) %>% 
  mutate(precipitation = ifelse(roof %in% c("dome","closed") & is.na(precipitation),"no",precipitation),
         wind_weather = ifelse(roof %in% c("dome","closed") & is.na(wind_weather),0,wind_weather)
         ) %>%
  #fix stray games with missing wind_weather
  mutate(precipitation = ifelse(game_id=="2022_21_CIN_KC","no",precipitation),
         wind_weather = ifelse(game_id=="2022_21_CIN_KC",13,wind_weather),
         wind_weather = ifelse(game_id=="2021_13_TB_ATL",0,wind_weather),
         roof = ifelse(game_id=="2021_13_TB_ATL","closed",roof),
         precipitation = ifelse(game_id=="2023_02_NYJ_DAL","no",precipitation),
         wind_weather = ifelse(game_id=="2023_02_NYJ_DAL",0,wind_weather),
         roof = ifelse(game_id=="2023_02_NYJ_DAL","closed",roof),
         precipitation = ifelse(game_id=="2023_03_DAL_ARI","no",precipitation),
         wind_weather = ifelse(game_id=="2023_03_DAL_ARI",0,wind_weather),
         roof = ifelse(game_id=="2023_03_DAL_ARI","closed",roof),
         wind_weather = ifelse(game_id=="2018_06_TB_ATL",5,wind_weather),
         wind_weather = ifelse(game_id=="2018_11_KC_LA",0,wind_weather),
         wind_weather = ifelse(game_id=="2019_11_CHI_LA",0,wind_weather),
         wind_weather = ifelse(game_id=="2017_10_HOU_LA",0,wind_weather),
         wind_weather = ifelse(game_id=="2017_14_PHI_LA",0,wind_weather)) %>% 
  mutate(wind_weather = ifelse(is.na(wind_weather) & home_team=="ATL",4.36,wind_weather)) %>% 
  #for precipitation, there's a fair number missing, so we'll do unk
  mutate(precipitation = ifelse(is.na(precipitation),"unknown",precipitation),
         wind_weather = as.numeric(wind_weather)) %>% 
  filter(season > 2014) %>% 
  count(game_id,weather,weather_desc,wind,wind_weather,roof,precipitation,
        game_stadium) %>% 
  mutate(game_stadium = ifelse(game_stadium %in% c("Empower Field at Mile High",
                               "Sports Authority Field at Mile High"),"Mile High",
                          game_stadium),
         game_stadium = ifelse(game_stadium %in% c("CenturyLink Field","Lumen Field"),
                               "Seahawks Stadium",game_stadium)) 

weathered %>% ungroup() %>% 
    count(game_stadium,precipitation)

ww <- weathered %>% 
  select(game_id,weather,weather_desc,wind,wind_weather,roof)
#check for precipitation descriptors
#unique(ww$weather_desc)
nowind_weather <- ww %>% 
  filter(is.na(wind_weather)) %>% 
  count(game_id,weather,roof)

noweather <- weathered %>% 
  filter(is.na(weather_desc)) %>% 
  count(game_id,weather,roof)
