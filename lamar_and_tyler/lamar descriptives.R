#dataset is a subset of data available from nflverse, with a good amount of cleaning
#the participation data linked with the play-by-play should get you most of what
#this has to provide

library(tidyverse)
library(gt)
library(gtsummary)
library(labelled)

qb_dataset_exp <- read.csv("qb_descriptive_data.csv")

#labeled categorical/dichotomous variables
qb_dataset_exp %>% 
  mutate(down_char=factor(ifelse(down==5,"2-Pt Conversion",as.character(down)),levels=
                            c("1","2","3","4","2-Pt Conversion")),
         lamarsnap = factor(lamarsnap,levels=c("Jackson","Huntley")),
         blkr_semiblkr=str_replace(blkr_semiblkr,"_","/")) %>% 
  set_variable_labels(
    down_char = "Down",
    N_passdef = "Number of Defensive Backs Fielded by Defense",
    blkr_semiblkr = "Number of Offensive Linemen & Tight Ends/Running Backs Fielded by Offense",
    posteam_type     = "Home/Away",
    no_huddle = "No Huddle",
    shotgun = "Shotgun Formation",
    play_cats = "Play Type",
    offense_formation = "Offensive Formation",
    defenders_in_box = "Defenders in Box"
  ) %>% 
  tbl_summary(
    by = 'lamarsnap',
    include = c(posteam_type,down_char,play_cats,no_huddle,shotgun,offense_formation,blkr_semiblkr,defenders_in_box,N_passdef),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_dichotomous() ~ "{p}%"),
    digits = list(everything() ~ 0)
  ) %>% 
  as_gt() %>% 
  tab_header(
    title = md("Table 1. Binary/Categorical Variables")
    #    subtitle = md("`gtcars` is an R dataset")
  ) %>% 
  opt_align_table_header(align = "left")  

#labeled continuous variables
qb_dataset_exp %>% 
  mutate(lamarsnap = factor(lamarsnap,levels=c("Jackson","Huntley"))) %>% 
  set_variable_labels(
    ydstogo = "Yards to 1st Down",
    yardline_100 = "Yards to End Zone",
    game_seconds_remaining = "Time Remaining (sec)",
    xpass = "Pass Likelihood (%)",
    wp = "Win Likelihood (%)",
    qb_epa     = "QB Expected Points Added"
  ) %>% 
  tbl_summary(
    by = 'lamarsnap',
    include = c(ydstogo,yardline_100,game_seconds_remaining, xpass,wp,qb_epa),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_dichotomous() ~ "{p}%"),
    digits = list(everything() ~ 2)
  ) %>%  
  as_gt() %>% 
  tab_header(
    title = md("Table 2. Continuous Variables")
    #    subtitle = md("`gtcars` is an R dataset")
  ) %>% 
  opt_align_table_header(align = "left")  
