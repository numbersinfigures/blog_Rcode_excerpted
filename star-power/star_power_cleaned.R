library(tidyverse)
library(CausalImpact)

## Data and setup

setwd("C:\\Users\\chknd\\Documents\\anatole\\numsinfigs\\posts\\shooting-stars")
addTaskCallback(function(...) {set.seed(1212);TRUE})

# download data from trends.google.com

#https://trends.google.com/trends/explore/TIMESERIES/1720213200?hl=en-US&tz=420&date=2022-01-02+2024-06-29&geo=US&hl=en&q=WNBA+tickets,MLS+tickets&sni=3 
#saved as ticket_comp.csv

#temporal issues identified: 
#Google Trends methodology change marked at 1/1/2022
#MLS season for 2021 delayed by labor contract negotiations

## WNBA TICKET ANALYSIS

#notes: draft week dates gleaned from google
wnba <- read_csv("ticket_comp.csv",skip=1) %>% 
  mutate(Week=as.Date(Week),
         draftweek = ifelse(Week %in% ymd("2021-04-11","2022-04-10","2023-04-09",
                                          "2024-04-14"),1,0)) %>% 
  rename("trendpoint"= "WNBA tickets: (United States)") %>% 
  filter(Week >= "2022-11-13",Week < "2024-06-30") %>% 
  mutate(indexnum=row_number()) 

wnba_tix <- wnba %>% 
  select(trendpoint,indexnum,draftweek) 

#extract CausalImpact inputs as global for easy of use
trendpoint <- wnba_tix$trendpoint
indexnum <- wnba_tix$indexnum
draftweek <- wnba_tix$draftweek

#mark pre-intervention and post-intervention period
pre.period <-c(1,74)
post.period <- c(75,85)

#create an outcome series with post-intervention as NA per CausalImpact demands
post.period.response <- trendpoint[post.period[1] : post.period[2]]
trendpoint[post.period[1] : post.period[2]] <- NA

#add components to BSTS state specification and create model
ss <- AddAutoAr(list(), trendpoint)
ss <- AddSeasonal(ss, trendpoint, nseasons = 52)
bsts.model <- bsts(formula=trendpoint ~ draftweek, ss, niter = 1000,seed=42)

#run CausalImpact to extract predictions
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
#check outputs
summary(impact)
#create own relatve impact estimates
wnba_Relative_avg = c(value=100*(impact$summary$Actual[1]/impact$summary$Pred[1]-1),
                      LCL=100*((impact$summary$Actual[1]/impact$summary$Pred.upper[1])-1),
                      UCL=100*((impact$summary$Actual[1]/impact$summary$Pred.lower[1])-1))
wnba_Relative_sum = c(value=100*(impact$summary$Actual[2]/impact$summary$Pred[2]-1),
                      LCL=100*((impact$summary$Actual[2]/impact$summary$Pred.upper[2])-1),
                      UCL=100*((impact$summary$Actual[2]/impact$summary$Pred.lower[2])-1))
wnba_Actual_avg = c(impact$summary$Actual[1],NA,NA)
wnba_Actual_sum = c(impact$summary$Actual[2],NA,NA)
wnba_Pred_avg = c(impact$summary$Pred[1],impact$summary$Pred.lower[1],impact$summary$Pred.upper[1])
wnba_Pred_sum = c(impact$summary$Pred[2],impact$summary$Pred.lower[2],impact$summary$Pred.upper[2])
wnba_table <- rbind(wnba_Actual_avg,wnba_Actual_sum,wnba_Pred_avg,wnba_Pred_sum,wnba_Relative_avg,wnba_Relative_sum)
wnba_table

#check other outputs for CausalImpact
summary(impact, "report")
plot(impact)
plot(impact$model$bsts.model)
plot(impact$model$bsts.model, "coefficients")
# plot(impact$model$bsts.model, "components")  #can sometimes cause issues if run 
# more than once

#format table for own plot
model_output <- impact$series %>% 
  bind_cols(Week=wnba$Week)

wnba_tix_plot <- model_output %>% 
  ggplot(data=.,aes(x=Week)) + 
  geom_ribbon(aes(ymin = point.pred.lower, ymax = point.pred.upper,fill="95% CI")) + 
  geom_line(aes(y=point.pred,linetype="pred"),color="darkorange",size=1) + 
  geom_line(aes(y=response,linetype="obs"),color="darkblue",size=1) + 
  geom_vline(xintercept=as.Date("2024-04-14")-.5,linetype="dotted") +   
  scale_fill_manual("",values="grey") + 
  scale_linetype_manual(name="",aesthetics=c("linetype","color"),
                        values = c("obs"="solid","pred"="twodash"),
                        breaks=c("obs","pred"),labels=c("Observed","Predicted")) +
  guides(linetype = guide_legend(order=1,override.aes = list(color = c("darkblue","darkorange" ))
  )) +
  xlab("Year/Month") + ylab("Relative Search Popularity") + 
  ylim(c(-10,100)) +
  theme_minimal() + theme(legend.key = element_rect(fill = c("white"),
                                                    color="white")) +
  theme(legend.spacing.y = unit(-1, "cm")) + 
  ggtitle('"WNBA Ticket" Interest')
wnba_tix_plot

## MLS TICKET ANALYSIS

#notes: draft week dates gleaned from google
mls <- read_csv("ticket_comp.csv",skip=1) %>% 
  mutate(Week=as.Date(Week),
         draftweek = ifelse(Week %in% ymd("2021-01-17","2022-01-10","2022-12-18",
                                          "2023-12-17"),1,0),
         allstar = ifelse(Week %in% ymd("2023-03-19","2023-07-16"),1,0)) %>% 
  filter(year(Week) > 2021, Week <= "2023-08-13") %>% 
  mutate(indexnum=row_number()) %>% 
  rename("trendpoint"= "MLS tickets: (United States)")

mls_tix <- mls %>% 
  select(trendpoint,indexnum,draftweek,allstar) 

#extract CausalImpact inputs as global for easy of use
trendpoint <- mls_tix$trendpoint
indexnum <- mls_tix$indexnum
draftweek <- mls_tix$draftweek
allstar <- mls_tix$allstar

#mark pre-intervention and post-intervention period
pre.period <-c(1, 74)
post.period <- c(75,85)

#create an outcome series with post-intervention as NA per CausalImpact demands
post.period.response <- trendpoint[post.period[1] : post.period[2]]
trendpoint[post.period[1] : post.period[2]] <- NA

#create an outcome series with post-intervention as NA per CausalImpact demands
ss <- AddAutoAr(list(), trendpoint)
ss <- AddSeasonal(ss, trendpoint, nseasons = 52)
bsts.model <- bsts(trendpoint ~ draftweek + allstar, ss, niter = 1000,seed=123)

#run CausalImpact to extract predictions
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)

summary(impact)
mls_Relative_avg = c(value=100*(impact$summary$Actual[1]/impact$summary$Pred[1]-1),
                     LCL=100*((impact$summary$Actual[1]/impact$summary$Pred.upper[1])-1),
                     UCL=100*((impact$summary$Actual[1]/impact$summary$Pred.lower[1])-1))
mls_Relative_sum = c(value=100*(impact$summary$Actual[2]/impact$summary$Pred[2]-1),
                     LCL=100*((impact$summary$Actual[2]/impact$summary$Pred.upper[2])-1),
                     UCL=100*((impact$summary$Actual[2]/impact$summary$Pred.lower[2])-1))
mls_Actual_avg = c(impact$summary$Actual[1],NA,NA)
mls_Actual_sum = c(impact$summary$Actual[2],NA,NA)
mls_Pred_avg = c(impact$summary$Pred[1],impact$summary$Pred.lower[1],impact$summary$Pred.upper[1])
mls_Pred_sum = c(impact$summary$Pred[2],impact$summary$Pred.lower[2],impact$summary$Pred.upper[2])
mls_table <- rbind(mls_Actual_avg,mls_Actual_sum,mls_Pred_avg,mls_Pred_sum,mls_Relative_avg,mls_Relative_sum)
mls_table

#check other outputs for CausalImpact
summary(impact, "report")
plot(impact)
plot(impact$model$bsts.model)
plot(impact$model$bsts.model, "coefficients")
#plot(impact$model$bsts.model, "components") 

#format table for own plot
model_output <- impact$series %>% 
  bind_cols(Week=mls$Week) 

mls_tix_plot <- model_output %>% 
  ggplot(data=.,aes(x=Week)) + 
  geom_ribbon(aes(ymin = point.pred.lower, ymax = point.pred.upper,fill="95% CI")) + 
  geom_line(aes(y=point.pred,linetype="pred"),color="brown",size=1) + 
  geom_line(aes(y=response,linetype="obs"),color="darkolivegreen",size=1) + 
  geom_vline(xintercept=as.Date("2023-06-04")-0.5,linetype="dotted") +  
  scale_fill_manual("",values="grey") + 
  scale_linetype_manual(name="",aesthetics=c("linetype","color"),
                        values = c("obs"="solid","pred"="twodash"),
                        breaks=c("obs","pred"),labels=c("Observed","Predicted")) +
  guides(linetype = guide_legend(order=1,override.aes = list(color = c("darkolivegreen","brown" ))
  )) +
  #scale_fill_manual(name = '',  values=c("bands" = "grey12", "sin" = "grey"))
  xlab("Year/Month") + ylab("Relative Search Popularity") + 
  ylim(c(-10,100)) +
  theme_minimal() + theme(legend.key = element_rect(fill = c("white"),
                                                    color="white")) +
  theme(legend.spacing.y = unit(-1, "cm")) + 
  ggtitle('"MLS Ticket" Interest')
mls_tix_plot

## COMBINE SELF-MADE PLOTS FOR WNBA AND MLS ANALYSES

library(patchwork)
tix_plots <- wnba_tix_plot/mls_tix_plot + plot_layout(ncol = 1, heights = c(1,1)) +
  plot_annotation(tag_levels = 'A')
tix_plots

#Quick add of row labels
stat_labels = c("Actual","Actual","Predicted","Predicted",
                "Relative Increase (%)","Relative Increase (%)",
                "Actual","Actual","Predicted","Predicted",
                "Relative Increase (%)","Relative Increase (%)")
league_labels = c(rep("WNBA",6),rep("MLS",6))
agg_labels = c(rep(c("avg","sum"),6))

# Create the Aggregated Prediction Tables
library(gt)
table_fuel <- as_tibble(wnba_table) %>% 
  bind_rows(as_tibble(mls_table)) %>% 
  bind_cols(stat=factor(stat_labels,levels=c("Predicted","Actual","Relative Increase (%)"))) %>% 
  bind_cols(league=league_labels) %>% 
  bind_cols(agg=agg_labels) %>% 
  mutate(across(where(is.numeric), round, 1)) %>%
  mutate(interval = paste0("(",LCL,", ",UCL,")")) %>% 
  mutate(interval = ifelse(interval=="(NA, NA)",NA,interval)) %>% 
  group_by(league) %>% 
  pivot_wider(id_cols=c(league,stat),names_from=agg,values_from=c(value,interval)) %>%
  arrange(stat) %>% 
  gt(rowname_col = "row", groupname_col = "league") %>%  
  tab_header(
    title = md("Relative Search Popularity for 'WNBA tickets' & 'MLS tickets'"),
    subtitle = md("11 weeks from when Clark drafted and Messi announced transfer, respectively")
  ) %>% 
  tab_spanner(
    label = "Post-Event\nWeekly Average",
    columns = c(value_avg,interval_avg)) %>% 
  tab_spanner(
    label = "Post-Event\nCumulative Total",
    columns = c(value_sum,interval_sum)
  ) %>% 
  cols_label(
    stat = "",
    value_avg = "Average",
    interval_avg = "95% CI",
    value_sum = "Total",
    interval_sum = "95% CI",
  ) %>% 
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = "---"
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>% 
  cols_align(
    align = c("center"),
    columns = c(interval_avg,
                interval_sum)
  ) %>% 
  cols_align(
    align = c("left"),
    columns = c(stat)
  ) %>% 
  tab_footnote(
    footnote = "Relative increase calculated using distribution of counts instead of 
    CausalImpact's default of calculating pointwise percentages before aggregating from the distribution.",
    locations = cells_body(columns=stat,rows=stat=="Relative Increase (%)")
  ) %>% 
  gtsave("results_table.png")
##

## Handy Resources
#https://oliviayu.github.io/post/2019-03-21-bsts/
#https://www.unofficialgoogledatascience.com/2017/07/fitting-bayesian-structural-time-series.html
