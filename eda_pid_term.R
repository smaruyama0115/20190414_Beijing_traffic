# ライブラリの読み込み----
library(tidyverse)
library(magrittr)
library(ggplot2)
library(infotheo)
library(Hmisc)
library(DataExplorer)
#library(rpivotTable)
#library(esquisse)
library(data.table)
library(lubridate)
#library(ggpubr)
library(leaflet)
library(geosphere)

# tibbleのprint設定----
print.tbl_df <- print.data.frame

# パッケージの優先順位変更----
unload_package <- function(pkg_name) {
  packages <- Filter(function(x) stringr::str_detect(x, "^package:"), search())
  packages <- Map(function(x) stringr::str_replace(x, "^package:", ""), packages)
  packages <- unlist(unname(packages))
  
  if(!(pkg_name %in% packages)) {
    return(pkg_name)
  }
  
  result_packages <- pkg_name
  while(TRUE) {
    tryCatch({
      detach(paste0("package:", pkg_name), character.only = TRUE)
      break
    }, error = function(e) {
      required_package <- stringr::str_match(e$message, pattern = "required by ‘(.+?)’")[1, 2]
      required_packages <- unload_package(required_package)
      result_packages <<- c(result_packages, required_packages)
    })
  }
  unique(result_packages)
}

prior_package <- function(pkg_name) {
  pkg_name <- as.character(substitute(pkg_name))
  pkg_names <- unload_package(pkg_name)
  for (pkg_name in pkg_names) {
    suppressPackageStartupMessages(library(pkg_name, character.only = TRUE))
  }
}

prior_package(dplyr)

# データの読み込み----
data_queries <- fread("data_set_phase1/train_queries.csv", stringsAsFactors=FALSE, sep=",")
test_queries <- fread("data_set_phase1/test_queries.csv", stringsAsFactors=FALSE, sep=",")
data_clicks  <- fread("data_set_phase1/train_clicks.csv", stringsAsFactors=FALSE, sep=",")

# 同一 pid で期間中に何回queryを投げたか集計
test_queries %>% 
  group_by(pid) %>% 
  summarize(count_same_pid = n()) %>% 
  ungroup() %>%
  group_by(count_same_pid) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  #arrange(desc(count)) %>%
  filter(count > 1) %>% 
  filter(count_same_pid > 10) %>% 
  ggplot(aes(x=count_same_pid,y=count)) +
  geom_line(stat="identity") +
  scale_y_log10()

# (test)次のqueryを投げるまでの時間の分布----
# (テストデータ内でもqueriyの投げ直しを行っているログがある)

test_queries2 <-
  test_queries %>% 
  arrange(pid,req_time) %>% 
  group_by(pid) %>% 
  mutate(
    req_time      = ymd_hms(req_time),
    lead_req_time = lead(req_time,order_by = req_time),
    next_req_time = ifelse(is.na(pid),NA,lead_req_time - req_time %>% as.integer)
  ) %>%
  ungroup()

test_queries2 %>%
  filter(!is.na(next_req_time)) %>% 
  ggplot(aes(next_req_time)) +
  geom_histogram() +
  xlim(0,50)

# (train)同一pidからのquery----
data_queries %>% 
  group_by(pid) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(100)

# (train)次のqueryを投げるまでの時間の分布----
data_queries2 <-
  data_queries %>% 
  arrange(pid,req_time) %>% 
  group_by(pid) %>% 
  mutate(
    req_time      = ymd_hms(req_time),
    lead_req_time = lead(req_time,order_by = req_time),
    lag_req_time  = lag(req_time,order_by = req_time),
    lead_o        = lead(o,order_by = req_time),
    lead_d        = lead(d,order_by = req_time),
    lag_o         = lag(o,order_by = req_time),
    lag_d         = lag(d,order_by = req_time),
    next_req_time = ifelse(is.na(pid),NA,lead_req_time - req_time %>% as.integer),
    prev_req_time = ifelse(is.na(pid),NA,lag_req_time - req_time %>% as.integer),
  ) %>%
  ungroup()

#pidが偏っていないか確認----
data_queries2 %>%
  filter(!is.na(deff_lead_req_time),deff_lead_req_time != 0,!is.na(pid)) %>%
  filter(deff_lead_req_time %>% as.integer <= 1000) %>% 
  #mutate(deff_lead_req_time = ceiling(deff_lead_req_time/10)*10) %>% 
  group_by(deff_lead_req_time,pid) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x=deff_lead_req_time %>% as.integer,y=count,fill=pid)) +
  geom_bar(stat="identity",position="fill") +
  scale_fill_viridis_c()
  #scale_fill_distiller("Spectral")

#deff_reqの分布を確認----
data_queries2 %>% ungroup() %>% 
  filter(!is.na(deff_lead_req_time),deff_lead_req_time != 0,!is.na(pid)) %>%
  filter(deff_lead_req_time %>% as.integer <= 300) %>% 
#  mutate(deff_lead_req_time = ceiling(deff_lead_req_time/10)*10) %>% 
  mutate(deff_lead_req_time = deff_lead_req_time %>% as.integer()) %>% 
  group_by(deff_lead_req_time,pid) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x=deff_lead_req_time,y=count,fill=pid)) +
  geom_bar(stat="identity") +
  scale_fill_viridis_c()

data_queries2 %>% head(4)
data_clicks   %>% head(4)

data_queries_clicks <-
  data_queries2 %>% 
  left_join(data_clicks,by="sid")

# deff_lead_req_timeごとにclicksの割合を確認----
data_queries_clicks %>%
  filter(!is.na(deff_lead_req_time),deff_lead_req_time != 0,!is.na(pid)) %>%
  filter(deff_lead_req_time <= 100) %>% 
#  mutate(deff_lead_req_time = ceiling(deff_lead_req_time/5)*5) %>%
  mutate(next_req_time = deff_lead_req_time %>% as.integer) %>% 
  mutate(click_mode = click_mode %>% as.factor) %>% 
  group_by(next_req_time,click_mode) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=next_req_time,y=count,fill=click_mode)) +
  geom_bar(stat="identity",position="fill")

# deff_lag_req_timeごとにclicksの割合を確認----
data_queries_clicks %>%
  mutate(deff_lag_req_time = - deff_lag_req_time) %>% 
  filter(!is.na(deff_lag_req_time),deff_lag_req_time != 0,!is.na(pid)) %>%
  filter(deff_lag_req_time <= 200) %>% 
  #  mutate(deff_lag_req_time = ceiling(deff_lag_req_time/5)*5) %>%
  mutate(deff_lag_req_time = deff_lag_req_time %>% as.integer) %>% 
  mutate(click_mode = click_mode %>% as.factor) %>% 
  group_by(deff_lag_req_time,click_mode) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=deff_lag_req_time,y=count,fill=click_mode)) +
  geom_bar(stat="identity",position="fill")

# 再検索する際の検索経路の変更について確認----
data_queries3 <-
  data_queries2 %>% 
  separate(col=o,into=c("o_lng","o_lat"),sep=",") %>% 
  separate(col=d,into=c("d_lng","d_lat"),sep=",") %>% 
  separate(col=lead_o,into=c("lead_o_lng","lead_o_lat"),sep=",") %>% 
  separate(col=lead_d,into=c("lead_d_lng","lead_d_lat"),sep=",") %>% 
  separate(col=lag_o,into=c("lag_o_lng","lag_o_lat"),sep=",") %>% 
  separate(col=lag_d,into=c("lag_d_lng","lag_d_lat"),sep=",") %>% 
  mutate(
    next_o_dist = pmap_dbl(.l = list(o_lng,o_lat,lead_o_lng,lead_o_lat),.f = ~distGeo(c(..1,..2),c(..3,..4))),
    next_d_dist = pmap_dbl(.l = list(d_lng,d_lat,lead_d_lng,lead_d_lat),.f = ~distGeo(c(..1,..2),c(..3,..4))),
    prev_o_dist = pmap_dbl(.l = list(o_lng,o_lat,lag_o_lng,lag_o_lat),.f = ~distGeo(c(..1,..2),c(..3,..4))),
    prev_d_dist = pmap_dbl(.l = list(d_lng,d_lat,lag_d_lng,lag_d_lat),.f = ~distGeo(c(..1,..2),c(..3,..4)))   
  )

data_queries3

data_queries4 <-
  mutate(flag_same_o = ifelse(next_o_dist==0,1,0))
  

data_queries_clicks <-
  data_queries3 %>% 
  left_join(data_clicks,by="sid")

data_queries_clicks %>% 
  filter(!is.na(next_req_time),!is.na(pid)) %>%
  filter(next_req_time <= 100) %>% 
  mutate(next_req_time = floor(next_req_time/5)*5) %>%
  ggplot(
    aes(x=next_req_time %>% as.factor,y=next_d_dist)
  )+
  geom_violin()

data_queries_clicks %>%
  filter(!is.na(next_req_time),!is.na(pid)) %>%
  filter(next_req_time <= 100) %>% 
  #mutate(next_req_time = floor(next_req_time/5)*5) %>%
  mutate(next_req_time = next_req_time %>% as.integer) %>% 
  mutate(click_mode = click_mode %>% as.factor) %>% 
  group_by(next_req_time,click_mode) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=next_req_time,y=count,fill=click_mode)) +
  geom_bar(stat="identity",position="fill")

data_queries_clicks %>% 
  filter(!is.na(next_req_time),!is.na(pid)) %>%
  filter(next_req_time >= 5, next_req_time < 10) %>% 
  filter(next_o_dist < 1000) %>% 
  ggplot(
    aes(x=next_o_dist)
  ) +
  geom_histogram()

data_queries_clicks$next_o_dist %>% table

data_queries_clicks %>%
  filter(!is.na(next_req_time),!is.na(pid)) %>%
  filter(next_req_time >= 5, next_req_time < 10) %>% 
  filter(next_o_dist < 1000) %>% 
  mutate(flag_same_o = ifelse(next_o_dist==0,1,0)) %>% 
  #mutate(next_o_dist = floor(next_o_dist/2)*2) %>%
  mutate(click_mode = click_mode %>% as.factor) %>% 
  group_by(flag_same_o,click_mode) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=flag_same_o ,y=count,fill=click_mode)) +
  geom_bar(stat="identity",position="fill")

data_queries_clicks %>%
  filter(!is.na(next_req_time),!is.na(pid)) %>%
  filter(next_req_time <= 100) %>% 
  #mutate(next_req_time = floor(next_req_time/5)*5) %>%
  mutate(next_req_time = next_req_time %>% as.integer) %>% 
  mutate(flag_same_o = ifelse(next_o_dist==0,1,0)) %>% 
  mutate(flag_same_d = ifelse(next_d_dist==0,1,0)) %>% 
  mutate(click_mode = click_mode %>% as.factor) %>% 
  group_by(next_req_time,click_mode,flag_same_o,flag_same_d) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=next_req_time,y=count,fill=click_mode)) +
  geom_bar(stat="identity",position="fill") +
  facet_grid(flag_same_d ~ flag_same_o)

data_queries_clicks %>%
  filter(!is.na(next_req_time),!is.na(pid)) %>%
  filter(next_req_time <= 100) %>% 
  mutate(next_req_time = floor(next_req_time/5)*5) %>%
  #mutate(next_req_time = next_req_time %>% as.integer) %>% 
  mutate(flag_same_o = ifelse(next_o_dist==0,"same_o","different_o")) %>% 
  mutate(flag_same_d = ifelse(next_d_dist==0,"same_d","different_d")) %>% 
  mutate(click_mode = click_mode %>% as.factor) %>% 
  group_by(next_req_time,click_mode,flag_same_o,flag_same_d) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=next_req_time,y=count,fill=click_mode)) +
  geom_bar(stat="identity",position="fill") +
  facet_grid(flag_same_d ~ flag_same_o)

data_queries_clicks %>%
  mutate(prev_req_time = - prev_req_time) %>% 
  filter(!is.na(prev_req_time),!is.na(pid)) %>%
  filter(prev_req_time <= 50) %>% 
  #mutate(next_req_time = floor(next_req_time/5)*5) %>%
  mutate(prev_req_time = prev_req_time %>% as.integer) %>% 
  mutate(flag_same_o = ifelse(prev_o_dist==0,1,0)) %>% 
  mutate(flag_same_d = ifelse(prev_d_dist==0,1,0)) %>% 
  mutate(click_mode = click_mode %>% as.factor) %>% 
  group_by(prev_req_time,click_mode,flag_same_o,flag_same_d) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=prev_req_time,y=count,fill=click_mode)) +
  geom_bar(stat="identity",position="fill") +
  facet_grid(flag_same_d ~ flag_same_o)

data_queries_clicks %>%
  filter(!is.na(next_req_time),!is.na(pid)) %>%
  filter(next_req_time <= 60) %>% 
  #mutate(next_req_time = floor(next_req_time/5)*5) %>%
  mutate(next_req_time = next_req_time %>% as.integer) %>% 
  mutate(flag_same_o = ifelse(next_o_dist==0,1,0)) %>% 
  mutate(flag_same_d = ifelse(next_d_dist==0,1,0)) %>% 
  mutate(click_mode = click_mode %>% as.factor) %>% 
  group_by(click_mode,flag_same_o,flag_same_d) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=flag_same_o,y=count,fill=click_mode)) +
  geom_bar(stat="identity",position="fill")

  

# 整形して保存----
# data_queries2 %>% 
#   select(sid,next_req_time) %>% 
#   write_csv(path = "data_set_phase1/train_next_req_time_r2.csv")
#   
# test_queries2 %>% 
#   select(sid,next_req_time) %>% 
#   write_csv(path = "data_set_phase1/test_next_req_time_r2.csv")

data_queries_10 <-
  data_queries %>% 
  arrange(pid,req_time) %>% 
  group_by(pid) %>% 
  mutate(
    req_time      = ymd_hms(req_time),
    lead_req_time = lead(req_time,order_by = req_time),
    lag_req_time  = lag(req_time,order_by = req_time),
    lead_o        = lead(o,order_by = req_time),
    lead_d        = lead(d,order_by = req_time),
    next_req_time = ifelse(is.na(pid),NA,lead_req_time - req_time %>% as.integer),
  ) %>%
  separate(col=o,into=c("o_lng","o_lat"),sep=",") %>% 
  separate(col=d,into=c("d_lng","d_lat"),sep=",") %>% 
  separate(col=lead_o,into=c("lead_o_lng","lead_o_lat"),sep=",") %>% 
  separate(col=lead_d,into=c("lead_d_lng","lead_d_lat"),sep=",") %>% 
  mutate(
    next_o_dist = pmap_dbl(.l = list(o_lng,o_lat,lead_o_lng,lead_o_lat,pid),.f = ~ifelse(is.na(..5),NA,distGeo(c(..1,..2),c(..3,..4)))),
    next_d_dist = pmap_dbl(.l = list(d_lng,d_lat,lead_d_lng,lead_d_lat,pid),.f = ~ifelse(is.na(..5),NA,distGeo(c(..1,..2),c(..3,..4))))
  ) %>% 
  ungroup()

data_queries_11 <-
  data_queries_10 %>% 
  mutate(
    flag_same_o = ifelse(next_o_dist!=0|is.na(next_o_dist),0,1),
    flag_same_d = ifelse(next_d_dist!=0|is.na(next_d_dist),0,1),
  )

data_queries_11 %>% 
  select(sid,next_req_time,flag_same_o,flag_same_d,next_o_dist,next_d_dist) %>% 
  write_csv(path = "data_set_phase1/train_next_req_time_r3.csv")

test_queries_10 <-
  test_queries %>% 
  arrange(pid,req_time) %>% 
  group_by(pid) %>% 
  mutate(
    req_time      = ymd_hms(req_time),
    lead_req_time = lead(req_time,order_by = req_time),
    lag_req_time  = lag(req_time,order_by = req_time),
    lead_o        = lead(o,order_by = req_time),
    lead_d        = lead(d,order_by = req_time),
    next_req_time = ifelse(is.na(pid),NA,lead_req_time - req_time %>% as.integer),
  ) %>%
  separate(col=o,into=c("o_lng","o_lat"),sep=",") %>% 
  separate(col=d,into=c("d_lng","d_lat"),sep=",") %>% 
  separate(col=lead_o,into=c("lead_o_lng","lead_o_lat"),sep=",") %>% 
  separate(col=lead_d,into=c("lead_d_lng","lead_d_lat"),sep=",") %>% 
  mutate(
    next_o_dist = pmap_dbl(.l = list(o_lng,o_lat,lead_o_lng,lead_o_lat,pid),.f = ~ifelse(is.na(..5),NA,distGeo(c(..1,..2),c(..3,..4)))),
    next_d_dist = pmap_dbl(.l = list(d_lng,d_lat,lead_d_lng,lead_d_lat,pid),.f = ~ifelse(is.na(..5),NA,distGeo(c(..1,..2),c(..3,..4))))
  ) %>% 
  ungroup()

test_queries_11 <-
  test_queries_10 %>% 
  mutate(
    flag_same_o = ifelse(next_o_dist!=0|is.na(next_o_dist),0,1),
    flag_same_d = ifelse(next_d_dist!=0|is.na(next_d_dist),0,1),
  )

test_queries_11

test_queries_11 %>% 
  select(sid,next_req_time,flag_same_o,flag_same_d,next_o_dist,next_d_dist) %>% 
  write_csv(path = "data_set_phase1/test_next_req_time_r3.csv")

data_plans

data_clicks

data_plans %>%
  group_by(sid) %>% 
  summarize(count_sid = n()) %>%
  ungroup() %>%
  left_join(data_clicks,by="sid") %>% 
  group_by(count_sid,click_mode) %>% 
  summarize(count_clicks = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=count_sid,y=count_clicks,fill=click_mode %>% ifelse(is.na(.),0,.) %>% as.factor)) +
  geom_bar(stat="identity",position="fill") +
  scale_fill_brewer(palette="Paired")

data_plans %>% 
  group_by(sid)

data_plans2 <-
  data_plans %>% 
  group_by(sid,transport_mode) %>%
  summarize(count=n()) %>% 
  ungroup() #%>% 
  #filter(transport_mode %in% c(1,2,7))

data_clicks2 <-
  data_clicks %>% 
  select(sid,click_mode) %>% 
  mutate(flag_click = 1)

data_plans2 %>% 
  left_join(data_clicks2,by=c("sid","transport_mode"="click_mode")) %>% 
  ggplot(aes(x=count,fill=flag_click %>% as.factor)) +
  geom_bar(stat="count",position="fill") +
  facet_grid(transport_mode ~ .)

data_plans2$count %>% table

data_plans3 <-
  data_plans2 %>% 
  spread(key = transport_mode,value=count) %>%
  mutate(
    flag_mode1_double = ifelse(`1`==2,1,0),
    flag_mode2_double = ifelse(`2`==2,1,0),
    flag_mode7_double = ifelse(`7`==2,1,0),
  ) %>% 
  replace_na(
    list(
      flag_mode1_double = 0,
      flag_mode2_double = 0,
      flag_mode7_double = 0
    )
  ) %>% 
  select(sid,starts_with("flag"))

test_plans <- fread("data_set_phase1/test_plans_r2.csv", stringsAsFactors=FALSE, sep=",")

test_plans2 <-
  test_plans %>% 
  group_by(sid,transport_mode) %>%
  summarize(count=n()) %>% 
  ungroup()

test_plans3 <-
  test_plans2 %>% 
  spread(key = transport_mode,value=count) %>%
  mutate(
    flag_mode1_double = ifelse(`1`==2,1,0),
    flag_mode2_double = ifelse(`2`==2,1,0),
    flag_mode7_double = ifelse(`7`==2,1,0),
  ) %>% 
  replace_na(
    list(
      flag_mode1_double = 0,
      flag_mode2_double = 0,
      flag_mode7_double = 0
    )
  ) %>% 
  select(sid,starts_with("flag"))

data_plans3 %>% summary
test_plans3 %>% summary()

data_plans3 %>% write_csv(path = "data_set_phase1/train_flag_double.csv")
test_plans3 %>% write_csv(path = "data_set_phase1/test_flag_double.csv")  
  
?replace_na


















