# ライブラリの読み込み----
library(tidyverse)
library(magrittr)
library(ggplot2)
#library(infotheo)
library(Hmisc)
library(DataExplorer)
#library(rpivotTable)
#library(esquisse)
library(data.table)
library(lubridate)
#library(ggpubr)
library(leaflet)

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

# ユーザー特徴
data_profiles <- read_csv("data_set_phase1/profiles.csv")
data_profiles %>% head(100) %>% print
#data_profiles %>% create_report(output_file = "data_profiles_report.html", output_dir = "create_report")

# #レコメンド一覧
#data_plans <- read_csv("data_set_phase1/train_plans.csv")
# 
# #test <- data_plans %>% head(100)
# #test %>% names
# #test %>% print
# 
# distance       <- data_plans$plans %>% str_extract_all("\"distance\":\\s\\d*")       %>% map(~str_remove(.,"\"distance\":\\s"))       %>% map(as.integer)
# price          <- data_plans$plans %>% str_extract_all("\"price\":\\s\\d*")          %>% map(~str_remove(.,"\"price\":\\s"))          %>% map(as.integer)
# eta            <- data_plans$plans %>% str_extract_all("\"eta\":\\s\\d*")            %>% map(~str_remove(.,"\"eta\":\\s"))            %>% map(as.integer)
# transport_mode <- data_plans$plans %>% str_extract_all("\"transport_mode\":\\s\\d*") %>% map(~str_remove(.,"\"transport_mode\":\\s")) %>% map(as.integer)
# 
# df_plans<-
#   pmap(
#     .l = list(distance,price,eta,transport_mode),
#     .f = ~data.frame(distance = ..1, price = ..2, eta = ..3, transport_mode = ..4)
#   ) %>% 
#   map(~mutate(.,order = row_number()))
# 
# data_plans %<>%
#   select(-plans) %>% 
#   mutate(plan = df_plans) %>% 
#   unnest(plan)

# 前処理が重かったので念の為保存しておく
#data_plans %>% write_csv(path = "data_set_phase1/train_plans_r2.csv")

# plans,clicksの読み込み----
data_plans <- fread("data_set_phase1/train_plans_r2.csv", stringsAsFactors=FALSE, sep=",")
data_clicks  <- fread("data_set_phase1/train_clicks.csv", stringsAsFactors=FALSE, sep=",")

#data_plans のplan_timeのフォーマットをなおす
data_plans %<>% mutate(plan_time = ymd_hms(plan_time))

# transport_modeをfactorに変換
data_plans %<>% mutate(transport_mode = transport_mode %>% as.factor)
data_clicks  %<>% mutate(click_mode = click_mode %>% as.factor)

# plan_timeの時間と、深夜フラグの追加
data_plans %<>% 
  mutate(hour = hour(plan_time)) %>% 
  mutate(flag_midnight = ifelse(hour<=4|hour>=23,1,0))

data_clicks %<>% 
  mutate(click_hour = hour(click_time)) %>% 
  mutate(flag_click_midnight = ifelse(click_hour<=4|click_hour>=23,1,0))

#基礎統計量
data_plans %>%
  select(-order,-hour,-flag_midnight) %>% 
  summary

#基礎統計量(transport_mode別)
for (i in 1:11) {
  print(str_c("transport_mode = ",i))
  data_plans %>% 
    filter(transport_mode == i) %>% 
    select(distance,price,eta) %>% 
    summary %>% print
}
data_plans
# transport_modeの度数
data_plans %>% 
  ggplot(
    aes(x=transport_mode %>% as.factor,fill=transport_mode)
  ) +
  geom_bar() +
  ggtitle("あいうえお") +
  theme_gray(base_family = "HiraKakuPro-W3")

data_clicks %>% 
  ggplot(
    aes(x=click_mode %>% as.factor,fill=click_mode)
  ) +
  geom_bar()

# 2->7->1->9->5->3の順で選ばれやすい
# 4,6はレコメンドされても特に選ばれにくい

# 日付別click_mode割合
data_clicks %>%
  mutate(click_day = click_time %>% date) %>% 
  filter(click_day <= "2018-11-30") %>% 
  ggplot(
    aes(x=click_day, fill=click_mode)
  ) +
  geom_bar(position="fill")

data_plans
# transport_modeの割合(時間別)
data_plans %>%
  ggplot(
    aes(x=hour %>% as.factor,fill=transport_mode)
  ) +
  geom_bar(position="fill")

# 1：23時~5時で増加 
# 2：23時~5時で減少
# 8：深夜しかレコメンドされない

# transport_modeの割合(深夜かどうかで比較)
data_plans %>% 
  ggplot(
    aes(x=flag_midnight %>% as.factor,fill=transport_mode)
  ) +
  geom_bar(position="fill")

# priceのヒストグラム
data_plans %>% 
  ggplot(
    aes(
      x = price,
    )
  ) + 
  geom_histogram() +
  facet_wrap(~ transport_mode,scales="free")

# 3,5,6は料金を取らない(=徒歩or自転車or車)
# 初乗り運賃
#  200:1 
#  300:2,7,9,11
#  800:4
# 1000:8
# 1300:10

# distanceのmedian
# 3 -> 12760
# 5 ->  1783
# 6 ->  3817

# etaのmedian
# 3 -> 1809
# 5 -> 1595
# 6 -> 1151

# 距離と時間から順当に考えると
# 3:車, 5:徒歩, 6:自転車

# distanceのヒストグラム
data_plans %>% 
  ggplot(
    aes(
      x = distance
    )
  ) + 
  geom_histogram() +
  facet_wrap(~ transport_mode,scales="free")

# densityとpriceの関係
data_plans %>%
  sample_n(size=100000) %>% 
  filter(!is.na(distance),!is.na(price)) %>% 
  ggplot(
    aes(
      x = distance,
      y = price
    )
  ) + 
  geom_point() +
  facet_wrap(~ transport_mode)

# densityとpriceの関係(スケールフリー)
data_plans %>%
  filter(!is.na(distance),!is.na(price)) %>% 
  sample_n(size=100000) %>% 
  ggplot(
    aes(
      x = distance,
      y = price
    )
  ) + 
  geom_point() +
  facet_wrap(~ transport_mode,scales="free")

# # densityとetaの関係
# data_plans %>%
#   sample_n(size=100000) %>% 
#   filter(!is.na(distance),!is.na(eta)) %>% 
#   ggplot(
#     aes(
#       x = distance,
#       y = eta
#     )
#   ) + 
#   geom_point() +
#   facet_wrap(~ transport_mode)
# 
# # etaとpriceの関係
# data_plans %>%
#   sample_n(size=100000) %>% 
#   filter(!is.na(eta),!is.na(price)) %>% 
#   ggplot(
#     aes(
#       x = eta,
#       y = price
#     )
#   ) + 
#   geom_point() +
#   facet_wrap(~ transport_mode)


# transport_mode 2の詳細
data_plans %>%
  filter(transport_mode == 2) %>%
  filter(price <= 700) %>% 
  sample_n(size=100000) %>%
  ggplot(
    aes(
      x = price %>% as.factor,
      y = distance
    )
  ) + 
  geom_boxplot()

# transport_mode 9の詳細
data_plans %>%
  filter(transport_mode == 9) %>%
  filter(price <= 700) %>% 
  sample_n(size=100000) %>% 
  ggplot(
    aes(
      x = price %>% as.factor,
      y = distance
    )
  ) + 
  geom_boxplot()

# transport_mode 2,9の詳細
data_plans %>%
  filter(transport_mode == 2 | transport_mode ==9) %>%
  filter(price <= 700) %>% 
  sample_n(size=100000) %>% 
  ggplot(
    aes(
      x = price %>% as.factor,
      y = distance,
    )
  ) + 
  geom_boxplot() +
  facet_grid(.~transport_mode)

# 2,9は地下鉄と関係がありそう

# transport_mode 1の詳細
data_plans %>%
  filter(transport_mode == 1) %>%
  sample_n(size=100000) %>% 
  ggplot(
    aes(
      x = price,
      y = distance,
      color = flag_midnight %>% as.factor
    )
  ) +
  geom_point() +
  xlim(0,5000) +
  facet_grid(.~flag_midnight)

# 深夜だからといって特に料金は変わらない
# 5kmごとに100RMB増加しているため、おそらくバス

# transport_mode 4の詳細
data_plans %>%
  filter(
    transport_mode == 4
    ) %>%
  sample_n(size=100000) %>% 
  ggplot(
    aes(
      x = distance,
      y = price
    )
  ) +
  geom_point()

data_plans %>% 
  filter(transport_mode == 4) %$%
  lm(price ~ distance,data=.) %>% 
  summary

#1km増加すると約293RMB上昇。料金の上がり幅的におそらくタクシー

# transport_mode 10の詳細
data_plans %>%
  filter(transport_mode == 10) %>%
  ggplot(
    aes(
      x = price,
      y = distance
    )
  ) +
  geom_point()

# transport_mode 11の詳細
data_plans %>%
  filter(
    transport_mode == 11
  ) %>%
  ggplot(
    aes(
      x = distance,
      y = price
    )
  ) +
  geom_point()

data_plans %>% 
  filter(
    transport_mode == 11
  ) %>%
  select(price) %>% 
  table

#100RMB単位の料金

# transport_mode 8の詳細
data_plans %>%
  filter(
    transport_mode == 8
  ) %>%
  ggplot(
    aes(
      x = distance,
      y = price,
    )
  ) +
  geom_point()

data_plans %>% 
  filter(transport_mode == 8) %$%
  lm(price ~ distance,data=.) %>% 
  summary

data_plans %>% 
  filter(
    transport_mode == 8
  ) %>%
  select(price) %>% 
  table

#100RMB単位の料金

# train_queries の読み込み----
data_queries <- fread("data_set_phase1/train_queries.csv", stringsAsFactors=FALSE, sep=",")

# 目的地の緯度経度ごとのカウント数
data_queries_d <-
  data_queries %>% 
  group_by(d) %>% 
  summarise(count=n()) %>%
  ungroup() %>% 
  arrange(desc(count))

# # 緯度経度のカウントをグラフで表示
# data_queries_d %>%
#   mutate(d=d %>% fct_inorder) %>% 
#   head(1500) %>% 
#   ggplot(
#     aes(x= d,y=count)
#   ) +
#   geom_bar(stat="identity") +
#   theme(axis.text.x = element_blank())

# カウントの多い上位100地点を地図上にプロット
data_queries_d_plot <-
  data_queries_d %>% 
  separate(col=d,into=c("lng","lat"),sep=",") %>% 
  mutate(lng=lng %>% as.numeric,lat=lat %>% as.numeric) %>% 
  mutate(log10_count = log10(count)) #%>% 
  #head(100)

#カウント別カラー用のパレットの作成
pal <- colorNumeric(palette="Spectral", domain=data_queries_d_plot$log10_count, reverse=TRUE)

# 地図の作成
data_queries_d_plot %>% 
  leaflet() %>% 
  addTiles() %>% 
  #setView(lng=,lat=,zoom=16) %>% 
  addCircles(lng=~lng,lat=~lat,color=~pal(log10_count),radius=500,stroke=FALSE,fillOpacity = 0.6) %>% 
  addLegend(position='topright', pal=pal, values=~log10_count) %>% 
  addScaleBar(position="bottomleft",options = scaleBarOptions(imperial=FALSE))

# 目的地の緯度経度ごとのカウント数
data_queries_od <-
  data_queries %>% 
  group_by(o,d) %>% 
  summarise(count=n()) %>%
  ungroup() %>% 
  arrange(desc(count)) %>% 
  separate(col=o,into=c("o_lng","o_lat"),sep=",") %>% 
  separate(col=d,into=c("d_lng","d_lat"),sep=",") %>% 
  mutate(
    o_lng=o_lng %>% as.numeric,
    o_lat=o_lat %>% as.numeric,
    d_lng=d_lng %>% as.numeric,
    d_lat=d_lat %>% as.numeric
  ) %>% 
  mutate(log10_count = log10(count)) %>% 
  head(300)

pal <- colorNumeric(palette="Spectral", domain=data_queries_od$log10_count, reverse=TRUE)

plot_queries_od <-
  leaflet() %>% 
  addTiles() %>% 
  addLegend(position='topright', pal=pal, values=data_queries_od$log10_count) %>% 
  addScaleBar(position="bottomleft",options = scaleBarOptions(imperial=FALSE))

for(i in 1:300){
  o_p  <- data_queries_od %>% filter(row_number() == i) %>% select(o_lng,o_lat,count,log10_count) %>% rename(lng=o_lng,lat=o_lat)
  d_p  <- data_queries_od %>% filter(row_number() == i) %>% select(d_lng,d_lat,count,log10_count) %>% rename(lng=d_lng,lat=d_lat)
  od_p <- union(o_p,d_p)
  
  plot_queries_od %<>% addPolylines(lng=od_p$lng,lat=od_p$lat,color=pal(od_p$log10_count),weight="5")
}

plot_queries_od

# test_queries の読み込み----
data_test_queries <- fread("data_set_phase1/test_queries.csv", stringsAsFactors=FALSE, sep=",")

# 同一ユーザー数を確認
data_test_queries %>% nrow

data_test_queries_pid <-
  data_test_queries %>% 
  select(pid) %>% 
  mutate(pid = pid %>%  as.integer) %>% 
  distinct()

print("test_queriesに含まれているpid")
data_test_queries_pid %>% nrow

# test_queries と train_queries の両方に含まれているpidを抽出
print("test_queriesとtrain_queriesの両方に含まれているpid")
data_queries %>% 
  inner_join(data_test_queries_pid,by="pid") %>% 
  select(pid) %>% 
  distinct() %>% 
  nrow

# 過去に車を使ったか、タクシーを使ったかなどの情報は推測に役立つかも？




