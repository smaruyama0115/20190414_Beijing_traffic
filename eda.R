#ライブラリのインストール
#install.packages("corrr", dependencies = c("Depends", "Suggests"))

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
data_plans_before <- fread("data_set_phase1/train_plans.csv", stringsAsFactors=FALSE, sep=",")
data_plans <- fread("data_set_phase1/train_plans_r3.csv", stringsAsFactors=FALSE, sep=",")
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
  arrange(desc(count)) %>% 
  separate(col=d,into=c("lng","lat"),sep=",") %>% 
  mutate(lng=lng %>% as.numeric,lat=lat %>% as.numeric) %>% 
  mutate(log10_count = log10(count))

#カウント別カラー用のパレットの作成
pal_d <- colorNumeric(palette="Spectral", domain=data_queries_d$log10_count, reverse=TRUE)

# 地図の作成
map_d <-
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng=data_queries_d$lng,lat=data_queries_d$lat,color=pal_d(data_queries_d$log10_count),radius=500,stroke=FALSE,fillOpacity = 0.6,group="d") %>% 
  addLegend(position='topright', pal=pal_d, values=data_queries_d$log10_count,group="d") %>% 
  addScaleBar(position="bottomleft",options = scaleBarOptions(imperial=FALSE))

map_d

# 出発地も同様に作る

# 目的地の緯度経度ごとのカウント数
data_queries_o <-
  data_queries %>% 
  group_by(o) %>% 
  summarise(count=n()) %>%
  ungroup() %>% 
  arrange(desc(count)) %>% 
  separate(col=o,into=c("lng","lat"),sep=",") %>% 
  mutate(lng=lng %>% as.numeric,lat=lat %>% as.numeric) %>% 
  mutate(log10_count = log10(count))

#カウント別カラー用のパレットの作成
pal_o <- colorNumeric(palette="Spectral", domain=data_queries_o$log10_count, reverse=TRUE)

# 地図の作成
map_o <-
  map_d %>% 
  addTiles() %>% 
  addCircles(lng=data_queries_o$lng,lat=data_queries_o$lat,color=pal_o(data_queries_o$log10_count),radius=500,stroke=FALSE,fillOpacity = 0.6,group="o") %>% 
  addLegend(position='topright', pal=pal_o, values=data_queries_o$log10_count,group = "o") %>% 
  addScaleBar(position="bottomleft",options = scaleBarOptions(imperial=FALSE)) %>% 
  addLayersControl(baseGroups=c("o","d"), options=layersControlOptions(collapsed = FALSE))

map_o
?addLegend

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

# 移動経路をclick_mode別にプロットする----
data_queries_mode <-
  data_queries %>% 
  inner_join(data_clicks,by="sid")

num_lines = 3000

# 目的地の緯度経度----
data_queries_od_mode <-
  data_queries_mode %>% 
  group_by(o,d,click_mode) %>% 
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
  sample_n(size=num_lines)

pal <- colorFactor(palette="Spectral",domain=data_queries_od_mode$click_mode)

plot_queries_od_mode <-
  leaflet() %>% 
  addTiles() %>% 
  addLegend(position='topright', pal=pal, values=data_queries_od_mode$click_mode) %>% 
  addScaleBar(position="bottomleft",options = scaleBarOptions(imperial=FALSE))

for(i in 1:num_lines){
  mode <- data_queries_od_mode %>% filter(row_number() == i) %>% .$click_mode
  o_p  <- data_queries_od_mode %>% filter(row_number() == i) %>% select(o_lng,o_lat,click_mode,log10_count) %>% rename(lng=o_lng,lat=o_lat)
  d_p  <- data_queries_od_mode %>% filter(row_number() == i) %>% select(d_lng,d_lat,click_mode,log10_count) %>% rename(lng=d_lng,lat=d_lat)
  od_p <- union(o_p,d_p)
  plot_queries_od_mode %<>% addPolylines(lng=od_p$lng,lat=od_p$lat,color=pal(od_p$click_mode),weight="3",group=mode %>% as.character)
}

plot_queries_od_mode %>% addLayersControl(overlayGroups=1:11 %>% as.character,options=layersControlOptions(collapsed = FALSE))
?addLayersControl
?layersControlOptions
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

# 最速・最安候補の影響----

#plansからtransport_mode1,2,7,9,11(バス,地下鉄,バス&地下鉄,地下鉄&自転車,バス&自転車)のみ抜き出す

# バスor地下鉄と推測されるtransport_modeのリストを作る
mode_bus_or_subway = c(1,2,7,9,11)

#各sidごとに最速・最安経路となるsidを抜き出す
data_plans_fastest <-
  data_plans %>% 
  filter(transport_mode %in% mode_bus_or_subway) %>% 
  group_by(sid) %>% 
  filter(eta == min(eta)) %>% 
  ungroup %>% 
  select(sid,transport_mode) %>%
  mutate(flag_fastest = 1)
  
data_plans_lowest <-
  data_plans %>% 
  filter(transport_mode %in% mode_bus_or_subway) %>% 
  group_by(sid) %>% 
  filter(price == min(price)) %>% 
  ungroup %>% 
  select(sid,transport_mode) %>%
  mutate(flag_lowest = 1)

# plansの中からバスor地下鉄が選択されたsidを抽出
data_clicks_bus_or_subway <-
  data_clicks %>%
  filter(click_mode %in% mode_bus_or_subway) %>% 
  select(sid,click_mode) %>% 
  mutate(flag_click = 1)

# plansからバス・地下鉄のみを抜き出し、最安・最短フラグをつける
data_plans_bus_of_subway <-
  data_plans %>%
  filter(transport_mode %in% mode_bus_or_subway) %>% 
  inner_join(data_clicks_bus_or_subway %>% select(sid),by="sid") %>% 
  group_by(sid) %>% 
  filter(n() >= 2) %>% 
  ungroup %>% 
  left_join(data_clicks_bus_or_subway,by=c("sid","transport_mode"="click_mode")) %>% 
  select(sid,transport_mode,flag_click) %>%
  distinct() %>% 
  left_join(data_plans_fastest,by=c("sid","transport_mode")) %>% 
  left_join(data_plans_lowest ,by=c("sid","transport_mode")) %>% 
  replace_na(list(flag_click = 0, flag_fastest = 0, flag_lowest = 0)) %>%
  mutate(flag_fast_and_low = str_c(flag_fastest,"-",flag_lowest)) %>% 
  mutate_at(.vars = vars(starts_with("flag_"),"transport_mode"),.funs = as.factor)

# 最速・最安フラグ別 clickの割合
data_plans_bus_of_subway %>% 
  ggplot(
    aes(x=flag_fast_and_low,fill=flag_click)
  ) +
  geom_bar(position="fill") +
  facet_wrap(~transport_mode)

# transport別の最速・最安フラグの割合
data_plans_bus_of_subway %>% 
  ggplot(
    aes(x=transport_mode,fill=flag_fast_and_low)
  ) +
  geom_bar(position="fill")

# transport別のclickの割合
data_plans_bus_of_subway %>% 
  ggplot(
    aes(x=transport_mode,fill=flag_click)
  ) +
  geom_bar(position="fill") 
  
data_plans_bus_of_subway %>% 
  ggplot(
    aes(x=flag_fastest,fill=flag_click)
  ) +
  geom_bar(position = "fill")
 
data_plans_bus_of_subway %>% 
  ggplot(
    aes(x=transport_mode,fill=flag_fastest)
  ) +
  geom_bar()

data_plans_bus_of_subway %>% map(class)
  
inner_join(data_plans_count,by="sid") %>% 
select(sid,transport_mode) %>%
arrange(transport_mode,sid) %>% 
nest(-sid) %>% 
mutate(data = data %>% as.character %>% str_extract("(\\d:\\d|c\\(\\d(,\\s\\d)*\\))")) %>%
ggplot(
  aes(x = data)
) +
geom_bar()

# 車を使うユーザーの特徴を抽出----

# 車を使用したsidを抽出
data_clicks_car <-
  data_clicks %>% 
  filter(click_mode == 3) %>% 
  select(sid) %>% 
  mutate(flag_car = 1)

data_plans_car <-
  data_plans %>% 
  filter(transport_mode == 3) %>% 
  select(sid) %>%
  distinct()

data_queries_car <-
  data_queries %>% 
  filter(!is.na(pid)) %>% 
  inner_join(data_plans_car,by="sid") %>%
  left_join(data_clicks_car,by="sid") %>% 
  replace_na(list(flag_car = 0)) %>% 
  select(pid,flag_car) %>% 
  group_by(pid) %>% 
  summarize(
      count_clicks  = n()
    , count_car     = sum(flag_car)
    , mean_car      = mean(flag_car)
    , flag_have_car = ifelse(count_car>0,1,0)
  ) %>% 
  left_join(data_profiles,by="pid")

data_queries_car

library(corrr)

# 相関係数の確認

data_queries_car %>%
  select(-pid,-count_clicks) %>% 
  correlate %>% 
  focus(c(count_car, mean_car, flag_have_car)) %>% 
  arrange(desc(abs(flag_have_car)))

data_queries_car %>% 
  filter(count_clicks < 30) %>% 
  ggplot(
    aes(x = flag_have_car)
  ) +
  geom_bar()

# 自転車を使うユーザーの特徴を抽出----

data_clicks_cycle <-
  data_clicks %>% 
  filter(click_mode == 2) %>% 
  select(sid) %>% 
  mutate(flag_cycle = 1)

data_plans_cycle <-
  data_plans %>% 
  filter(transport_mode == 2) %>% 
  select(sid) %>%
  distinct()

data_queries_cycle <-
  data_queries %>% 
  filter(!is.na(pid)) %>% 
  inner_join(data_plans_cycle,by="sid") %>%
  left_join(data_clicks_cycle,by="sid") %>% 
  replace_na(list(flag_cycle = 0)) %>% 
  select(pid,flag_cycle) %>% 
  group_by(pid) %>% 
  summarize(
    count_clicks  = n()
    , count_cycle     = sum(flag_cycle)
    , mean_cycle      = mean(flag_cycle)
    , flag_have_cycle = ifelse(count_cycle>0,1,0)
  ) %>% 
  left_join(data_profiles,by="pid")

data_queries_cycle

# 相関係数の確認

data_queries_cycle %>%
  select(-pid,-count_clicks) %>% 
  correlate %>% 
  focus(c(count_cycle, mean_cycle, flag_have_cycle)) %>% 
  arrange(desc(abs(count_cycle)))

# タクシーを使うユーザーの特徴を抽出----

data_clicks_cycle <-
  data_clicks %>% 
  filter(click_mode == 4) %>% 
  select(sid) %>% 
  mutate(flag_cycle = 1)

data_plans_cycle <-
  data_plans %>% 
  filter(transport_mode == 4) %>% 
  select(sid) %>%
  distinct()

data_queries_cycle <-
  data_queries %>% 
  filter(!is.na(pid)) %>% 
  inner_join(data_plans_cycle,by="sid") %>%
  left_join(data_clicks_cycle,by="sid") %>% 
  replace_na(list(flag_cycle = 0)) %>% 
  select(pid,flag_cycle) %>% 
  group_by(pid) %>% 
  summarize(
    count_clicks  = n()
    , count_cycle     = sum(flag_cycle)
    , mean_cycle      = mean(flag_cycle)
    , flag_have_cycle = ifelse(count_cycle>0,1,0)
  ) %>% 
  left_join(data_profiles,by="pid")

data_queries_cycle

# 相関係数の確認

data_queries_cycle %>%
  select(-pid,-count_clicks) %>% 
  correlate %>% 
  focus(c(count_cycle, mean_cycle, flag_have_cycle)) %>% 
  arrange(desc(abs(flag_have_cycle)))

# order が clickに与える影響を確認----
data_plans_join_clicks <-
  data_plans %>% 
  left_join(
    data_clicks %>% mutate(flag_click = 1),
    by=c("sid","transport_mode"="click_mode")
  ) %>% 
  replace_na(list(flag_click = 0))

data_plans_join_clicks

data_plans_join_clicks %>% 
  group_by(sid) %>% 
  filter(max(order)==3) %>% 
  ungroup() %>% 
  ggplot(
    aes(
      x=order %>% as.factor,
      fill=flag_click %>% as.factor
    )
  ) +
  geom_bar(position="fill") +
  facet_wrap(~transport_mode)

?group_by

#徒歩の割合が気温によってどれだけ下がるかを確認----
data_plans_join_clicks %>% 
  group_by(sid) %>% 
  filter(any(transport_mode==6)) %>% 
  ungroup() %>%
  filter(transport_mode == 6) %>% 
  mutate(date=plan_time %>% ymd_hms %>% date) %>% 
  ggplot(
    aes(
      x=date,
      fill=flag_click %>% factor
    )
  ) + 
  geom_bar()

data_plans_join_clicks

# 全transport_modeについて、recomendされたうちclickされた割合を日付ごと出す
data_plans_join_clicks %>% 
  mutate(date=plan_time %>% ymd_hms %>% date) %>% 
  group_by(date, transport_mode) %>% 
  summarise(
    click_rate = mean(flag_click)
  ) %>% 
  ggplot(
    aes(
      x=date,
      y=click_rate,
      color=transport_mode %>% as.factor
    )
  ) +
  geom_line() +
  scale_color_brewer(palette = "Paired")

# clicksの総計を日付ごと出す

data_clicks %>% 
  mutate(date=click_time %>% ymd_hms %>% date) %>% 
  filter(date < "2018/12/01") %>% 
  group_by(date) %>%
  ggplot(
    aes(
      x=date,
      fill=click_mode %>% as.factor
    )
  ) +
  geom_bar() +
  scale_color_brewer(palette = "Paired")

# ・2018-10-08以前は傾向が違う(ただしclick_rateはそこまで変わらない)
# ・若干年末に向かってclicksの総量は減少気味？
# ・10/09,11/05は欠損
# ・11/15は大幅にclicksが少ない
# ・11/15,22のmode9,11は他の日と比べて以上にclick_rateが小さい
# ・mode7(地下鉄&バス), 2(地下鉄)は年末に向けて大きく上昇。
#　 mode1(バス)は微増、mode9,11は大幅に劣化
# 　mode6は微減
# 　徒歩のclick_rateはほぼ変わりがない
# 曜日別のclicks総量(1=日曜日)
data_clicks %>% 
  mutate(
    date = click_time %>% ymd_hms %>% date,
    wday = date %>% wday
    ) %>% 
  filter(
    date >  "2018-10-08",
    date <  "2018-12-01",
    date != "2018-11-15"
    ) %>%
  group_by(date) %>% 
  mutate(count_clicks=n()) %>% 
  ungroup %>% 
  group_by(wday) %>%
  ggplot(
    aes(
      x=wday %>% as.factor,
      y=count_clicks
      )
  ) +
  geom_boxplot() +
  scale_color_brewer(palette = "Paired")

# 土曜日が一番多く、月曜日が一番少ない分布に見える

# 曜日別のclicks総量(transport_mode別)
data_clicks %>% 
  mutate(
    date = click_time %>% ymd_hms %>% date,
    wday = date %>% wday
  ) %>% 
  filter(
    date >  "2018-10-08",
    date <  "2018-12-01",
    date != "2018-11-15"
  ) %>%
  ggplot(
    aes(
      x=wday %>% as.factor
    )
  ) +
  geom_bar() +
  facet_wrap(~click_mode) +
  scale_color_brewer(palette = "Paired")

# # click_rateの曜日別変化
# data_plans_join_clicks %>% 
#   mutate(
#     date = plan_time %>% ymd_hms %>% date,
#     wday = date %>% wday
#   ) %>%
#   filter(
#     date >  "2018-10-08",
#     date <  "2018-12-01",
#     date != "2018-11-15"
#   ) %>%
#   group_by(date, wday, transport_mode) %>% 
#   summarise(
#     click_rate = mean(flag_click)
#   ) %>%
#   ungroup %>% 
#   ggplot(
#     aes(
#       x = wday %>% as.factor,
#       y = click_rate
#     )
#   ) +
#   geom_boxplot() +
#   facet_wrap(~transport_mode)

# 2,7,9,11は単純に季節変動が大きいということかも。
  
# mode5(徒歩)のclick_rateとetaの関係を確認 ---

# mode5がplanに含まれた数を距離別に見てみる
data_plans_join_clicks %>% 
  filter(transport_mode==5) %>% 
  ggplot(
    aes(x = eta)
  ) +
  geom_histogram()

# bin分割してみる
data_plans_join_clicks
data_plans_join_clicks %>% 
  filter(transport_mode==5) %>% 
  mutate(
    eta_bin = eta %>% discretize(disc="equalwidth",nbins=60) %>% .$X %>% factor
  ) %>% 
  ggplot(
    aes(x = eta_bin,fill=flag_click %>% factor)
  ) +
  geom_bar(position="fill")

# 直線距離ごとにクリックされたmodeの割合を見る

#直線距離のヒストグラム
data_queries_with_dist <-
  data_queries %>% 
  separate(col=o,into=c("o_lng","o_lat"),sep=",") %>% 
  separate(col=d,into=c("d_lng","d_lat"),sep=",") %>%
  mutate_at(
    .vars = vars(ends_with("lng"),ends_with("lat")),
    .funs = as.numeric
  ) %>% 
  mutate(
    od_dist = pmap_dbl(.l = list(o_lng,o_lat,d_lng,d_lat),.f = ~distGeo(c(..1,..2),c(..3,..4)))
  )

data_clicks

data_queries_with_dist %>% 
  inner_join(data_clicks,by="sid") %>%
  mutate(od_dist_bin = od_dist %>% discretize(disc="equalfreq",nbins=100) %>% .$X %>% factor) %>% 
  ggplot(
    aes(
       x=od_dist_bin %>% as.factor
      ,fill=click_mode %>% as.factor
      )
  ) +
  geom_bar(position="fill")

?discretize
?distGeo
?separate
distGeo(c(0,0),c(1,1))

