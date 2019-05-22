# install.packages("devtools")
# install.packages("esquisse")
# devtools::install_github("thomasp85/patchwork")
    
library(tidyverse)
library(magrittr)
library(ggplot2)
library(infotheo)
library(Hmisc)
library(DataExplorer)
library(data.table)
library(lubridate)
library(leaflet)
library(geosphere)
library(httr)
library(patchwork)
library(esquisse)
library(RColorBrewer)

# パッケージの優先順位変更
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

# # 出回っていたaddressのcsvを確認----
# data_address <- read_csv("data_set_phase1/address-info-clean2.csv",locale=locale(encoding="SJIS"))
# data_address %>% View()

# # #train_plans(レコメンド情報)の読み込み
# data_plans <- read_csv("data_set_phase1/train_plans.csv")
# 
# #plansを処理しやすい形に変形
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
# 
# # 前処理が重かったので念の為保存しておく
# data_plans %>% write_csv(path = "data_set_phase1/train_plans_r2.csv")

# queryの緯度経度情報を取得
data_queries <- fread("data_set_phase1/train_queries.csv", stringsAsFactors=FALSE, sep=",")

data_o <-
  data_queries %>% 
  select(o) %>% 
  distinct() %>% 
  rename(location = o)

data_d <-
  data_queries %>% 
  select(d) %>% 
  distinct() %>% 
  rename(location = d)

data_location <-
  data_o %>% 
  rbind(data_d) %>% 
  distinct()

# APIを用いてPOI情報を取得

data_poi_subway = data.frame()
data_poi_bus = data.frame()
data_poi_taxi = data.frame()
data_poi_automobile_rental = data.frame()
data_poi_filling_station = data.frame()

url = "https://restapi.amap.com/v3/place/around"
key = list(
   "1d4f000959256da7a9de1224355fd27d"
  ,"4f780910c1c90fb08a75b6050c12fdcd"
  ,"e605f3a7d21b00ba6f1b760f6c7e2e1f"
  ,"33479b2da9be7d9c74c4d731b0a9a55c"
  ,"851d6ded552e6a0de76ff7c5948d7ae0"
  ,"3c83cc913dd4328b9de8fb2456f0f231"
  ,"41e99da9854ee15311fb5237810fa444"
  ,"e3c0c211113be0d3eda10b7a3ad8846c"
  ,"4a7ed472754634489f91520f33c90dc3"
  ,"0012b2712d23f646f338c95ad48de407"
)



# key = list(
#    "e3c0c211113be0d3eda10b7a3ad8846c"
#   ,"4a7ed472754634489f91520f33c90dc3"
#   ,"0012b2712d23f646f338c95ad48de407"
#   ,"33479b2da9be7d9c74c4d731b0a9a55c"
# )

# key = list(
#    "e3c0c211113be0d3eda10b7a3ad8846c"
#   ,"4a7ed472754634489f91520f33c90dc3"
#   ,"0012b2712d23f646f338c95ad48de407"
#   ,"41e99da9854ee15311fb5237810fa444"
# )

#types  = "150500" # subway
#types = "150700" # Bus Station
#types = "151100" # Taxi
types = "010900" # Automobile Rental
#types = "010100" # filling station
radius = "3000"
offset = "24"
key_reset_th = 1900
max_length = length(data_location$location)

max_length

count = 1
key_index = 1

for(i in 1:max_length){
  count = count + 1
  tmp_key = key[[key_index]]
  if(count > key_reset_th){
    key_index = key_index + 1
    count = 1
  }
  tmp_location = data_location$location[[i]]

  res <- GET(
    url = url,
    query = list(
      key = tmp_key
      , location = tmp_location
      , types = types
      , radius = radius
      , offset = offset
    )
  )
  
  result <- res %>% content

  if(result$status == "0"){
    print(i)
    print("error")
  }

  if(result$pois %>% map_chr(~.$typecode) %>% length != 0){
    data_poi_tmp <-
      data.frame(
        num      = i,
        location = tmp_location,
        typecode = result$pois %>% map_chr(~.$typecode),
        distance = result$pois %>% map_chr(~.$distance),
        name     = result$pois %>% map_chr(~.$name),
        id       = result$pois %>% map_chr(~.$id)
      )
    data_poi_automobile_rental %<>% rbind(data_poi_tmp)
  }
  Sys.sleep(0.1)
}

data_poi_bus

data_poi %>% tail(100)
# apiのテスト

data_poi_subway %>% tail(10)
data_poi_bus %>% tail(10)
data_poi_filling_station %>% tail(10)
data_poi_automobile_rental %>% tail(10)

#data_poi の保存
#data_poi %>% write_csv(path = "data_set_phase1/data_poi.csv")
#data_poi_subway %>% write_csv(path = "data_set_phase1/data_poi_subway.csv")
#data_poi_bus %>% write_csv(path = "data_set_phase1/data_poi_bus.csv")
#data_poi_taxi %>% write_csv(path = "data_set_phase1/data_poi_taxi.csv")
#data_poi_filling_station %>% write_csv(path = "data_set_phase1/data_poi_filling_station.csv")
#data_poi_automobile_rental %>% write_csv(path = "data_set_phase1/data_poi_automobile_rental.csv")

#data_poiの読み込み
data_poi <- read_csv("data_set_phase1/data_poi_subway.csv")

# データの確認
data_poi %>% head(100) %>% View

# poiの整形----
data_poi %>% 
  mutate(typecode_length = nchar(typecode)) %>% 
  arrange(desc(typecode_length))

data_poi2 <-
  data_poi %>% 
  separate(col = typecode,into = c("typecode","typecode2","typecode3","typecode4"),sep="\\|")

data_poi3 <-
  data_poi2 %>% 
  select(-typecode2,-typecode3,-typecode4) %>% 
  rbind(data_poi2 %>% select(-typecode ,-typecode3,-typecode4) %>% rename(typecode = "typecode2")) %>%
  rbind(data_poi2 %>% select(-typecode ,-typecode2,-typecode4) %>% rename(typecode = "typecode3")) %>% 
  rbind(data_poi2 %>% select(-typecode ,-typecode2,-typecode3) %>% rename(typecode = "typecode4")) %>%
  drop_na(typecode) %>% 
  mutate(typecode = as.numeric(typecode)) %>% 
  select(-num)

# 整形結果の確認
#data_poi3 %>% head(10)

# poicodeの読み込み----
data_poicode <- read_csv("data_set_phase1/amap_poicode_utf8n_cr.csv")

#読込結果の確認
# data_poicode %>% head(10)

# queriesの読み込み
data_queries <- fread("data_set_phase1/train_queries.csv", stringsAsFactors=FALSE, sep=",")

# clicksの読み込み
data_clicks  <- fread("data_set_phase1/train_clicks.csv", stringsAsFactors=FALSE, sep=",")

# queryとpoiの突合
data_query_poi <-
  data_queries %>% 
  left_join(data_clicks,by="sid") %>% 
  left_join(data_poi3,by=c("d"="location")) %>% 
  left_join(data_poicode,by=c("typecode"="subtype"))
  
data_query_poi
data_poi_taxi

# queryに出てくるbigcat/midcat/subcatの度数を確認
data_query_poi %>%
  mutate(bigcat = fct_infreq(bigcat)) %>% 
  # filter(
  #   bigcat != "Daily Life Service",
  #   bigcat != "Food & Beverages",
  #   bigcat != "Commercial House",
  #   bigcat != "Shopping"
  # ) %>% 
  ggplot(aes(x=bigcat)) +
  geom_bar(stat="count") +
  scale_y_log10()+
  coord_flip()

data_query_poi %>%
  mutate(midcat = fct_infreq(midcat)) %>% 
  ggplot(aes(x=midcat)) +
  geom_bar(stat="count") +
  scale_y_log10() +
  coord_flip()

data_query_poi %>%
  mutate(subcat = fct_infreq(subcat)) %>% 
  ggplot(aes(x=subcat)) +
  geom_bar(stat="count") +
  scale_y_log10() +
  theme(axis.text.y=element_text(size=rel(0.4))) +
  coord_flip()

#頻度を表で確認
data_query_poi %>% 
  mutate(bigcat = fct_infreq(bigcat)) %>% 
  group_by(bigcat) %>% 
  summarize(count = n())

data_query_poi %>% 
  mutate(midcat = fct_infreq(midcat)) %>% 
  group_by(midcat) %>% 
  summarize(count = n())

# すべてのカテゴリを合算した際のclickの割合
g_all <-
  data_query_poi %>% 
  mutate(bigcat = fct_infreq(bigcat)) %>% 
  ggplot(aes(x="cat_all",fill=click_mode %>% as.factor)) +
  geom_bar(stat="count",position="fill") +
  coord_flip() +
  scale_fill_brewer(palette="Paired") +
  guides(fill=FALSE)

# bigcatのclickの割合を確認
g_bigcat <-
  data_query_poi %>% 
  mutate(bigcat = fct_infreq(bigcat)) %>% 
  ggplot(aes(x=bigcat,fill=click_mode %>% as.factor)) +
  geom_bar(stat="count",position="fill") +
  coord_flip() +
  scale_fill_brewer(palette="Paired")

g_bigcat + g_all + plot_layout(ncol=1,heights = c(10,1))

# midcatのclickの割合を確認
data_query_poi %>% 
  mutate(midcat = fct_infreq(midcat)) %>% 
  ggplot(aes(x=midcat,fill=click_mode %>% as.factor)) +
  geom_bar(stat="count",position="fill") +
  coord_flip()

# subcatのclickの割合を確認
data_query_poi %>% 
  mutate(subcat = fct_infreq(subcat)) %>% 
  ggplot(aes(x=subcat,fill=click_mode %>% as.factor)) +
  geom_bar(stat="count",position="fill") +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.4)))

# bigcatがTransportation Serviceのものだけ割合を確認
data_query_poi %>%
  filter(bigcat == "Transportation Service") %>% 
  #filter(bigcat == "Place Name & Address") %>% 
  mutate(subcat = fct_infreq(subcat)) %>% 
  ggplot(aes(x=subcat)) +
  geom_bar(stat="count") +
  scale_y_log10() +
  #theme(axis.text.y=element_text(size=rel(0.4))) +
  coord_flip()

g_bigcat_trans <-
  data_query_poi %>% 
  filter(bigcat == "Transportation Service") %>% 
  #filter(bigcat == "Place Name & Address") %>% 
  mutate(subcat = fct_infreq(subcat)) %>% 
  ggplot(aes(x=subcat,fill=click_mode %>% as.factor)) +
  geom_bar(stat="count",position="fill") +
  coord_flip() +
  scale_fill_brewer(palette="Paired")

g_bigcat_trans + g_all + plot_layout(ncol=1,heights = c(10,1))

# いろいろ見ているうちにPOIの取得が良くないことに気づいたので、poytype別にAPIを叩くことに変更----

data_poi_subway <- read_csv("data_set_phase1/data_poi_subway.csv")

#data_poi_subywayの読み込み----
data_poi_bus <- read_csv("data_set_phase1/data_poi_bus.csv")

data_poi_bus %>% tail

# poicodeの読み込み----
data_poicode <- read_csv("data_set_phase1/amap_poicode_utf8n_cr.csv")

#読込結果の確認
# data_poicode %>% head(10)

# queriesの読み込み
data_queries <- fread("data_set_phase1/train_queries.csv", stringsAsFactors=FALSE, sep=",")

# clicksの読み込み
data_clicks  <- fread("data_set_phase1/train_clicks.csv", stringsAsFactors=FALSE, sep=",")

# queryとpoiの突合
data_query_poi <-
  data_queries %>% 
  left_join(data_clicks,by="sid") %>% 
  left_join(data_poi_bus,by=c("o"="location")) %>% 
  left_join(data_poicode,by=c("typecode"="subtype"))

#各緯度経度に対して、地下鉄までの最短距離を抽出(3km以内に地下鉄がなければNA)
data_query_poi_mindist <-
  data_query_poi %>% 
  group_by(sid) %>% 
  slice(which.min(distance)) %>% 
  ungroup() %>% 
  rbind(data_query_poi %>% filter(is.na(distance)))

# data_query_poi %>% View
# data_query_poi_mindist %>% View
# data_query_poi_mindist %>% summary

#地下鉄までの最短距離についてヒストグラムを作成

data_query_poi_mindist %>% 
  ggplot(aes(x=distance)) + geom_histogram() #+ scale_y_log10()

data_query_poi_mindist %>% 
  #mutate(flag_subway = (is.na(distance)|distance>=1000)) %>% 
  mutate(flag_subway = round(distance/100)*100) %>% 
  ggplot(aes(x=flag_subway %>% as.factor,fill=click_mode %>% as.factor)) +
  geom_bar(stat="count") +
  #scale_fill_brewer(palette="Spectral") +
  theme(axis.text.x = element_text(angle=90)) + scale_y_log10()

data_query_poi_mindist %>% 
  #mutate(flag_subway = (is.na(distance)|distance>=1000)) %>% 
  mutate(flag_subway = round(distance/100)*100) %>% 
  ggplot(aes(x=flag_subway %>% as.factor,fill=click_mode %>% ifelse(is.na(.),0,.) %>% as.factor)) +
  geom_bar(stat="count",position="fill") +
  scale_fill_brewer(palette="Paired") +
  theme(axis.text.x = element_text(angle=90))


# taxiについて----
data_poi_taxi <- read_csv("data_set_phase1/data_poi_taxi.csv")

data_poi_xxx <- data_poi_taxi
data_query_poi <-
  data_queries %>% 
  left_join(data_clicks,by="sid") %>% 
  left_join(data_poi_xxx,by=c("o"="location")) %>% 
  left_join(data_poicode,by=c("typecode"="subtype"))

#各緯度経度に対して、taxiまでの最短距離を抽出(3km以内にtaxiがなければNA)
data_query_poi_mindist <-
  data_query_poi %>% 
  group_by(sid) %>% 
  slice(which.min(distance)) %>% 
  ungroup() %>% 
  rbind(data_query_poi %>% filter(is.na(distance)))

data_query_poi_mindist %>% 
  ggplot(aes(x=distance)) + geom_histogram() #+ scale_y_log10()

data_query_poi_mindist %>% 
  #mutate(flag_subway = (is.na(distance)|distance>=1000)) %>% 
  mutate(flag_subway = round(distance/100)*100) %>% 
  ggplot(aes(x=flag_subway %>% as.factor,fill=click_mode %>% as.factor)) +
  geom_bar(stat="count") +
  #scale_fill_brewer(palette="Spectral") +
  theme(axis.text.x = element_text(angle=90)) + scale_y_log10()

data_query_poi_mindist %>% 
  #mutate(flag_subway = (is.na(distance)|distance>=1000)) %>% 
  mutate(flag_subway = round(distance/200)*200) %>% 
  ggplot(aes(x=flag_subway %>% as.factor,fill=click_mode %>% ifelse(is.na(.),0,.) %>% as.factor)) +
  geom_bar(stat="count",position="fill") +
  scale_fill_brewer(palette="Paired") +
  theme(axis.text.x = element_text(angle=90))

data_query_poi_mindist %>% 
  #mutate(flag_subway = (is.na(distance)|distance>=1000)) %>% 
  mutate(flag_subway = ifelse(distance>=1000|is.na(distance),1,0)) %>% 
  ggplot(aes(x=flag_subway %>% as.factor,fill=click_mode %>% ifelse(is.na(.),0,.) %>% as.factor)) +
  geom_bar(stat="count",position="fill") +
  scale_fill_brewer(palette="Paired") +
  theme(axis.text.x = element_text(angle=90))

# filling_stationについて----
data_poi_filling_station <- read_csv("data_set_phase1/data_poi_filling_station.csv")

data_poi_xxx <- data_poi_filling_station

# poiの整形----

data_poi_xxx <-
  data_poi_xxx %>% 
  separate(col = typecode,into = c("typecode","typecode2","typecode3","typecode4"),sep="\\|")

data_poi_xxx <-
  data_poi_xxx %>% 
  select(-typecode2,-typecode3,-typecode4) %>% 
  rbind(data_poi_xxx %>% select(-typecode ,-typecode3,-typecode4) %>% rename(typecode = "typecode2")) %>%
  rbind(data_poi_xxx %>% select(-typecode ,-typecode2,-typecode4) %>% rename(typecode = "typecode3")) %>% 
  rbind(data_poi_xxx %>% select(-typecode ,-typecode2,-typecode3) %>% rename(typecode = "typecode4")) %>%
  drop_na(typecode) %>% 
  mutate(typecode = as.numeric(typecode))

data_query_poi <-
  data_queries %>% 
  left_join(data_clicks,by="sid") %>% 
  left_join(data_poi_xxx,by=c("d"="location")) %>% 
  left_join(data_poicode,by=c("typecode"="subtype"))

#各緯度経度に対して、taxiまでの最短距離を抽出(3km以内にtaxiがなければNA)
data_query_poi_mindist <-
  data_query_poi %>% 
  group_by(sid) %>% 
  slice(which.min(distance)) %>% 
  ungroup() %>% 
  rbind(data_query_poi %>% filter(is.na(distance)))

data_query_poi_mindist %>% 
  ggplot(aes(x= (round(distance/200)*200) %>% as.factor)) + geom_bar(stat="count") + scale_y_log10()

data_query_poi_mindist %>% 
  #mutate(flag_subway = (is.na(distance)|distance>=1000)) %>% 
  mutate(flag_subway = round(distance/200)*200) %>% 
  ggplot(aes(x=flag_subway %>% as.factor,fill=click_mode %>% ifelse(is.na(.),0,.) %>% as.factor)) +
  geom_bar(stat="count",position="fill") +
  scale_fill_brewer(palette="Paired") +
  theme(axis.text.x = element_text(angle=90))# + scale_y_log10()

# filling_stationについて----
data_poi_automobile_rental <- read_csv("data_set_phase1/data_poi_automobile_rental.csv")

data_poi_xxx <- data_poi_automobile_rental

# poiの整形----

data_poi_xxx <-
  data_poi_xxx %>% 
  separate(col = typecode,into = c("typecode","typecode2","typecode3","typecode4"),sep="\\|")

data_poi_xxx <-
  data_poi_xxx %>% 
  select(-typecode2,-typecode3,-typecode4) %>% 
  rbind(data_poi_xxx %>% select(-typecode ,-typecode3,-typecode4) %>% rename(typecode = "typecode2")) %>%
  rbind(data_poi_xxx %>% select(-typecode ,-typecode2,-typecode4) %>% rename(typecode = "typecode3")) %>% 
  rbind(data_poi_xxx %>% select(-typecode ,-typecode2,-typecode3) %>% rename(typecode = "typecode4")) %>%
  drop_na(typecode) %>% 
  mutate(typecode = as.numeric(typecode))

data_query_poi <-
  data_queries %>% 
  left_join(data_clicks,by="sid") %>% 
  left_join(data_poi_xxx,by=c("d"="location")) %>% 
  left_join(data_poicode,by=c("typecode"="subtype"))

#各緯度経度に対して、taxiまでの最短距離を抽出(3km以内にtaxiがなければNA)
data_query_poi_mindist <-
  data_query_poi %>% 
  group_by(sid) %>% 
  slice(which.min(distance)) %>% 
  ungroup() %>% 
  rbind(data_query_poi %>% filter(is.na(distance)))

data_query_poi_mindist %>% 
  ggplot(aes(x= (round(distance/200)*200) %>% as.factor)) + geom_bar(stat="count") + scale_y_log10()

data_query_poi_mindist %>% 
  #mutate(flag_subway = (is.na(distance)|distance>=1000)) %>% 
  mutate(flag_subway = round(distance/200)*200) %>% 
  ggplot(aes(x=flag_subway %>% as.factor,fill=click_mode %>% ifelse(is.na(.),0,.) %>% as.factor)) +
  geom_bar(stat="count",position="fill") +
  scale_fill_brewer(palette="Paired") +
  theme(axis.text.x = element_text(angle=90))# + scale_y_log10()

data_poi_automobile_rental

# POIの整形(1km以内にあるPOIの集計)----
data_poi_subway_count <-
  read_csv("data_set_phase1/data_poi_subway.csv") %>% 
  filter(distance <= 1000) %>% 
  group_by(location) %>% 
  summarize(count_suwbay = n())

data_poi_bus_count <-
  read_csv("data_set_phase1/data_poi_bus.csv") %>% 
  filter(distance <= 1000) %>% 
  group_by(location) %>% 
  summarize(count_bus = n())

data_poi_taxi_count <-
  read_csv("data_set_phase1/data_poi_taxi.csv") %>% 
  filter(distance <= 1000) %>% 
  group_by(location) %>% 
  summarize(count_taxi = n())

data_poi_filling_station_count <-
  read_csv("data_set_phase1/data_poi_filling_station.csv") %>% 
  filter(distance <= 1000) %>% 
  group_by(location) %>% 
  summarize(count_filling_station = n())

data_poi_automobile_rental_count <-
  read_csv("data_set_phase1/data_poi_automobile_rental.csv") %>% 
  filter(distance <= 1000) %>% 
  group_by(location) %>% 
  summarize(count_automobile_rental = n())

data_poi_count <-
  data_poi_subway_count %>% 
  full_join(data_poi_bus_count,by="location") %>% 
  full_join(data_poi_taxi_count,by="location") %>% 
  full_join(data_poi_filling_station_count,by="location") %>% 
  full_join(data_poi_automobile_rental_count,by="location") %>% 
  mutate_at(.vars = vars(starts_with("count_")), .funs = ~replace_na(.,0))

data_poi_count %>% write_csv(path = "data_set_phase1/data_poi_count.csv")


”# POIの取得テスト----
key = "1d4f000959256da7a9de1224355fd27d"
location = "116.473168,39.993015"
#location = "100.0,45.0"
radius = "1000"

res <- GET(
  url = "https://restapi.amap.com/v3/place/around",
  query = list(
    key = key
    , location = location
    , radius = radius
  )
)

result <- res %>% content
result %>% list.tree
result$pois %>% list.tree
result$status == "1"

result$pois %>% length
View(result$pois[[1]])

result$pois[[4]]$typecode
result$pois[[4]]$distance

result$pois %>% map_chr(~.$typecode)
result$pois %>% map_chr(~.$distance)

data.frame(
  location = location,
  typecode = result$pois %>% map_chr(~.$typecode),
  distance = result$pois %>% map_chr(~.$distance),
  name     = result$pois %>% map_chr(~.$name),
  id       = result$pois %>% map_chr(~.$id)
  )

?map
result

for (i in 1:length(result$pois)) {
  tmp_typecode = result$pois[[i]]$typecode
  tmp_dist = result$pois[[i]]$distance
  print(tmp_dist)
}

https://restapi.amap.com/v3/place/around
1d4f000959256da7a9de1224355fd27d	

?POST
test <- POST(
  url = "http://app.mobike.com/api/nearby/v4/nearbyBikeInfo",
  add_headers(
      "platform" = "1",
      "Content-Type" = "application/x-www-form-urlencoded",
      "User-Agent" = "User-Agent: Mozilla/5.0 (Android 7.1.2; Pixel Build/NHG47Q) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.9 NTENTBrowser/3.7.0.496 (IWireless-US) Mobile Safari/537.36"
  ),
  body = list(latitude="39.914824",longitude="116.435446"),
  encode = "form"
)
test
test %>% content %>% list.tree
39.914824, 116.435446
