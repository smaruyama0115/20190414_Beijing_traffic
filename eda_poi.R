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

# 出回っていたaddressのcsvを確認----
data_address <- read_csv("data_set_phase1/address-info-clean2.csv",locale=locale(encoding="SJIS"))
data_address %>% View()

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

# data_poi = data.frame()
# 
# url = "https://restapi.amap.com/v3/place/around"
# key = "1d4f000959256da7a9de1224355fd27d"
# key = "4f780910c1c90fb08a75b6050c12fdcd"
# key = "e605f3a7d21b00ba6f1b760f6c7e2e1f"
# key = "33479b2da9be7d9c74c4d731b0a9a55c"
# radius = "1000"
# max_length = length(data_location$location)
# 
# max_length
# key
# 
# for(i in 2805:4000){
#   tmp_location = data_location$location[[i]]
#   
#   res <- GET(
#     url = url,
#     query = list(
#       key = key
#       , location = tmp_location
#       , radius = radius
#     )
#   )
#   
#   result <- res %>% content
# 
#   if(result$status == "0"){print("error")}
#   
#   if(result$pois %>% map_chr(~.$typecode) %>% length != 0){
#     data_poi_tmp <-
#       data.frame(
#         num      = i,
#         location = tmp_location,
#         typecode = result$pois %>% map_chr(~.$typecode),
#         distance = result$pois %>% map_chr(~.$distance)
#       )
#     data_poi %<>% rbind(data_poi_tmp)
#   }
#   
#   Sys.sleep(0.1)
# }
# 
# data_poi %>% tail(100)

#data_poi の保存
#data_poi %>% write_csv(path = "data_set_phase1/data_poi.csv")

#data_poiの読み込み
data_poi <- read_csv("data_set_phase1/data_poi.csv")

data_poi %>% head(10)

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

data_poi3 %>% head(10)

# poicodeの読み込み
data_poicode <- read_csv("data_set_phase1/amap_poicode_utf8n_cr.csv")

data_poicode %>% head(10)

# queriesの読み込み
data_queries <- fread("data_set_phase1/train_queries.csv", stringsAsFactors=FALSE, sep=",")

# clicksの読み込み
data_clicks  <- fread("data_set_phase1/train_clicks.csv", stringsAsFactors=FALSE, sep=",")

# queryとpoiの突合
data_query_poi <-
  data_queries %>% 
  left_join(data_poi3,by=c("o"="location")) %>% 
  inner_join(data_poicode,by=c("typecode"="subtype")) %>% 
  inner_join(data_clicks,by="sid")
  
data_query_poi

# queryに出てくるbigcat/midcat/subcatの度数を確認
data_query_poi %>%
  mutate(bigcat = fct_infreq(bigcat)) %>% 
  filter(
    bigcat != "Daily Life Service",
    bigcat != "Food & Beverages",
    bigcat != "Commercial House",
    bigcat != "Shopping"
  ) %>% 
  ggplot(aes(x=bigcat)) +
  geom_bar(stat="count") +
  coord_flip()

data_query_poi %>%
  mutate(midcat = fct_infreq(midcat)) %>% 
  ggplot(aes(x=midcat)) +
  geom_bar(stat="count") +
  coord_flip()

data_query_poi %>%
  mutate(subcat = fct_infreq(subcat)) %>% 
  ggplot(aes(x=subcat)) +
  geom_bar(stat="count") +
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

# bigcatのclickの割合を確認
data_query_poi %>% 
  mutate(bigcat = fct_infreq(bigcat)) %>% 
  ggplot(aes(x=bigcat,fill=click_mode %>% as.factor)) +
  geom_bar(stat="count",position="fill") +
  coord_flip()

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
  theme(axis.text.y=element_text(size=rel(0.6)))

data_query_poi

# POIの取得テスト----
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
  result   = c(result)
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







