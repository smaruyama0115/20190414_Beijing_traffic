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

# queryの緯度経度情報を取得----
data_queries <- fread("data_set_phase1/train_queries.csv", stringsAsFactors=FALSE, sep=",")
test_queries <- fread("data_set_phase1/test_queries.csv", stringsAsFactors=FALSE, sep=",")

data_queries %>% head(10)
test_queries %>% head(10)

data_o <-
  data_queries %>% 
  union_all(test_queries) %>% 
  select(o) %>% 
  distinct() %>% 
  rename(location = o)

data_d <-
  data_queries %>%
  union_all(test_queries) %>% 
  select(d) %>% 
  distinct() %>% 
  rename(location = d)

data_location <-
  data_o %>% 
  rbind(data_d) %>% 
  distinct()

data_location2 <-
  data_location %>% 
  separate(col=location,into=c("lng","lat"),sep=",") %>% 
  mutate(
    lng = lng %>% as.character %>% as.numeric,
    lat = lat %>% as.character %>% as.numeric
    )

data_mobike = data.frame()

max_length = length(data_location2$lng)

for(i in 1:max_length){
  for(add_lng in seq(from=-0.004, to=0.004, by=0.002)){
    for(add_lat in seq(from=-0.004, to=0.004, by=0.002)){
      tmp_lng = data_location2$lng[[i]] + add_lng
      tmp_lat = data_location2$lat[[i]] + add_lat
      
      result <- POST(
        url = "http://app.mobike.com/api/nearby/v4/nearbyBikeInfo",
        add_headers(
          "platform" = "1",
          "Content-Type" = "application/x-www-form-urlencoded",
          "User-Agent" = "User-Agent: Mozilla/5.0 (Android 7.1.2; Pixel Build/NHG47Q) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.9 NTENTBrowser/3.7.0.496 (IWireless-US) Mobile Safari/537.36"
        ),
        body = list(latitude=tmp_lat,longitude=tmp_lng),
        encode = "form"
      )
      
      result_con <- result %>% content
      
      if(result$status != "200"){
        print(i)
        print("error")
      }
      
      if(result_con$bike %>% map_chr(~.$distId) %>% length != 0){
        data_mobike_tmp <-
          data.frame(
            num        = i,
            origin_lng = data_location2$lng[[i]],
            origin_lat = data_location2$lat[[i]],
            lng        = tmp_lng,
            lat        = tmp_lat,
            distId     = result_con$bike %>% map_chr(~.$distId),
            distX      = result_con$bike %>% map_dbl(~.$distX),
            distY      = result_con$bike %>% map_dbl(~.$distY),
            distNum    = result_con$bike %>% map_int(~.$distNum),
            distance   = result_con$bike %>% map_chr(~.$distance),
            bikeIds    = result_con$bike %>% map_chr(~.$bikeIds),
            biketype   = result_con$bike %>% map_int(~.$biketype),
            type       = result_con$bike %>% map_int(~.$type),
            operateType = result_con$bike %>% map_int(~.$operateType)
            
          )
        data_mobike %<>% rbind(data_mobike_tmp)
      }
    }
  }
}

data_mobike_distinct <-
  data_mobike %>% 
  group_by(bikeIds) %>% 
  filter(row_number() == 1) %>% 
  ungroup()  

data_mobike_distinct %>% nrow()

data_mobike %>% tail(10)

data_mobike %>% select(bikeIds) %>% distinct() %>% nrow()
data_mobike %>% select(bikeIds) %>% nrow()

data_mobike
data_location2$lng[[1]]

# 取得したmobikeデータの保存----
#data_mobike %>% write_csv(path = "data_set_phase1/data_mobike.csv")
#data_mobike_distinct %>% write_csv(path = "data_set_phase1/data_mobike_distinct.csv")

data_mobike_distinct <- fread("data_set_phase1/data_mobike_distinct.csv", stringsAsFactors=FALSE, sep=",")

# データのsummary ----
data_mobike_distinct %>% summary()
data_mobike_distinct$biketype %>% table
data_mobike_distinct$operateType %>% table

# mobikeの情報をメッシュにまとめる----
data_mobike_distinct2 <-
  data_mobike_distinct %>% 
  group_by(origin_lng,origin_lat) %>% 
  summarize(count_mobike=n()) %>% 
  ungroup() %>%
  mutate(
    origin_lng = format(origin_lng,nsmall=2),
    origin_lat = format(origin_lat,nsmall=2)
  ) %>% 
  unite(col = "location", c("origin_lng","origin_lat"), sep = ",") # %>% 
#  mutate(log10_count = log10(count))

data_mobike_distinct2 %>% write_csv(path = "data_set_phase1/data_mobike_clean.csv")

# data_mobike_distinct2 %>% esquisser

# 自転車のカウント数をメッシュで地図上に表示----
# (都会の方が多いということぐらいしかわからない)
pal <- colorNumeric(palette="Spectral", domain=data_mobike_distinct2$count, reverse=TRUE)

map <-
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng=data_mobike_distinct2$origin_lng ,lat=data_mobike_distinct2$origin_lat,color=pal(data_mobike_distinct2$count),radius=500,stroke=FALSE,fillOpacity = 0.6,group="o") %>% 
  addLegend(position='topright', pal=pal, values=data_mobike_distinct2$count) %>% 
  addScaleBar(position="bottomleft",options = scaleBarOptions(imperial=FALSE))

map

# mobikeの数とclick_rateについての関係を確認----
data_queries <- fread("data_set_phase1/train_queries.csv", stringsAsFactors=FALSE, sep=",")
data_clicks  <- fread("data_set_phase1/train_clicks.csv", stringsAsFactors=FALSE, sep=",")
data_poi_count <- fread("data_set_phase1/data_poi_count.csv", stringsAsFactors=FALSE, sep=",")

data_queries
data_clicks
data_poi_count

data_queries2 <-
  data_queries %>%
  left_join(data_poi_count,by=c("o"="location")) %>% 
  #separate(col=o,into=c("o_lng","o_lat"),sep=",",convert = TRUE) %>% 
  left_join(data_clicks,by="sid") %>% 
  #left_join(data_mobike_distinct2,by=c("o_lng" = "origin_lng","o_lat"="origin_lat")) %>% 
  left_join(data_mobike_distinct2,by=c("o"="location")) %>% 
  mutate(count=ifelse(is.na(count),0,count))

data_queries2 %>% 
  ggplot(aes(x=count)) +
  geom_histogram()

# countごとclickの内訳(自転車が多いと地下鉄利用者が多い(都市部のため))----
data_queries2 %>%
  mutate(count = floor(count/20)*20) %>%
  mutate(click_mode = click_mode %>% as.factor) %>% 
  group_by(count,click_mode) %>% 
  summarize(click_count = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=count,y=click_count,fill=click_mode)) +
  geom_bar(stat="identity",position="fill")

# 地下鉄吸うとcountの関係----
data_queries2 %>% 
  ggplot(aes(x=count,y=count_suwbay)) +
  geom_point()

# count_subway <= 3 のエリアに対して、自転車の数ごとのclickの割合を比較----
data_queries2 %>%
  filter(count_suwbay <= 2) %>% 
  mutate(count = floor(count/5)*5) %>%
  #mutate(count = ifelse(count<=50,0,1)) %>%
  mutate(click_mode = click_mode %>% as.factor) %>% 
  group_by(count,click_mode,count_suwbay) %>% 
  summarize(click_count = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=count,y=click_count,fill=click_mode %>% ifelse(is.na(.),0,.) %>% as.factor)) +
  geom_bar(stat="identity",position="fill") +
  facet_grid(count_suwbay ~ .) +
  scale_fill_brewer(palette="Paired")

data_queries2$count %>% table

data_queries2  

data_queries

# # 目的地の緯度経度ごとのカウント数
# data_queries_o <-
#   data_queries %>% 
#   group_by(o) %>% 
#   summarise(count=n()) %>%
#   ungroup() %>% 
#   arrange(desc(count)) %>% 
#   separate(col=o,into=c("lng","lat"),sep=",") %>% 
#   mutate(lng=lng %>% as.numeric,lat=lat %>% as.numeric) %>% 
#   mutate(log10_count = log10(count))
# 
# #カウント別カラー用のパレットの作成
# pal_o <- colorNumeric(palette="Spectral", domain=data_queries_o$log10_count, reverse=TRUE)
# 
# # 地図の作成
# map_o <-
#   map_d %>% 
#   addTiles() %>% 
#   addCircles(lng=data_queries_o$lng,lat=data_queries_o$lat,color=pal_o(data_queries_o$log10_count),radius=500,stroke=FALSE,fillOpacity = 0.6,group="o") %>% 
#   addLegend(position='topright', pal=pal_o, values=data_queries_o$log10_count,group = "o") %>% 
#   addScaleBar(position="bottomleft",options = scaleBarOptions(imperial=FALSE)) %>% 
#   addLayersControl(baseGroups=c("o","d"), options=layersControlOptions(collapsed = FALSE))

# バイクを地図上に表示----

map_mobike <-
  leaflet() %>% 
  addTiles() %>% 
  addCircles(lng=data_mobike$distX ,lat=data_mobike$distY)

map_mobike

data_mobike %>% 
  group_by(num) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

result_con

# # 目的地の緯度経度ごとのカウント数
# data_queries_d <-
#   data_queries %>% 
#   group_by(d) %>% 
#   summarise(count=n()) %>%
#   ungroup() %>% 
#   arrange(desc(count)) %>% 
#   separate(col=d,into=c("lng","lat"),sep=",") %>% 
#   mutate(lng=lng %>% as.numeric,lat=lat %>% as.numeric) %>% 
#   mutate(log10_count = log10(count))
# 
# #カウント別カラー用のパレットの作成
# pal_mobike <- colorNumeric(palette="Spectral", domain=data_queries_d$log10_count, reverse=TRUE)
# 
# # 地図の作成
# map_mobike <-
#   leaflet() %>% 
#   addTiles() %>% 
#   addCircles(lng=data_queries_d$lng,lat=data_queries_d$lat,color=pal_d(data_queries_d$log10_count),radius=500,stroke=FALSE,fillOpacity = 0.6,group="d") %>% 
#   addLegend(position='topright', pal=pal_d, values=data_queries_d$log10_count,group="d") %>% 
#   addScaleBar(position="bottomleft",options = scaleBarOptions(imperial=FALSE))
# 
# map_mobike

# APIのテスト----
result <- POST(
  url = "http://app.mobike.com/api/nearby/v4/nearbyBikeInfo",
  add_headers(
    "platform" = "1",
    "Content-Type" = "application/x-www-form-urlencoded",
    "User-Agent" = "User-Agent: Mozilla/5.0 (Android 7.1.2; Pixel Build/NHG47Q) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.9 NTENTBrowser/3.7.0.496 (IWireless-US) Mobile Safari/537.36"
  ),
  body = list(latitude="39.914824",longitude="116.435446",radius="1500"),
  encode = "form"
)

result_con <- result %>% content
result
result_con %>% list.tree

if(result$status != "200"){
  print(i)
  print("error")
}

result_con %>% list.tree
result_con$bike %>% list.tree

if(result_con$bike %>% map_chr(~.$distId) %>% length != 0){
  data_mobike_tmp <-
    data.frame(
      #num      = i,
      #location = tmp_location,
      distId   = result_con$bike %>% map_chr(~.$distId),
      distX    = result_con$bike %>% map_dbl(~.$distX),
      distY    = result_con$bike %>% map_dbl(~.$distY),
      distNum  = result_con$bike %>% map_int(~.$distNum),
      distance = result_con$bike %>% map_chr(~.$distance),
      bikeIds  = result_con$bike %>% map_chr(~.$bikeIds),
      biketype = result_con$bike %>% map_int(~.$biketype),
      type     = result_con$bike %>% map_int(~.$type),
      operateType = result_con$bike %>% map_int(~.$operateType)
      
    )
  data_mobike %<>% rbind(data_mobike_tmp)
}





seq(from=-0.004, to=0.004, by=0.002)
data_mobike %>% map(class)
, to=0.004, by=0.002)


