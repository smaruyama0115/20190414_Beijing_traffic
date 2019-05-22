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

# queryの緯度経度情報を取得
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
  separate(col=location,into=c("lng","lat"),sep=",")



# APIのテスト----
result <- POST(
  url = "http://app.mobike.com/api/nearby/v4/nearbyBikeInfo",
  add_headers(
    "platform" = "1",
    "Content-Type" = "application/x-www-form-urlencoded",
    "User-Agent" = "User-Agent: Mozilla/5.0 (Android 7.1.2; Pixel Build/NHG47Q) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.9 NTENTBrowser/3.7.0.496 (IWireless-US) Mobile Safari/537.36"
  ),
  body = list(latitude="39.914824",longitude="116.435446"),
  encode = "form"
)

result_con <- result %>% content

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
  #data_poi_automobile_rental %<>% rbind(data_poi_tmp)
}

data_poi_tmp












