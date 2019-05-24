# sidとplanの時間差とclicksに関係があるのではないかと思って分析したが、
# そもそも時間差がまったくなかった

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
data_plans   <- fread("data_set_phase1/train_plans_r2.csv", stringsAsFactors=FALSE, sep=",")
data_clicks  <- fread("data_set_phase1/train_clicks.csv", stringsAsFactors=FALSE, sep=",")

test_queries <- fread("data_set_phase1/test_queries.csv", stringsAsFactors=FALSE, sep=",")

data_queries

data_plans_unique <-
  data_plans %>% 
  select(sid,plan_time) %>% 
  mutate(plan_time = ymd_hms(plan_time)) %>% 
  distinct()

data_queries_plans <-
  data_queries %>%
  mutate(req_time = ymd_hms(req_time)) %>% 
  inner_join(data_plans_unique,by="sid") %>% 
  mutate(diff = plan_time - req_time) %>% 
  left_join(data_clicks,by="sid")

data_queries_plans$diff %>% table
  
data_queries_plans %>% 
  ggplot(aes(x=diff %>% as.factor,fill=click_mode %>% as.factor)) +
  geom_bar(stat="count",position="fill")








