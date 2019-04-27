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
library(mlr)

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
data_queries  <- fread("data_set_phase1/train_queries.csv",  stringsAsFactors=FALSE, sep=",")
data_plans    <- fread("data_set_phase1/train_plans_r2.csv", stringsAsFactors=FALSE, sep=",")
data_clicks   <- fread("data_set_phase1/train_clicks.csv",   stringsAsFactors=FALSE, sep=",")
data_profiles <- fread("data_set_phase1/profiles.csv",       stringsAsFactors=FALSE, sep=",")

# plansのフォーマットなおす
data_plans %<>% mutate(plan_time = ymd_hms(plan_time))

data_queries
data_profiles

# dataを結合してtrainデータを作成

data_train <-
  data_queries %>% 
  inner_join(data_plans,by="sid") %>% 
  left_join(data_profiles,by="pid") %>% 
  left_join(
    data_clicks %>% mutate(flag_click = 1),
    by=c("sid","transport_mode"="click_mode")
  )

# 関係のない列を排除
data_train %<>%
  select(-sid,-pid,-req_time,-o,-d,-plan_time,-click_time)

# 欠損値補完----
# flag_click & price のNAは0で埋める
# profile系のNAは最頻値で埋める
data_train %<>% 
  replace_na(list(
      flag_click = 0
    , price = 0
  )) %>% 
  mlr::impute(
    classes=list(
      numeric = imputeMode()
    )
  ) %>% .$data

# カテゴリカル変数をone-hot encoding
data_train %<>%
  mutate_at(
    .vars = c("flag_click","transport_mode"),
    .funs = as.factor
  ) %>% 
  createDummyFeatures(target = "flag_click")


# タスクの定義----
traffic.task = makeClassifTask(id="traffic",data=data_train,target="flag_click")
traffic.task %>% print

# 学習器の作成----
traffic.lrns = makeLearner("classif.xgboost", id="xgboost")

# チューニング前の各アルゴリズムの性能を評価----
traffic.bmr = benchmark(
  learners    = traffic.lrns,
  tasks       = traffic.task,
  resamplings = cv5,
  measures    = list(acc,timetrain)
)

# ベンチマークテストの結果をグラフで確認----
g1<-
  plotBMRBoxplots(
    traffic.bmr,
    measure = acc,
    style="violin"
  ) +
  aes(color = learner.id) +
  theme(strip.text.x = element_text(size = 6))

g2<-
  plotBMRBoxplots(
    traffic.bmr,
    measure = timetrain,
    style="violin"
  ) +
  aes(color = learner.id) +
  theme(strip.text.x = element_text(size = 6))
print(g1)
print(g2)

data_train
?mutate_at
data_train %>% View

?mlr::impute









