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

data_address <- read_csv("data_set_phase1/address-info-clean2.csv",locale=locale(encoding="SJIS"))

data_address %>% View()
