rm(list=ls())

setwd('~/Documents/GIT/robo_transporte/')

library(jsonlite)
library(rgdal)
library(lubridate)
library(ggmap)
library(rgeos)
library(dbscan)
library(ggplot2)
library(viridis)
library(gridExtra)
library(data.table)
library(dplyr)
library(gridExtra)
library(ggpmisc)
library(plotly)


source('source/lee_limplia_json.R')

source('source/procesa_lineas.R')

source('source/analisis_exploratorio.R')

source('source/clusters_interseccion_con_rutas.R')

source('source/interactivos_mapas_cluster.R')
