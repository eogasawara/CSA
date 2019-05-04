wf_map_discover_motifs <- function(relation) {
  setwd(relation$wd)
  source("csa/code/workflow_activities.R")
  folder <- as.character(relation$folder)
  dataset <- as.character(relation$dataset)
  w <- as.numeric(relation$w)
  a <- as.numeric(relation$a)
  sb <- as.numeric(relation$sb)
  tb <- as.numeric(relation$tb)
  si <- as.numeric(relation$si)
  ka <- as.numeric(relation$ka)
  dataset <- discovery_motifs(folder, dataset, w, a, sb, tb, si, ka)
  return(dataset)
}

wf_map_analyze_motifs <- function(relation) {
  setwd(relation$wd)
  source("csa/code/workflow_activities.R")
  folder <- as.character(relation$folder)
  dataset <- as.character(relation$dataset)
  w <- as.numeric(relation$w)
  a <- as.numeric(relation$a)
  sb <- as.numeric(relation$sb)
  tb <- as.numeric(relation$tb)
  si <- as.numeric(relation$si)
  ka <- as.numeric(relation$ka)
  dataset <- analyze_motifs(folder, dataset, w, a, sb, tb, si, ka)
  return(dataset)
}

wf_discover_motifs_toy <- function(sc) {
  data <- read.csv(file = "csa/toy/toy.csv")
  data$folder <- "/shared/eogasawara/csa/toy"
  data$dataset <- "csa/data/toy.RData"
  data$wd <- "/home/eogasawara"
  data$id <- 1:nrow(data)
  
  data_tbl <- copy_to(sc, data, overwrite = TRUE)
  
  dataset <- spark_apply(data_tbl, wf_map_discover_motifs, group_by="id") %>% collect
  save(dataset, file="/shared/eogasawara/csa/toy/log.RData")
  return(dataset)
}

wf_discover_motifs_seismic <- function(sc) {
  data <- read.csv(file = "csa/seismic/seismic.csv")
  data$folder <- "/shared/eogasawara/csa/seismic"
  data$dataset <- "csa/data/t401.RData"
  data$wd <- "/home/eogasawara"
  data$id <- 1:nrow(data)
  
  data_tbl <- copy_to(sc, data, overwrite = TRUE)
  
  dataset <- spark_apply(data_tbl, wf_map_discover_motifs, group_by="id") %>% collect
  save(dataset, file="/shared/eogasawara/csa/seismic/log.RData")
  return(dataset)
}

wf_analyze_motifs_seismic <- function(sc) {
  data <- read.csv(file = "csa/seismic/seismic.csv")
  data$folder <- "/shared/eogasawara/csa/seismic"
  data$dataset <- "/home/eogasawara/csa/data/t401.RData"
  data$wd <- "/home/eogasawara"
  data$id <- 1:nrow(data)
  
  data_tbl <- copy_to(sc, data, overwrite = TRUE)
  
  dataset <- spark_apply(data_tbl, wf_map_analyze_motifs, group_by="id") %>% collect
  save(dataset, file="csa/seismic/seismic-analysis.RData")
  return(dataset)
}

library(dplyr)
library(sparklyr)
Sys.setenv("SPARK_HOME" = "/usr/local/spark")
sc <- spark_connect(master = "spark://aldebaran:7077")

#dataset <- wf_discover_motifs_toy(sc)

dataset <- wf_discover_motifs_seismic(sc)

#dataset <- wf_analyze_motifs_seismic(sc)

spark_disconnect(sc)


