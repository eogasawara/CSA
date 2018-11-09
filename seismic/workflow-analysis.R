source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/stmotif.R")
args = commandArgs(trailingOnly=TRUE)

isSpark <- FALSE
if (length(args)) {
  args = commandArgs(trailingOnly=TRUE)  
  folder = args[1]
  cfg = args[2]
  isSpark = (length(args)==2)
  
  if (isSpark) {
    library(SparkR)
    sparkR.session(appName = sprintf("motif-analysis-%s", cfg))
    connectBackend.orig <- getFromNamespace('connectBackend', pos='package:SparkR')
    connectBackend.patched <- function(hostname, port, timeout = 3600*48) {
      connectBackend.orig(hostname, port, timeout)
    }
    assignInNamespace("connectBackend", value=connectBackend.patched, pos='package:SparkR')    
  }
} else {
  folder = "C:/Users/eduar/Dropbox/Aplicativos/ShareLaTeX/2018-10a-ESA-stmotifs-seismic/experiments/workflow"
  cfg = "seismic"
  setwd("C:/Users/eduar/Dropbox/Aplicativos/ShareLaTeX/2018-10a-ESA-stmotifs-seismic/experiments")
}

si.max <- 0
analysis <- function(folder, cfg, dataset, w, a, sb, tb, si, ka) {
  library(dplyr)
  load(sprintf("%s/%s-%d-%d-%d-%d-%d-%d.RData", folder, dataset, w, a, sb, tb, si, ka))
  
  motifs <- NULL
  for (mot in rstmotifs) {
    occ <- nrow(mot$vecst)
    x <- as.vector(mot$recmatrix)
    x[x>0] <- 1
    motifs <- rbind(motifs, data.frame(motif=mot$isaxcod,occ=occ, blocks=sum(x)))
  }
  
  general <- motifs %>% 
    summarize(motifs.g = n(), occ.g = sum(occ), blocks.g = sum(blocks))
  
  general$motifs.gu = length(unique(motifs$motif))

  motifs <- motifs %>% filter(occ > si.max & blocks > 1)
  
  motifs <- motifs %>% 
    group_by(motif) %>%
    summarize(occ = sum(occ), blocks = sum(blocks))

  motifs <- motifs %>%
    summarize(motifs.s = n(), occ.s = sum(occ), blocks.s = sum(blocks))

  motifs$dataset = dataset
  motifs$w = w
  motifs$a = a
  motifs$sb = sb
  motifs$tb = tb
  motifs$si = si
  motifs$ka = ka
  motifs$motifs.gu = general$motifs.gu
  motifs$motifs.g = general$motifs.g
  motifs$occ.g = general$occ.g
  motifs$blocks.g = general$blocks.g
  
  motifs <- motifs %>% select(w, sb, tb, si, ka, motifs.gu, motifs.g, occ.g, blocks.g, motifs.s, occ.s, blocks.s)
  
  return(motifs)
}

launch <- function(list.args){
  dataset <- as.character(list.args$dataset)
  w <- as.numeric(list.args$w)
  a <- as.numeric(list.args$a)
  sb <- as.numeric(list.args$sb)
  tb <- as.numeric(list.args$tb)
  si <- as.numeric(list.args$si)
  ka <- as.numeric(list.args$ka)
  return(analysis(folder, cfg, dataset, w, a, sb, tb, si, ka))
}

arg <- read.csv(file = sprintf("%s/%s.csv", folder, cfg))
if (!isSpark) {
  arg <- arg %>% filter(sb==20 & w==5 & si==4 & ka==3)
}  
si.max <- max(arg$si)

list <- list()
for(i in 1:nrow(arg)){
    list[[i]] <- arg[i,]
}

if (isSpark) {
  output <- spark.lapply(list,launch)
  sparkR.session.stop()  
} else {
  output <- lapply(list,launch)
}

dataset <- NULL
for (i in output) {
  dataset <- rbind(dataset, i)  
}

save(dataset, file=sprintf("%s/workflow_analysis.RData", folder))

