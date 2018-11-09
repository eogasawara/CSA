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
    sparkR.session(appName = sprintf("motif-%s", cfg))
    connectBackend.orig <- getFromNamespace('connectBackend', pos='package:SparkR')
    connectBackend.patched <- function(hostname, port, timeout = 3600*48) {
      connectBackend.orig(hostname, port, timeout)
    }
    assignInNamespace("connectBackend", value=connectBackend.patched, pos='package:SparkR')    
  }
} else {
  folder = "C:/Users/eduar/Dropbox/Aplicativos/ShareLaTeX/2018-10a-ESA-stmotifs-seismic/experiments/workflow"
  cfg = "seismic"
}

spark_motif_execute <- function(folder, cfg, dataset, w, a, sb, tb, si, ka) {
  start_time <- Sys.time()
  rstmotifs <- NULL
  motifs <- 0
  occurrences <- 0
  dnorm <- 0
  dsearch <- 0
  drank <- 0
  executed <- FALSE
  outputTry <- tryCatch({
    load(sprintf("%s/%s.RData", folder, dataset))
    DS <- NormSAX(D,a)
    dnorm <- Sys.time() - start_time
    stmotifs <- SearchSTMotifs(D,DS,w,a,sb,tb,si,ka)
    dsearch <- Sys.time() - start_time
    rstmotifs <- RankSTMotifs(stmotifs)
    drank <- Sys.time() - start_time
    save(rstmotifs, file=sprintf("%s/%s-%d-%d-%d-%d-%d-%d.RData", folder, dataset, w, a, sb, tb, si, ka))
    motifs <- length(rstmotifs)
    for (i in rstmotifs) {
      occurrences <- occurrences + nrow(i$vecst)
    }
    executed <- TRUE
  },error=function(e) { 
    executed <- FALSE
  })
  
  dtotal <- Sys.time() - start_time
  execution <- sprintf("%s,%d,%d,%d,%d,%d,%d,%d,%d,%.1f,%.1f,%.1f,%.1f\n", dataset, w, a, sb, tb, si, ka, motifs, occurrences, dnorm, dsearch, drank, dtotal)
  if (executed) {
    cat(execution, file=sprintf("%s/%s.log", folder, cfg), append = TRUE)
  }else{
    cat(execution, file=sprintf("%s/%s.err", folder, cfg), append = TRUE)
  }
  return(execution)  
}

launch <- function(list.args){
  dataset <- as.character(list.args$dataset)
  w <- as.numeric(list.args$w)
  a <- as.numeric(list.args$a)
  sb <- as.numeric(list.args$sb)
  tb <- as.numeric(list.args$tb)
  si <- as.numeric(list.args$si)
  ka <- as.numeric(list.args$ka)
  return(spark_motif_execute(folder, cfg, dataset, w, a, sb, tb, si, ka))
}

arg <- read.csv(file = sprintf("%s/%s.csv", folder, cfg))
if (!isSpark) {
  arg <- arg %>% filter(sb==20 & w==5 & si==4 & ka==3)
}

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
