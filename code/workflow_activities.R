discovery_motifs <- function(folder, dataset, w, a, sb, tb, si, ka) {
  library(STMotif)

  start_time <- Sys.time()
  rstmotifs <- NULL
  motifs <- 0
  occurrences <- 0
  dnorm <- 0
  dsearch <- 0
  drank <- 0
  cfg <- sprintf("%s/%d-%d-%d-%d-%d-%d.RData", folder, w, a, sb, tb, si, ka)
  executed <- FALSE
  outputTry <- tryCatch({
    load(dataset)
    DS <- NormSAX(D,a)
    dnorm <- as.numeric(difftime(Sys.time(), start_time, unit="mins"))
    stmotifs <- SearchSTMotifs(D,DS,w,a,sb,tb,si,ka)
    dsearch <- as.numeric(difftime(Sys.time(), start_time, unit="mins"))
    rstmotifs <- RankSTMotifs(stmotifs)
    drank <- as.numeric(difftime(Sys.time(), start_time, unit="mins"))
    save(rstmotifs, file=cfg)
    motifs <- length(rstmotifs)
    for (i in rstmotifs) {
      occurrences <- occurrences + nrow(i$vecst)
    }
    executed <- TRUE
  },error=function(e) { 
    executed <- FALSE
  })
  
  dtotal <- as.numeric(difftime(Sys.time(), start_time, unit="mins"))
  
  dataset <- data.frame(dataset, w, a, sb, tb, si, ka, motifs, occurrences, dnorm, dsearch, drank, dtotal)
  return(dataset)  
}

analyze_motifs <- function(folder, dataset, w, a, sb, tb, si, ka) {
  library(dplyr)
  si.max <- 0
  cfg <- sprintf("%s/%d-%d-%d-%d-%d-%d.RData", folder, w, a, sb, tb, si, ka)
  load(cfg)
  
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

