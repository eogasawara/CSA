source("csa/code/stmotif.R")

library(dplyr)
library(readr)

folder <- "/shared/eogasawara/csa/seismic"

curvature.max <- function(x, y, df=3, do_plot=TRUE) {
  smodel = smooth.spline(x, y, df = df)
  curvature = predict(smodel, x = x, deriv = 2)
  yv = max(curvature$y)
  xv = match(yv,curvature$y)
  if (do_plot) {
    plot(x, y)
    points(x[xv], y[xv], pch=19)
  }
  res = data.frame(x[xv], y[xv], yv)
  colnames(res) = c("x", "y", "z")
  return (res)
}

seismicexec <- get(load("/shared/eogasawara/csa/seismic/log.RData"))
dataset <- get(load("csa/seismic/seismic-analysis.RData"))
seismicexec <- merge(dataset, seismicexec, by.x=c('w', 'sb', 'tb', 'si', 'ka'), by.y=c('w', 'sb', 'tb', 'si', 'ka'), all.x = TRUE)

result <- seismicexec %>% 
  group_by(sb, tb) %>%
  summarize(qtd.m = round(mean(motifs_gu),digits=0), 
            qtd.mg = round(mean(motifs_g),digits=0), 
            qtd.ms = round(mean(motifs_s),digits=0), 
            time.n = round(mean(dnorm),digits=1), 
            time.d = round(mean(dsearch-dnorm),digits=1), 
            time.r = round(mean(drank-dsearch),digits=1), 
            time.t = round(mean(dtotal),digits=1), 
            total = n()) 
overall <- result[with(result, order(sb, tb)),]

disclosure <- seismicexec %>% 
  group_by(sb, tb, w) %>%
  summarize(motifs.s = round(mean(motifs_s),digits=0), 
            occ.s = round(mean(occ_s),digits=0), 
            time.t = round(mean(dtotal),digits=1), 
            total = n()) 
disclosure <- disclosure[with(disclosure, order(sb, tb, w)),]

#sb == 20, w == 4
result <- seismicexec %>% 
  filter(sb == 20&w==4) %>%
  group_by(si, ka) %>%
  summarize(qtd.m = round(mean(motifs_s),digits=0), qtd.o = round(mean(occ_s),digits=0), time = round(mean(dtotal),digits=1), total = n()) 
result <- result[with(result, order(si, ka)),]

#sb == 20, w == 4, si == 3, ka == 3

filename <- sprintf("%s/%d-%d-%d-%d-%d-%d.RData", "/shared/eogasawara/csa/seismic", 4, 7, 20, 20, 3, 3)
load(filename)

motifs <- list()
i <- 0
k <- 0
cmotifs <- NULL
for (mot in rstmotifs) {
  i <- i + 1
  x <- as.vector(mot$recmatrix)
  occ <- nrow(mot$vecst)
  x[x>0] <- 1
  if ((sum(x)>1) && (occ > 7)) {
    k <- k + 1
    motifs[[k]] <- mot
    cmotifs <- rbind(cmotifs, data.frame(motif=mot$isaxcod, occ = occ, i=i))
  }
}

cmotifs <- cmotifs %>% group_by(motif) %>% summarize(i=min(i), occ=sum(occ))

extractMotifRank <- function(rstmotifs, rank) {
  k <- 0
  motifs <- list()
  i <- c()
  for (mot in rstmotifs) {
    j <- 0
    if (length(i) >= 0 && sum(as.integer(i==mot$isaxcod))>0)
      j <- 1
    if ((j == 0) && (k < rank)) {
      x <- as.vector(mot$recmatrix)
      occ <- nrow(mot$vecst)
      x[x>0] <- 1
      if ((sum(x)>1) && (occ > 7)) {
        i <- c(i, mot$isaxcod)
        k <- k + 1
        motifs[[k]] <- mot
      }
    }
  }
  return(motifs)
}

motifs <- extractMotifRank(rstmotifs, 5)



