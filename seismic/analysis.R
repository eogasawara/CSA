source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/stmotif.R")

library(dplyr)
library(readr)

folder = "C:/Users/eduar/Dropbox/Aplicativos/ShareLaTeX/2018-10a-ESA-stmotifs-seismic/experiments"
cfg = "seismic"
setwd(folder)

check_delta <- function() {
  seismicexec <- read_csv("workflow/seismic.log", col_names = FALSE)
  colnames(seismicexec) <- c("dataset","w","a","sb","tb","si","ka", "motifs", "occurrences", "dnorm", "dsearch","drank","dtotal")
  
  dataset <- merge(seismic, seismicexec, by.x=c('dataset', 'w', 'a', 'sb', 'tb', 'si', 'ka'), by.y=c('dataset', 'w', 'a', 'sb', 'tb', 'si', 'ka'), all.x = TRUE)
  dataset <- dataset[is.na(dataset$dnorm),]
  
  dataset$dnorm <- NULL
  dataset$dsearch <- NULL
  dataset$drank <- NULL
  dataset$dtotal <- NULL
  dataset$motifs <- NULL
  dataset$occurrences <- NULL
  
  write_csv(dataset, "workflow/seismic-delta.csv")
}

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

seismic <- read_csv("workflow/seismic.csv")
seismicexec <- read_csv("workflow/seismic.log")
load("workflow/workflow_analysis.RData")
seismicexec <- merge(dataset, seismicexec, by.x=c('w', 'sb', 'tb', 'si', 'ka'), by.y=c('w', 'sb', 'tb', 'si', 'ka'), all.x = TRUE)

result <- seismicexec %>% 
  group_by(sb, tb) %>%
  summarize(qtd.m = round(mean(motifs.gu),digits=0), 
            qtd.mg = round(mean(motifs.g),digits=0), 
            qtd.ms = round(mean(motifs.s),digits=0), 
            time.n = round(mean(dnorm),digits=1), 
            time.d = round(mean(dsearch-dnorm),digits=1), 
            time.r = round(mean(drank-dsearch),digits=1), 
            time.t = round(mean(dtotal),digits=1), 
            total = n()) 
overall <- result[with(result, order(sb, tb)),]

disclosure <- seismicexec %>% 
  group_by(sb, tb, w) %>%
  summarize(motifs.s = round(mean(motifs.s),digits=0), 
            occ.s = round(mean(occ.s),digits=0), 
            time.t = round(mean(dtotal),digits=1), 
            total = n()) 
disclosure <- disclosure[with(disclosure, order(sb, tb, w)),]

#sb == 20, w == 4
result <- seismicexec %>% 
  filter(sb == 20&w==4) %>%
  group_by(si, ka) %>%
  summarize(qtd.m = round(mean(motifs.s),digits=0), qtd.o = round(mean(occ.s),digits=0), time = round(mean(dtotal),digits=1), total = n()) 

result <- result[with(result, order(si, ka)),]

#sb == 20, w == 4, si == 3, ka == 3

filename <- sprintf("%s/%s-%d-%d-%d-%d-%d-%d.RData", "workflow", "t401", 4, 7, 20, 20, 3, 3)
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



