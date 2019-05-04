source("csa/code/stmotif.R")

library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(scales)

folder = "/shared/eogasawara/csa/seismic"

D <- get(load(file = "csa/data/t401.RData"))
dataset<- D
alpha<-25
colorEncode <- 1:alpha
datasetColor.Org <- as.matrix(dataset)
datasetColor.Org <- as.vector(datasetColor.Org)
datasetColor.Org <- STSNormalization(datasetColor.Org)
mybin <- binning(datasetColor.Org, alpha)
datasetColor.Org <- colorEncode[mybin$bins_factor]
datasetColor.Org <- t(matrix(datasetColor.Org, nrow = nrow(dataset), ncol = ncol(dataset)))
datasetColor.Org <- melt(datasetColor.Org)
datasetColor.Org$motif <- FALSE

filename <- sprintf("%s/%d-%d-%d-%d-%d-%d.RData", folder, 4, 7, 20, 20, 3, 3)
load(filename)

motifs <- list()
i <- 0
cmotifs <- NULL
for (mot in rstmotifs) {
  x <- as.vector(mot$recmatrix)
  occ <- nrow(mot$vecst)
  x[x>0] <- 1
  if ((sum(x)>1) && (occ > 7)) {
    i <- i + 1
    motifs[[i]] <- mot
    cmotifs <- rbind(cmotifs, data.frame(motif=mot$isaxcod, occ = occ, i=i))
  }
}
cmotifs <- cmotifs %>% group_by(motif) %>% summarize(i=min(i), occ=sum(occ))
cmotifs <- cmotifs[with(cmotifs, order(occ,decreasing=TRUE)),]


extractMotifOccurrences <- function(rstmotifs, cmotifs, i, hist.plot=FALSE) {
  motifs <- list()
  r <- NULL
  for (mot in rstmotifs) {
    if (mot$isaxcod %in% i) {
      r <- c(r,mot$rank$proj)
      x <- as.vector(mot$recmatrix)
      occ <- nrow(mot$vecst)
      x[x>0] <- 1
      if ((sum(x)>1) && (occ > 7) && (mot$rank$proj >= 1)) {
        k <- which(i == mot$isaxcod)
        if ((k > length(motifs)) || is.null(motifs[[k]])) {
          motifs[[k]] <- mot
        } else {
          t <- motifs[[k]]
          t$vecst <- rbind(t$vecst, mot$vecst)
          motifs[[k]] <- t
        }
      }
    }
  }
  for (i in length(motifs):1) {
    if (is.null(motifs[[i]]))
      motifs[[i]] <- NULL
  }
  hist(r, plot=hist.plot)
  return(motifs)
}

motifs <- extractMotifOccurrences(rstmotifs, cmotifs, cmotifs$motif[1:10], hist.plot = TRUE)

palhetaCores <- brewer.pal(length(motifs), 'Spectral')

tam <- length(motifs)
motifs.plot <-data.frame("s"=NULL, "t"=NULL, "g"= NULL)
for (pos in 1:length(motifs)){
  motifs.plot<- rbind(motifs.plot ,data.frame("s"=motifs[[pos]]$vecst$s, "t"=motifs[[pos]]$vecst$t, "g"= pos, "color"=palhetaCores[pos])) 
}

datasetColor <- merge(datasetColor.Org, motifs.plot, by.x=c('Var1', 'Var2'), by.y=c('s', 't'), all.x = TRUE)
datasetColor$motif[!is.na(datasetColor$g)] <- TRUE
datasetColor$g <- NULL
datasetColor$color <- as.character(datasetColor$color)

p <- NULL
p <- ggplot(data=datasetColor, aes(x=datasetColor$Var1, y=datasetColor$Var2, fill=datasetColor$value, color=datasetColor$color)) 
p <-  p + geom_raster() 
p <-  p + scale_fill_gradientn(colours = c("white","dimgrey"), values = scales::rescale(1:alpha), limits=c(1,alpha))  
p <-  p + theme_bw() + xlab("Space") + ylab("Time") + scale_y_reverse()
p <-  p + guides(fill=FALSE, color=FALSE)
p <-  p + geom_point(colour = ifelse(datasetColor$motif,datasetColor$color,NA), size = 1, show.legend = FALSE)
plot(p)











