library(ggplot2)
library(STMotif)

plot.series <- function(series, label_series = "", label_x = "", label_y = "") {
  grf <- ggplot(data=series, ggplot2::aes(x = series$x, y = series$value, colour = series$color, group = 1))
  grf <- grf + scale_colour_identity(series$color) + geom_line() + geom_point(data=series, aes(x = series$x, y = series$value), size=0.5,color='black') + facet_grid(variable ~ ., switch = "y")
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
  return(grf)
}

prep_plot <- function(dataset, rstmotifs, title="ST", fixcolor=NULL){
  space<- 1:12
  namesCol <- paste("X",colnames(dataset),sep = "")
  data <- as.data.frame(dataset[,space])
  colnames(data) <- paste(title,space, sep = "")
  data <- data.frame(x = 1:nrow(data),data)
  data <- reshape2::melt(data,id.vars = 1)
  data <- data.frame(data, color = "black")
  
  levels(data$color) <- c("black", "red2", "orange", "green2", "orange2")
  
  
  for (i in (1:length(rstmotifs))) {
    m <- rstmotifs[[i]]
    for (j in 1:nrow(m$vecst)) {
      pos <- (m$vecst$s[j]-1)*20+m$vecst$t[j]
      mycolor <- levels(data$color)[i+1]
      if (!is.null(fixcolor))
        mycolor <- fixcolor
      data$color[(pos:(pos+2))] <- mycolor
    }
  }
  
  grf <- plot.series(data[1:nrow(data),])
  plot(grf)
  
  return(data)
}

load("csa/data/toydataset.RData") 

#input toydataset
w  <- 4 
a  <- 5
si <- 2

ka <- 1 
tb <- 20
sb <- 1

D <- STSADatasetAdjust(D, tb, sb)
DS <- NormSAX(D,a)
stmotifs_sole <- SearchSTMotifs(D,DS,w,a,sb,tb,si,ka)
rstmotifs_sole <- RankSTMotifs(stmotifs_sole)

#grafico exportado em 5" x 4.25"
x <- prep_plot(D, rstmotifs_sole, fixcolor = "green2")

ka <- 2
tb <- 10
sb <- 4

DOrg <- D
D <- STSADatasetAdjust(DOrg, tb, sb)
DS <- NormSAX(D,a)
stmotifs <- SearchSTMotifs(D,DS,w,a,sb,tb,si,ka)
rstmotifs <- RankSTMotifs(stmotifs)
#grafico exportado em 5" x 4.25"
x <- prep_plot(DOrg, rstmotifs)

x$variable <- as.character(x$variable)

x$variable[x$variable=="ST1" & x$x <= 10] <- "CS11"
x$variable[x$variable=="ST2" & x$x <= 10] <- "CS11"
x$variable[x$variable=="ST3" & x$x <= 10] <- "CS11"
x$variable[x$variable=="ST4" & x$x <= 10] <- "CS11"
x$x[x$variable=="CS11"] <- c(1:40)

x$variable[x$variable=="ST1" & x$x > 10] <- "CS12"
x$variable[x$variable=="ST2" & x$x > 10] <- "CS12"
x$variable[x$variable=="ST3" & x$x > 10] <- "CS12"
x$variable[x$variable=="ST4" & x$x > 10] <- "CS12"
x$x[x$variable=="CS12"] <- c(1:40)

x$variable[x$variable=="ST5" & x$x <= 10] <- "CS21"
x$variable[x$variable=="ST6" & x$x <= 10] <- "CS21"
x$variable[x$variable=="ST7" & x$x <= 10] <- "CS21"
x$variable[x$variable=="ST8" & x$x <= 10] <- "CS21"
x$x[x$variable=="CS21"] <- c(1:40)

x$variable[x$variable=="ST5" & x$x > 10] <- "CS22"
x$variable[x$variable=="ST6" & x$x > 10] <- "CS22"
x$variable[x$variable=="ST7" & x$x > 10] <- "CS22"
x$variable[x$variable=="ST8" & x$x > 10] <- "CS22"
x$x[x$variable=="CS22"] <- c(1:40)

x$variable[x$variable=="ST9" & x$x <= 10] <- "CS31"
x$variable[x$variable=="ST10" & x$x <= 10] <- "CS31"
x$variable[x$variable=="ST11" & x$x <= 10] <- "CS31"
x$variable[x$variable=="ST12" & x$x <= 10] <- "CS31"
x$x[x$variable=="CS31"] <- c(1:40)

x$variable[x$variable=="ST9" & x$x > 10] <- "CS32"
x$variable[x$variable=="ST10" & x$x > 10] <- "CS32"
x$variable[x$variable=="ST11" & x$x > 10] <- "CS32"
x$variable[x$variable=="ST12" & x$x > 10] <- "CS32"
x$x[x$variable=="CS32"] <- c(1:40)

x <- x[with(x, order(variable, x)),]

#graficos exportados em 5" x 3"
grf <- plot.series(x)
plot(grf)
