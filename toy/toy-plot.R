#packages
#install.packages("ggplot2")
#library(ggplot2)


plot.series <- function(series, label_series = "", label_x = "", label_y = "") {
  grf <- ggplot(data=series, ggplot2::aes(x = series$x, y = series$value, colour = series$color, group = 1))
  grf <- grf + scale_colour_identity(series$color) + geom_line() + geom_point(data=series, aes(x = series$x, y = series$value), size=0.5,color='black') + facet_grid(variable ~ ., switch = "y")
  grf <- grf + xlab(label_x)
  grf <- grf + ylab(label_y)
  #grf <- grf + ggtitle("Motifs in spatial-time series")
  grf <- grf + theme_bw(base_size = 10)
  grf <- grf + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
  grf <- grf + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
  return(grf)
}

space<- 1:12
namesCol <- paste("X",colnames(dataset),sep = "")
data <- as.data.frame(dataset[,space])
colnames(data) <- paste("CS",space, sep = "")
data <- data.frame(x = 1:nrow(data),data)
data <- reshape2::melt(data,id.vars = 1)
data <- data.frame(data, color = "black")
levels(data$color) <- c("black", "red2","blue2", "green2")

#ST coloridas 
#1
st<-0
data[(st+1):(st+4),][4]<- "red2"
data[(st+11):(st+11+3),][4]<- "blue2"
data[(st+16):(st+16+3),][4]<- "green2"
#2
st<-20
data[(st+1):(st+4),][4]<- "red2"
data[(st+11):(st+11+3),][4]<- "blue2"
data[(st+16):(st+16+3),][4]<- "green2"
#3
st<-40
data[(st+1):(st+4),][4]<- "red2"
data[(st+6):(st+6+3),][4]<- "blue2"
data[(st+11):(st+11+3),][4]<- "blue2"
data[(st+16):(st+16+3),][4]<- "green2"
#4
st<-60
data[(st+1):(st+4),][4]<- "red2"
data[(st+6):(st+6+3),][4]<- "blue2"
data[(st+11):(st+11+3),][4]<- "green2"
#5
st<-80
data[(st+1):(st+4),][4]<- "red2"
data[(st+11):(st+11+3),][4]<- "green2"
data[(st+16):(st+16+3),][4]<- "green2"
#6
st<-100
data[(st+1):(st+4),][4]<- "red2"
#7
st<-120
data[(st+1):(st+4),][4]<- "red2"
data[(st+11):(st+11+3),][4]<- "blue2"
#8
st<-140
data[(st+6):(st+6+3),][4]<- "red2"
data[(st+11):(st+11+3),][4]<- "blue2"
data[(st+16):(st+16+3),][4]<- "green2"
#9
st<-160
data[(st+6):(st+6+3),][4]<- "red2"
data[(st+11):(st+11+3),][4]<- "green2"
data[(st+16):(st+16+3),][4]<- "green2"
#10
st<-180
data[(st+6):(st+6+3),][4]<- "red2"
data[(st+11):(st+11+3),][4]<- "blue2"
data[(st+16):(st+16+3),][4]<- "red2"
#11
st<-200
data[(st+6):(st+6+3),][4]<- "blue2"
data[(st+11):(st+11+3),][4]<- "blue2"
data[(st+16):(st+16+3),][4]<- "red2"
#12
st<-220
data[(st+6):(st+6+3),][4]<- "blue2"
data[(st+11):(st+11+3),][4]<- "blue2"
data[(st+16):(st+16+3),][4]<- "red2"
plot.series(data[1:nrow(data),])

dataOriginal<- data

#Combined Series
data[ 1:10,2]<-"CS1"
data[ 1:10,1]<-1:10
data[21:30,2]<-"CS1"
data[21:30,1]<-11:20
data[41:50,2]<-"CS1"
data[41:50,1]<-21:30
data[61:70,2]<-"CS1"
data[61:70,1]<-31:40

data[11:20,2]<-"CS2"
data[11:20,1]<-1:10
data[31:40,2]<-"CS2"
data[31:40,1]<-11:20
data[51:60,2]<-"CS2"
data[51:60,1]<-21:30
data[71:80,2]<-"CS2"
data[71:80,1]<-31:40

data[81:90,2]<-"CS3"
data[81:90,1]<-1:10
data[101:110,2]<-"CS3"
data[101:110,1]<-11:20
data[121:130,2]<-"CS3"
data[121:130,1]<-21:30
data[141:150,2]<-"CS3"
data[141:150,1]<-31:40

data[91:100,2]<-"CS4"
data[91:100,1]<-1:10
data[111:120,2]<-"CS4"
data[111:120,1]<-11:20
data[131:140,2]<-"CS4"
data[131:140,1]<-21:30
data[151:160,2]<-"CS4"
data[151:160,1]<-31:40

data[161:170,2]<-"CS5"
data[161:170,1]<-1:10
data[181:190,2]<-"CS5"
data[181:190,1]<-11:20
data[201:210,2]<-"CS5"
data[201:210,1]<-21:30
data[221:230,2]<-"CS5"
data[221:230,1]<-31:40

data[171:180,2]<-"CS6"
data[171:180,1]<-1:10
data[191:200,2]<-"CS6"
data[191:200,1]<-11:20
data[211:220,2]<-"CS6"
data[211:220,1]<-21:30
data[231:240,2]<-"CS6"
data[231:240,1]<-31:40

dataB<- NULL
dataB<- rbind(dataB, data[data[["variable"]]=="CS1",])
dataB<- rbind(dataB, data[data[["variable"]]=="CS2",])
dataB<- rbind(dataB, data[data[["variable"]]=="CS3",])
dataB<- rbind(dataB, data[data[["variable"]]=="CS4",])
dataB<- rbind(dataB, data[data[["variable"]]=="CS5",])
dataB<- rbind(dataB, data[data[["variable"]]=="CS6",])

plot.series(dataB[1:nrow(dataB),])
plot.series(dataB[1:nrow(dataB[dataB[["variable"]]=="CS2",]),])
