setwd("C:/Users/eduar/Dropbox/Aplicativos/ShareLaTeX/2018-10a-ESA-stmotifs-seismic/experiments")
library(xlsx)
library(readxl)

## CRIAÇÃO DO ARQUIVO RDATA APARTIR DO EXCEL COM O SAX
STSNormalization <- function (vector){
  return ((vector-mean(vector, na.rm = T))/sd(vector, na.rm = T))
}

# binning the dataset
# Build an encode for the values
binning <- function(v, alpha) {
  p <- seq(from = 0, to = 1, by = 1/alpha)
  q <- quantile(v, p)
  qf <- matrix(c(q[1:(length(q)-1)],q[2:(length(q))]), ncol=2)
  vp <- cut(v, unique(q), FALSE, include.lowest=TRUE)
  m <- tapply(v, vp, mean)
  vm <- m[vp]
  mse <- mean( (v - vm)^2, na.rm = TRUE)
  return (list(binning=m, bins_factor=vp, q=q, qf=qf, bins=vm, mse=mse))
}

STSSaxEncode <- function(dataset, vector, alpha) {
  mybin <- binning(vector, alpha)
  myletters <- letters[1:alpha]
  saxvector <- myletters[mybin$bins_factor]
  saxvector = matrix(saxvector, nrow = nrow(dataset), ncol = ncol(dataset))
  saxvector = data.frame(saxvector)
  colnames(saxvector) =  colnames(dataset)
  return(saxvector)
}

mydata <- read.xlsx("toy/toydataset.xlsx", "Versao5-SAX")
mydata$NA. <- NULL
vector <- as.matrix(mydata)
vector <- as.vector(vector)
vectorCorreto <- vector
alpha <- 5
tabela <- table(vector)

vector<-replace(vector, vector=="a", -10)
vector<-replace(vector, vector=="b", 105)
vector<-replace(vector, vector=="c",470)
vector<-replace(vector, vector=="d",790)
vector<-replace(vector, vector=="e",1000)
vector<-as.numeric(vector)

vectorNorm <- STSNormalization(vector)
saxdataframe <- STSSaxEncode(mydata, vectorNorm, alpha)

saxdataset <- as.matrix(saxdataframe)
saxdataset <- as.vector(saxdataset)
all(saxdataset==vectorCorreto)
errados <- which(saxdataset!=vectorCorreto)
print(c(length(errados), errados))

vectorMatrix = matrix(vector, nrow = 20, ncol = 12)
save(vectorMatrix, file = "toy/toydataset.RData") 

