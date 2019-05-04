library(xlsx)
source("csa/code/stmotif.R")
mydata <- read.xlsx("csa/data/toydataset.xlsx", "data")

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
D <- vectorMatrix
save(D, file = "csa/data/toydataset.RData") 

