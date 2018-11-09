setwd("C:/Users/eduar/Dropbox/Aplicativos/ShareLaTeX/2018-10a-ESA-stmotifs-seismic/experiments")

source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphics.R")

loadlibrary("reshape")
loadlibrary("RColorBrewer")
loadlibrary("gridExtra")
loadlibrary("dplyr")

load("t401.RData")


STSNormalization <- function (vector){
  return ((vector-mean(vector, na.rm = T))/sd(vector, na.rm = T))
}

binning <- function(v, alpha) {
  p <- seq(from = 0, to = 1, by = 1/alpha)
  q <- quantile(v, p)
  qf <- matrix(c(q[1:(length(q)-1)],q[2:(length(q))]), ncol=2)
  vp <- cut(v, q, FALSE, include.lowest=TRUE)
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


inline <- t
t <- (462-440+1):462
x <- 15:934
inline = inline[t, x]

vector <- as.matrix(inline)
vector <- as.vector(vector)
vectorNorm <- STSNormalization(vector)

alpha <- c(1:25)
mse <- alpha
for (i in alpha) {
  mybin <- binning(vector, i)
  mse[i] <- mybin$mse
}

cm <- curvature.max(alpha, mse)

d <- data.frame(alpha = alpha, mse = mse)

h <- c(alpha[cm$x], mse[cm$x])

grf <- ggplot() + theme_bw(base_size = 10) +
  geom_point(data = d, aes(alpha, mse), colour = "black", size = 1) +
  geom_point(data = d[h, ], aes(alpha, mse), colour = "red", size = 2) + 
  theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom") + theme(legend.key = element_blank()) 
options(repr.plot.width=4, repr.plot.height=3)
ggsave(grf,file="mse.pdf",height=3,width=4)
plot(grf)

series <- data.frame(value=vector)
grf <- plot.density(series, label_series = "distribuição", colors="gray")
options(repr.plot.width=4, repr.plot.height=3)
ggsave(grf,file="density.pdf",height=3,width=4)
plot(grf)

