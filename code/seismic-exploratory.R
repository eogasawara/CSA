source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphics.R")

loadlibrary("reshape")
loadlibrary("RColorBrewer")
loadlibrary("gridExtra")
loadlibrary("dplyr")
loadlibrary("STMotif")
source("csa/code/stmotif.R")

folder = "/shared/eogasawara/csa/seismic"

D <- get(load(file = "csa/data/t401.RData"))

slice_netherlands <- function(t) {
  inline <- t
  t <- (462-440+1):462
  x <- 15:934
  inline = inline[t, x]
  return(inline)
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

inline <- D
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

