STSNormalization <- function (vector){
  return ((vector-mean(vector, na.rm = T))/sd(vector, na.rm = T))
}

binning <- function(v, a) {
  p <- seq(from = 0, to = 1, by = 1/a)
  q <- quantile(v, p)
  qf <- matrix(c(q[1:(length(q)-1)],q[2:(length(q))]), ncol=2)
  vp <- cut(v, unique(q), FALSE, include.lowest=TRUE)
  m <- tapply(v, vp, mean)
  vm <- m[vp]
  mse <- mean( (v - vm)^2, na.rm = TRUE)
  return (list(binning=m, bins_factor=vp, q=q, qf=qf, bins=vm, mse=mse))
}

STSSaxEncode <- function(dataset, vector, a) {
  mybin <- binning(vector, a)
  myletters <- letters[1:a]
  saxvector <- myletters[mybin$bins_factor]
  saxvector = matrix(saxvector, nrow = nrow(dataset), ncol = ncol(dataset))
  saxvector = data.frame(saxvector)
  colnames(saxvector) =  colnames(dataset)
  return(saxvector)
}

