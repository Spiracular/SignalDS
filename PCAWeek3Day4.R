library(ggplot2)
library(dplyr)
library(corrplot)

set.seed(1)
X = matrix(sample(0:1, 100, replace = TRUE), nrow = 10)
X = scale(X)
w = 1:10


objective = function(w,X) {
  #Takes in data frame X
  # X = matrix(X)
  (t(w) %*% t(X) %*% X %*% w)/(t(w) %*% w)
}

objective(w, X)

vnorm = function(v) {
  sqrt(sum(v^2))
}

################################################
# Extracting the first principal component
################################################

extract_pc = function(X, method) {
  opt = optim(rep(1, ncol(X)), objective, method = method, control = list(maxit = 10000, fnscale = -1), X = X)
  opt$par/vnorm(opt$par)
}

opt = extract_pc(X, "CG")

prcompv = prcomp(X)$rotation[,1]

angle = function(x, y) {
  z = acos(sum(x*y)/vnorm(x)/vnorm(y))*180/pi
  return(min(z, 180-z))
}

angle(opt,prcompv)

################################################
# Extracting every principal component
################################################

remove_pc = function(Xorig, Xinit, pc) {
  Xinit - Xorig %*% (pc %*% t(pc))
}

norm(remove_pc(X, X, 1:10))

extract_all = function(X, method) {
  Xorig = X
  pc = matrix(nrow = ncol(Xorig), ncol = ncol(Xorig))
  
  for (i in 1:ncol(Xorig)) {
    opt = extract_pc(X, method)
    X = remove_pc(Xorig, X, opt)
    pc[,i] = opt
  }
   return(pc)
}

method = "CG"

extract_all(X, method)
prcompv_all = prcomp(X)$rotation

angles_all = function(X, method) {
  v1 = extract_all(X, method)
  v2 = prcomp(X)$rotation
  sapply(1:ncol(X), function(i) angle(v1[,i], v2[,i]))
}

angles_all(X, method)
