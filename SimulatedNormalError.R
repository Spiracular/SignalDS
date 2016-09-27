library("ggplot2")
a = seq(0.1,0.9, by=0.1)
n = c(1,5,25, 100)*100


getSamples = function(a, n, b = 1){
  y = vector(mode="double", n)
  x = rnorm(n)
  for (i in 1:n){
    y[i] = a*x[i] + rnorm(1,0,b)
  }
  df = data.frame(x=x, y=y)
  return (df)
}

xydf = getSamples(a[1],n[1])
xydf

ggplot(xydf, aes(x,y)) + geom_point() + geom_abline(slope=a[1])
ggplot(xydf, aes(x,y)) + geom_point() + geom_smooth(method=lm)

ggplot(xydf, aes(x)) + geom_histogram(fill = "#CC0000", colour = "#AA0000")


                                    
estimateSlopes = function(a, n, numTrials = 500){
  collect = vector(mode='double', length = numTrials)
  for (i in 1:numTrials){
    df = getSamples(a,n)
    linear_fit = lm(y ~ x, data=df)
    collect[[i]] = coef(linear_fit)[[2]]  
  }

  collect
}

coeffcollect = estimateSlopes(a[1],n[1])

coefmat= matrix(coeffcollect, ncol=1)
coefplot = data.frame(coefmat)
# head(coeffcollect)
# head(coefplot)

ggplot(coefplot, aes(coefmat)) + geom_histogram(fill = "#CC0000", colour = "#AA0000", binwidth = .01)


#n = c(2,5)
#a = c(1,2,3)
dfSD = matrix(nrow=length(n), ncol=length(a))
dfSD = data.frame(dfSD)
for (i in 1:length(a)){
  ncollect = vector(mode="double", length = length(n))
  for (j in 1:length(n)){
    ncollect[j] = sd ( estimateSlopes(a = a[i], n = n[j]))
  }
  dfSD[i] = ncollect
}
head(dfSD)
#This is badish.

rnorm(3)

list(1,2,list(1,2,3))

?data.frame

rnorm()
?geom_abline