#Functional Programming

df = data.frame(matrix(1:100, nrow=10))
df
means = sapply(1:ncol(df) , function(i) mean(df[[i]]))
head(means)

#lapply() is basically the parent function for the other *apply functions.

double = lapply(1:10, function(x) 2*x)
typeof(double)
head(double)
#If the function returns pairs or something similar, unlisting will muddle the results.
#function(x) in this context is an "anonymous function" declared in *apply for the sake of local function execution.



#############
?deviance
?prod()
?sd

is.numeric(iris$Species)
sapply(iris, class)
is.numeric(iris)

#Something that should be equivalent to product of vector (harder, but worth knowing): exp(sum(log()))

is.numeric()==TRUE

?which

TFiris = sapply(iris, is.numeric)
iris[TFiris]
##########


df = mtcars
sapply(df, class)
#sapply = unlisted lapply (to vector or matrix)

normalize = function(df){
  TFnum = sapply(df, is.numeric)
  dftf = df[TFnum]
  means = sapply(dftf, mean)
  stds = sapply(dftf, sd)
  for (i in 1:ncol(dftf)){
    dftf[[i]] = (dftf[[i]] - means[[i]])/stds[[i]]
  }
  df[TFnum] = dftf
  return (df)
}

ncars = normalize(mtcars)
class(ncars)

niris= normalize(iris)
head(niris)
sapply(niris[1:4], sd)

newapply = function(args, func) {
  collector = vector(mode="list", length=length(args))
  for (i in 1:length(args)) {
    collector[i] = func(args[[i]])
  }
  return(collector)
}

newapply(df, sd)

system.time(replicate(1000, newapply(df, sd)))


subtract_cols = function(df) {
  newdf = df
  for (i in 2:length(df)) {
    newdf[i] = newdf[i] - df[i-1]
  }
  return(newdf)
}

subtract_cols(df)
df = cbind(df, x11 = c(201:210))

#can loop through: columns in df, indices in length(df), or name in names(df)

?prod
multiply = function(x,k=2) { k*x }

sapply(1:10, multiply, k=5)

?mean

sapply(df, mean, na.rm=TRUE)
L = lapply(1:5, function(x) sample(c(1:4, NA)))
L
sample(c(1:4,NA))
sapply(L,mean,na.rm=TRUE)

namemaker = function(df){
  for (i in 1:length(df)){
    newname = paste(c(names(df[i]), i), collapse="_")
    names(df)[i] = newname
  }
  return(df)
}

namemaker(df)

x = seq(3,6,0.1)
y=x


sapply(x, function(x) prod(exp(x), cos(x)))
#Oh cool, this works.



for (i in x){
  print (prod(exp(i), cos(i)))
}


x
?seq


df[5,5] = NA
df[5,5] = 45
df
a,b,b,a
0,1,1,0

dummy_var = function(df){
  dfanswer = df
  for (i in 1:ncol(df)){
    if (is.factor(df[[i]])){
      levelset = levels(df[[i]])
      for (x in levelset){
        TFmap = df[[i]]==x
        newcol = as.numeric(TFmap)
        newname = paste(c(names(df[i]), x), collapse=".")
        #        print(newname)
        #        dfanswer = cbind(dfanswer, newest = newcol)
        dfanswer[newname] = newcol
      }
    }  
  }
  return (dfanswer)
}

sindex = sample(1:nrow(iris), 10)
smiris = iris[sindex,]
model.matrix( ~ Species - 1, data=smiris)
smiris

dummy_var(smiris)

m = matrix(1:9, nrow=3)
m
apply(m,1,mean)
apply(m,2,mean)
apply(m, c(1,2), mean)
matsmiris = smiris[1:4]
matsmiris
apply(smiris, 1, max)
?max
max(1,2,4,6,7,438482934954392, "hello")
max(1,2,4,6,7,43)

values = lapply(1:10, function(x) rnorm(10))
values
str(values)
values[[3]][3] = NA
values
weights = lapply(1:10, function(x) rnorm(10))
weights
Map(weighted.mean,values,weights, values, weights, MoreArgs=list(na.rm=TRUE))
?mapply

library("HistData")
library("dplyr")
library("Rmisc")
library("ggplot2")
df = women
nrow(women)
mapply('/', women[2], women[1], USE.NAMES=FALSE)
#
women[1,]
women[14,]

?mapply

?women
BMI = function(){
  #convert to kg
  #convert to meters
  #kg/meters
}

'/'(1,2)

?Arithmetic

?Reduce

Reduce('+', 1:10) == sum(1:10)

?detach
detach(package::Rmisc, unload=TRUE)
#AUGH, can't get the package detached. Oh well. Namespace conflict w/ dplyr it is!


setlist = list(c(1,3,5,6,7), c(55,6,7,7,3), c(5,6,7,8,3,9))
Reduce(union, setlist)
Reduce(intersect, setlist)
?union



pal = c(1,2,4,5,6,6,5,4)

dircheck = function(func, ob){
  ranswer = Reduce(func, ob, right=TRUE)
  lanswer = Reduce(func, ob, right=FALSE)
  if (ranswer == lanswer) {
    return(ranswer) 
  } else {
      return(NA)
    }
}

dircheck('*',pal)

myReduce = function(func,x) {
  result = func(x[1], x[2])
  for (i in 3:length(x)) {
    result = func(result, x[i])
  }
  return(result)
}

divab = function(x,y) { x/y }

short = c(2,4,3)

Reduce(mean,x=c(100,2,5),init=c(2,3,100)))

# globeTF = TRUE
# dothing = function(a,b){
#   
#   globeTF <<- !globeTF
# }

contfrac = function(x,y){
  y + 1/x
}
x = rep(c(1,2), 1000)
Reduce(contfrac, x)

n=100
n:1

Ramanrad = function(x,y){
  return( sqrt(1 + y*x) )
}

n=1000
initial = sqrt(1+n)
Reduce(Ramanrad, n:2, initial)

#These return TRUE or FALSE for entry in input args
?Filter
?Find
?Position

?Negate


nalist = c(NA,"b","a", NA, NA, "a",NA)
Filter(Negate(is.na), nalist)
Find(Negate(is.na), nalist, right=TRUE)
Find(Negate(is.na), nalist)

Any = function(b){
  return(Reduce('|', b))
}

All = function(b){
  return(Reduce('&',b))
}

bool1 = c(TRUE, TRUE, TRUE, TRUE)
bool2 = c(TRUE, TRUE, TRUE, FALSE)
bool3 = c(FALSE, FALSE)

Any(bool3)



?'|'

?all
?any



?mean

?rev

uniques()

