# #Oh good, the speed on this thing actually seems to be respectable
# #When one is running it on a decent server!

# install.packages("doMC")
# install.packages("foreach")
# install.packages("tictoc")
# install.packages("xgboost")
# install.packages("randomForest")
# install.packages("dplyr")

# library("dplyr")
# library("doMC")
# library("foreach")
# library("tictoc")
# library("randomForest")
# library("xgboost")

# whitewine=read.csv("winequality-white.csv", header=TRUE, sep=";")
# redwine = read.csv("winequality-red.csv", header=TRUE, sep=";")
# ?read.delim

# allwine = rbind(whitewine, redwine)
# names(allwine) = sapply(names(allwine), function(n) gsub("[.]","_", n) )

# winequality = select(allwine, quality)
# allwine = select(allwine, -quality)


# allthewine = cbind(allwine, winequality)

##Unparalell Run
#tic()
#straightForest = randomForest(quality~., allthewine, ntree=2000)
#toc()
#0.006 sec Wow major problems man

###Paralell Run
#treecounts=c(rep(500, 4))
#tic()
#wildForest = foreach(i=1:4, .combine=randomForest::combine(), .multicombine=TRUE) %dopar% {
#  Forests = randomForest(quality~., allthewine, ntree=treecounts[i])
#}
#toc()
#0.003 sec Bleh :P
####NEWRUN###


library("dplyr")
library("doMC")
library("foreach")
library("tictoc")
library("randomForest")
library("xgboost")
library("gbm")

whitewine=read.csv("winequality-white.csv", header=TRUE, sep=";")
redwine = read.csv("winequality-red.csv", header=TRUE, sep=";")
?read.delim

allwine = rbind(whitewine, redwine)
names(allwine) = sapply(names(allwine), function(n) gsub("[.]","_", n) )

winequality = select(allwine, quality)
allwine = select(allwine, -quality)
winequality = as.matrix(allwine)

?randomForest

#Set up for paralell processing
registerDoMC(detectCores()) #4
getDoParWorkers()

##Unparalell Run
tic()
straightForest = randomForest(x=allwine, y=winequality$quality, ntree=2000)
toc()
#0.006 sec but problems!
#Actual Time: 104.147 sec elapsed
#straightForest = randomForest(x=allwine, y=winequality, ntree=2000)

###Paralell Run
wildForest = vector("list", 4)

treecounts=c(rep(500, 4))
tic()
newForester = foreach(i=1:4, .combine=combine, .multicombine = TRUE) %dopar% {
  randomForest(x=allwine, y=winequality$quality, ntree=treecounts[i])
}
toc()
#0.003 sec but problems!
#Actual Time: 47.617 sec elapsed

foreach(i = 1:3, .combine=) %dopar% { sqrt(i)
}
#


allthewine = cbind(allwine, winequality)

#n.trees = 500, interaction.depth = 40, shrinkage = 0.04, n.minobsinnode = 2
head(allthewine)

tic()
simpleG=gbm(quality~., data=allthewine, n.trees=500, interaction.depth=10, shrinkage=0.04, n.minobsinnode=2)
toc()
#4.788 sec elapsed
#Assumed Gaussian

tic()
simpleG2=gbm.fit(x=allwine, y=winequality$quality, distribution="gaussian", n.trees=500, interaction.depth=10, shrinkage=0.04, n.minobsinnode=2)
toc()
#4.783 sec elapsed

?xgboost
#x=allwine, y=winequality$quality
tic()
xgboost(data = allwine, label = winequality$quality, nrounds=500, max_depth=10, eta=0.04, min_child_weight =2)
toc()
#This assumes linear, which is going to be different from the above by a lot.
