#Africa Soil Challenge

#install.packages("readr")
library("readr")
library("glmnet")
library("caret")
library("ggplot2")

getwd()
??readr
soils= readr::read_csv("AfricaSoil/training.csv")
head(soils)

#Best results are usually Ensemble method / Stacking
#?gradient-boosted trees

length(soils)

head(soils$Ca)


mean(soils$m7490.25)
sd(soils$m7490.25)
nrow(soils)

CO2nums = 
paste("m", )

head(soils)

grep("m2379.76", names(soils))
#2656#2670
grep("m2352.76", names(soils))

#Remove CO2 absorption lines  as suggested in data guide
soil = soils[-(2656:2670)]

length(soils)
length(soil)

names(soil)
#PIDN, "BSAN"     "BSAS"     "BSAV"     "CTI"      "ELEV"     "EVI"      "LSTD"     "LSTN"     "REF1"     "REF2"     "REF3"     "REF7"    
#"RELI"     "TMAP"     "TMFI"     "Depth"    "Ca"       "P"        "pH"       "SOC"      "Sand"

mean(soil$Sand)
sd(soil$Sand)
namegrab = names(soil)[3565:3585]

summary(soil[namegrab], mean)

namemeans = sapply(soil[namegrab], mean)
###Most of these are not well scaled.
namesds = sapply(soil[namegrab], sd)

namemeans[namemeans<0.1]
namesds[(namesds>0.9 & namesds<1.1)]




#Depth is a character not a number
unique(soil$Depth)
#Topsoil = 1, Subsoil = 0

#Isolate only wavenumbers
wvsoil = soil[2:3564]
names(wvsoil)


alphas = c(1,0.1,0.05,0.01,0.001,0)

swvsoil = scale(wvsoil)

soilcvg = list()

for (i in 1:length(alphas)){
  soilcvg[[i]] = cv.glmnet(swvsoil, soil$Ca, alpha=alphas[[i]])
}

soilcvg[[2]]$cvm

lambdamins = numeric(length = length(alphas))

for (i in 1:length(alphas)){
  lambdamins[[i]] = soilcvg[[i]]$lambda.min
}
lambdamins

names(soilcvg[[1]])

length(alphas)
listplot = vector("list", length=6)
for (i in 1:6){
  listplot[[i]]= qplot(soilcvg[[i]]$lambda, soilcvg[[i]]$cvm, xlim=c(0,8))
}
multiplot(plotlist=listplot)
soilcvg[[3]]$lambda
qplot(soilcvg[[1]]$lambda, soilcvg[[1]]$cvm)



#Set grid parameters
param_grid = expand.grid(alpha = 0:10 * 0.1, lambda = 10^seq(-4,1,length.out=11))
#param_grid = expand.grid(alpha = c(1), lambda = c(0.006))
#Set 10-fold cross-validation (repeat x3)
#control = trainControl(method = "repeatedcv", number = 3, repeats = 1, verboseIter = TRUE)
control = trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)
#Search over grid
caret_fit = train(x=wvsoil, y=soil$Ca, method="glmnet", tuneGrid=param_grid, trControl=control, preProc=c("center", "scale"))
#View optimal vals of alpha, lambda
caret_fit$bestTune
#View cross-val RMSE estimates
caret_fit$results$RMSE

#Best fit: alpha = 0.2, lambda = 0.01

ggplot(data=soil,aes(P)) + geom_histogram()
ggplot(data=soil,aes(log(P+1))) + geom_histogram()


covsoil = cov(soil)
c(row.names(soil))[1:5]
