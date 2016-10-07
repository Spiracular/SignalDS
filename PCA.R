#install.packages('DAAG')
library("ggplot2")
library("dplyr")
library("psych")
library("DAAG")
library("glmnet")
library("pROC")

df = msq
df = select(df,Extraversion, Neuroticism, active:scornful)
df = df[,colSums(is.na(df))<400]
df =  df[(rowSums(is.na(df))==0),]

sort(sapply(df, sum))
sort(sapply(df, sd))

features = select(df, -Extraversion, -Neuroticism)
head(features)

PCAdata = prcomp(features, scale=TRUE)

#p$rotation
#matrix w/ rows for columns(xes) of df, columns as PCs, entries as loadings of a column for a PC.

#p$x
#PC_ scores for each row of df

#p$sdev
#eigenvalues of covariance matrix

top = function(p, n, num=10){
  v = p$rotation[, n]
  v[order(abs(v), decreasing = TRUE)][1:num]
}


top5 = PCAdata$rotation[,1:5]

corrplot(PCAdata$rotation, is.corr=FALSE)
#General pattern: Early PCAs are determined by a mix of weak influences from multiple associated variables.
#The first few could be captured by fairly distinct concepts.
#Middle PCAs are often 1 strong influence and many weak ones, harder to pinpoint to a phrase. Mudddled.
#Late PCAs are strongly determined (functionally defined as) 1 or 2 variables.

top(PCAdata, 31, 10)

corrplot(top5, is.corr=FALSE)

colnames(PCAdata$rotation)[1:6]  = c("EnHappy", "NotUnhap", "Disquiet", "NotTired", "Frustrated", "StillNerves")
head(PCAdata$rotation)

#PC1: energetic happiness
#PC2: inverse unhappiness
#PC3: disquiet/unsettled (reversed serenity)
#PC4: I'm not tired
#PC5: "I'm not angry!" D:<
#~PCA6: still nervousness (internal nervousness)

qplot(,PCAdata$sdev)


#cv.lm is a less-used name for CVlm from DAAG

head(df)

dim(PCAdata$rotation)

##Start loop for rmses of different k values

rmse = function(x_hat,y) sqrt(mean((x_hat-y)^2))

n = ncol(PCAdata$x)

rmses_E = numeric(n)
rmses_N = numeric(n)

rmses_cvE = numeric(n)
rmses_cvN = numeric(n)

for (i in seq(n)){
  xdf = PCAdata$x[,1:i]
  xdf = cbind(xdf, "Extraversion" = df$Extraversion, "Neuroticism" = df$Neuroticism)
  xdf = data.frame(xdf)
  head(xdf)
  
  sumss = 0
  Ncvlm = CVlm(data = xdf, form.lm = formula(Neuroticism~.-Extraversion), plotit = FALSE, printit = FALSE)
  Ecvlm = CVlm(data = xdf, form.lm = formula(Extraversion~.-Neuroticism), plotit = FALSE, printit = FALSE)
  rmses_N[i] = rmse(Ncvlm$Predicted, xdf$Neuroticism)
  rmses_E[i] = rmse(Ecvlm$Predicted, xdf$Extraversion)
  rmses_cvN[i] = rmse(Ncvlm$cvpred, xdf$Neuroticism)
  rmses_cvE[i] = rmse(Ecvlm$cvpred, xdf$Extraversion)
}

#Note: Predicted vs. cvpred

qplot(,rmses_N)
qplot(,rmses_E)

qplot(,rmses_cvN)
qplot(,rmses_cvE)

###PCA on Speed Dating Dataset###
spdt = read.csv("speeddating-aggregated.csv")
length(spdt)
spdt = spdt[(rowSums(is.na(spdt))==0),]
names(spdt)
hobbies = select(spdt, sports:yoga)

?prcomp

Pmsq= prcomp(hobbies, scale=TRUE)

qplot(,Pmsq$sdev)
#Eigenvalues descending

corrplot(Pmsq$rotation, is.corr=FALSE)

colnames(Pmsq$rotation)[1:5]= c("NoAesth", "NoSports", "Outdoorsy", "Musician", "Introvert")

corrplot(Pmsq$rotation[,1:5], is.corr=FALSE)



#################################
#Predic gender, race(white=2, asian=4), career code (academia=2, business=7) using PCA w/ logistic regression

sapply(spdt, class)

genh = cbind(hobbies, "gender" = spdt$gender)

rach = cbind(hobbies, "race" = spdt$race)
rach = rach[(rach$race == 2 | rach$race == 4),]
rX = (spdt$race == 2 | spdt$race == 4)
rach$race = rach$race==4
#Asian = TRUE = 1, white = 0

carh = cbind(hobbies, "career_c" = spdt$career_c)
carh = carh[(carh$career_c == 2 | carh$career_c == 7),]
cX = (spdt$career_c == 2 | spdt$career_c == 7)
carh$career_c = carh$career_c==2
#Academic = TRUE = 1, business = 0




#genderlm = glm(gender~., family="binomial", data=gdf)

# formxy = formula("y~x1 + x2")
# y = c(0.57,0.26,0.127)
# x1 = c(1,0.5,0.25)
# x2 = c(0.07,0.01,0.002)
# xy= cbind(x1,x2,y)
# xy = data.frame(xy)
# xylm = lm(formxy, xy)


?glm

logoddP = function(L) exp(L) / (1+exp(L))

###Repeatable segment###
n = ncol(Pmsq$x)
gpred = matrix(nrow=nrow(genh), ncol=n)
tdf = genh$gender
coefkeep = vector("list", length=n)
rockeep = numeric(n)
rmsekeep = numeric(n)
for (i in seq(n)){
  xdf = data.frame(cbind(tdf, Pmsq$x[,1:i]))
  blm = glm(formula= tdf~., family="binomial", data=xdf)
  coefkeep[[i]] = coef(blm)
  preds = logoddP(predict(blm, xdf))
  rockeep[i] = roc(tdf, preds)$auc
  rmsekeep[i] = rmse(tdf, preds)
}
qplot(, rockeep)
qplot(, rmsekeep)
sapply(coefkeep, function(x) x[2])
###
#Thing I have to add above: stepwise vs. regularized regression on activities
#()




###ADD recalculation of PCs from subset###
##(Alternative, different thing: subset x with df$career_c==things, and keep PCs from original)
##(Needed because we are only using a subset of the data)##
###((Do note that PCA1 on this subset may be different from PCA1 on a diferent subset, or the whole))
#Pmsq = prcomp(features, scale=TRUE)
n = ncol(Pmsq$x)
gpred = matrix(nrow=nrow(rach), ncol=n)
tdf = rach$race
coefkeep = vector("list", length=n)
rockeep = numeric(n)
rmsekeep = numeric(n)
for (i in seq(n)){
  xdf = data.frame(cbind(tdf, Pmsq$x[rX,1:i]))
  blm = glm(formula= tdf~., family="binomial", data=xdf)
  coefkeep[[i]] = coef(blm)
  preds = logoddP(predict(blm, xdf))
  rockeep[i] = roc(tdf, preds)$auc
  rmsekeep[i] = rmse(tdf, preds)
}
qplot(,rockeep)
qplot(, rmsekeep)
sapply(coefkeep, function(x) x)

head(preds)
head(predict(blm, xdf))


dim(Pmsq$rotation)
#keeps PCs as columns, activities as rows.


##Careers (1=academics)
n = ncol(Pmsq$x)
gpred = matrix(nrow=nrow(carh), ncol=n)
tdf = carh$career_c
coefkeep = vector("list", length=n)
rockeep = numeric(n)
rmsekeep = numeric(n)
for (i in seq(n)){
  xdf = data.frame(cbind(tdf, Pmsq$x[cX,1:i]))
  blm = glm(formula= tdf~., family="binomial", data=xdf)
  coefkeep[[i]] = coef(blm)
  preds = logoddP(predict(blm, xdf))
  rockeep[i] = roc(tdf, preds)$auc
  rmsekeep[i] = rmse(tdf, preds)
}
qplot(,rockeep)
qplot(, rmsekeep)
sapply(coefkeep, function(x) x[1:5])

coefkeep

head(preds)
head(predict(blm, xdf))


###Do regularized regression for the activities towards predicting career_c###
length(carh)
cscale = scale(carh[-18])
scarh = cbind(scale(carh[-18]), carh[18])
llr = glm(formula=career_c~., family="binomial", data=scarh)#logistic regression
#rlr = glmnet(cscale, carh[[18]], family="binomial")
coef(llr)
#str(coef(rlr))
coef(blm)
summary(llr)



# head(xdf)
# 
# get_log_rmses= function(PCs, ctarget, form, n){
#   n = ncol(PCs$x)
#   rmsekeeper = vector(n)
#   lmskeep = vector(mode = "list", length =n)
#   for (i in n){
#     = cbind(, PCs$x[,1:i])
#   }
# }
# 
# glm(formula= form, family="binomial", data=xdf, )
# 
# 
# xdf = Pmsq$x[,1:i]
# sumss = 0
# Ncvlm = CVlm(data = xdf, form.lm = formula(Neuroticism~.-Extraversion), plotit = FALSE, printit = FALSE)
# Ecvlm = CVlm(data = xdf, form.lm = formula(Extraversion~.-Neuroticism), plotit = FALSE, printit = FALSE)



#####Starting multinomila logistic regression#####
#admission: this part was largely pulled from the solutions

top4career = names(sort(table(spdt$career_c), decreasing = TRUE))[1:4]
#2=academic,7=business,1=lawyer,6=creative
dfc = filter(spdt, career_c %in% top4career)
features = as.matrix(select(dfc, -career_c))
careerfit  =glmnet(features, dfc$career_c, family="multinomial")
careerpreds = predict(careerfit, features, s=0)

PCA_of_MN_preds = prcomp(scale(data.frame(careerpreds)))
rownames(PCA_of_MN_preds$rotation) = c("Lawyer","Academic","Creative","Finance")
corrplot(PCA_of_MN_preds$rotation, is.corr=FALSE)


glmnet(family="multinomial")
predict(s=0)
prcomp()

###Then I'm going to try to do it this way and see if I can make it work... (attempt at Polychoric)
#scale a dataset
#Cov (or cor)
#eigen
%>% cov() %>% eigen()

###Side note: Polychoric correlation is the better comparison operator (?) than pearson for making PCA in a binomial context.
?prcomp#PRincipal COMPonents

#Sept 19 2016
###Make your own PCA function###

?optim

PCAfunct = function(X){
  #i in rXn
  #
  #s in k = nXp
  cXp = ncol(X)
  rXn = nrow(X)
  k = cXp#number of PCs
  Ww=matrix(nrow = k, ncol = cXp)
  #w = weights
  
  
  Tt = matrix(nrow = rXn, ncol = k)
  #t = principal component scores
  t[i,k] = x[i,] %*% w[k,]
  w[]
  formula()
  = optim(par = , fn=wmax, )
  
  for (i in 1:rXn){
    
  }
}
optim(par = )

wmax = function(w, x){
  max(sum((x%*%w)^2)
}

##Note: renormalize before the second step.

# #Stupid test
# diag = matrix(c(1,0,0,0,1,0,0,0,0.5,0,0,0.5), nrow = 3, ncol=4)
# dPCA= prcomp(diag)


###Solution taken from Michelle's.###
###Went over carefully


set.seed(1)
X = matrix(sample(0:1, 100, replace = TRUE), nrow = 10)
X = scale(X)
w = 1:10


objective = function(w,X) {
  #Takes in data frame X
  # X = matrix(X)
  (t(w) %*% t(X) %*% X %*% w)/(t(w) %*% w)
}
#The above series of matrix products will be the function to optimize for normalized input data.

vnorm = function(v) {
  sqrt(sum(v^2))
}
#Denominator for vector normalizaiton (keep direction, magnitude = 1)

extract_pc = function(X, method) {
  opt = optim(par = rep(1, ncol(X)), fn = objective, method = method, control = list(maxit = 10000, fnscale = -1), X = X)
  opt$par/vnorm(opt$par)#Normalized vector
}

opt = extract_pc(X, "CG")

prcompv = prcomp(X)$rotation[,1]

angle = function(x, y) {
  z = acos(sum(x*y)/vnorm(x)/vnorm(y))*180/pi
  return(min(z, 180-z))
}

angle(opt,prcompv)