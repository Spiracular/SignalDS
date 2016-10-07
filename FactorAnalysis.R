###Factor Analysis###
#install.packages("GPArotation")
library("GPArotation")
library("psych")
library("corrplot")
library("dplyr")
library("ggplot2")

?fa()
#Enter covariance or correlation matrix as r

set.seed(1)

Xs = rnorm(100)
Ys = rnorm(100)
Zs = rnorm(100)

factors = cbind(Xs, Ys, Zs)
dfactors = data.frame(factors)

noisyProxies = function(feature, k, correlation){
  flength = length(feature)
  nprox = matrix(nrow=flength, ncol=k)
  for (i in 1:k){
    error = rnorm(flength)
    nprox[,i] = feature*correlation + error 
  }

  nprox
}

noises = cbind(noisyProxies(dfactors$Xs, 4, 0.9), noisyProxies(dfactors$Ys, 3, 0.9))
noises = as.data.frame(noises)
#Quick note: V1:V4 are X-based, V5:V7 are Y-based.

corrplot(cor(noises))
corrplot(cor(noises, dfactors))

PCA_XYZ = prcomp(noises)

names(PCA_XYZ)
corrplot(cor(noises, PCA_XYZ$x))
corrplot(cor(dfactors, PCA_XYZ$x))

## Orthogonal Factor Analysis

FA_XYZ = fa(noises, nfactors=2, rotate = "varimax")
names(FA_XYZ)

corrplot(cor(noises, FA_XYZ$scores))

### Factors vs. Underlying Variables
corrplot(cor(dfactors, FA_XYZ$scores))


error = rnorm(50)
weights = cbind("x"= runif(50), "y" = runif(50), "z" = runif(50))
mixedvars = sapply(1:50, function(i) dfactors$Xs*weights[i,1] + dfactors$Ys*weights[i,2] + dfactors$Zs*weights[i,3] + 0.5*rnorm(100))
mixedvars = as.data.frame(mixedvars)
###Or should I do as.data.frame? (Was previously doing just data.frame)
head(mixedvars)
dim(mixedvars)

PCA_mix = prcomp(mixedvars)
FA_mix = fa(mixedvars, nfactors = 3, rotate = "varimax")

#vs. data
corrplot(cor(mixedvars, PCA_mix$x))
corrplot(cor(mixedvars, FA_mix$scores))

#vs. underlying vars
corrplot(cor(dfactors, PCA_mix$x[,1:3]))
corrplot(cor(dfactors, FA_mix$scores))

#sum(weights[,"x"])
#sum(weights[,"y"])
#sum(weights[,"z"])
#head(weights)

#Oblique Factor Analysis
Ws = 0.5*Xs + Ys
cor(Ws,dfactors)

Wnoise = cbind(noisyProxies(Xs, 10, 0.8), noisyProxies(Ws, 14, 0.8))
Wnoise = as.data.frame(Wnoise)
head(Wnoise)

VMFA_XW = fa(Wnoise, nfactors = 2, rotate="varimax")
OMFA_XW = fa(Wnoise, nfactors = 2, rotate="oblimin")

corrplot(cor(cbind(dfactors, Ws), VMFA_XW$scores))
corrplot(cor(cbind(dfactors, Ws), OMFA_XW$scores))
####(Expected better from oblimin, but they are essentially indistinguishable :/)

# Factor Analysis on Real Datasets

spdt = read.csv("speeddating-aggregated.csv")
sapply(spdt, class)

?select
hobbies = select(spdt, sports:yoga)


VMhob = lapply(1:4, function(nf) fa(hobbies, nfactors=nf, rotate="varimax"))
OMhob = lapply(1:4, function(nf) fa(hobbies, nfactors=nf, rotate="oblimin"))

corrplot(VMhob[[4]]$loadings)
corrplot(OMhob[[4]]$loadings)


B5data = read.csv("BIG5/data.csv", sep = "\t")

##note to self: csv2 knows how to deal with 1,000

head(B5data)
dim(B5data)
sapply(B5data, class)

typeof(B5data[[4,4]])


MB5= as.matrix(select(B5data, -country))
scaledB5 = scale(MB5)

FAB5 = fa(scaledB5, nfactors = 5)
PCB5 = prcomp(scaledB5)

corrplot(FAB5$loadings, is.corr=FALSE)
corrplot(PCB5$rotation[,1:5], is.corr=FALSE)

FAB5$loadings

Big5 = select(B5data, -country)
unique(Big5$gender)
#Oh geez... okay, 3 is other and 0 is missing. 1=male 2=female.
Big5 = Big5[!(Big5$gender==3 | Big5$gender==0), ]
Big5$gender = Big5$gender - 1
Big5 = select(Big5, gender, E1:O10)

names(FAB5)
xFAB5 = cbind(FAB5$scores, select(B5data, gender))
xFAB5 = xFAB5[!(xFAB5$gender==3 | xFAB5$gender==0), ]
xFAB5$gender = xFAB5$gender - 1

logresB5 = glm(gender~., family="binomial", data=Big5)
logresFAB5 = glm(gender~., family="binomial", data=xFAB5)

logoddP = function(L) exp(L) / (1+exp(L))

#Logistic regression on the original data
corrplot(as.matrix(coef(logresB5)[-1]), is.corr = FALSE)
corrplot(as.matrix(logoddP(coef(logresB5)[-1])), is.corr=FALSE)

#Logistic regression on the factors
corrplot(as.matrix(coef(logresFAB5)[-1]), is.corr = FALSE)
corrplot(as.matrix(logoddP(coef(logresFAB5)[-1])), is.corr=FALSE)

coef(logresFAB5)

#Coefficients in the factors model are much larger, of course.

Extraversion = select(B5data, E1:E10)
Neuroticism  = select(B5data, N1:N10)
Agreeableness = select(B5data, A1:A10)
Conscientiousness = select(B5data, C1:C10)
Openess = select(B5data, O1:O10)

EFA = fa(Extraversion, 2)
corrplot(EFA$loadings)
#Paper had Enthusiasm/Assertiveness, got Party-person/Center-of-Attention 
#Match = FALSE

NFA = fa(Neuroticism, 2)
corrplot(NFA$loadings)
#Paper had Volatility/Withdrawal, got Worrier=Withdrawal/Moodiness=Volatility (checks out!)
#Match = TRUE

AFA = fa(Agreeableness, 2)
corrplot(AFA$loadings)
#Paper had Compassion/Politeness, got Empathy=Compassion/Disinterested-in-People
#Match = partial?

CFA = fa(Conscientiousness, 2)
corrplot(CFA$loadings)
#Paper had Industriousness/Orderliness, got Be-Prepared=Industriousness/Messiness= - Orderliness
#Match = TRUE (or close enough)

OFA = fa(Openess, 2)
corrplot(OFA$loadings)
#Paper had Intellect/Openness, got Ideas/Vocab
#Match = FALSE
#It also looks like it's begging to add another few factors, so I'll expand on it.

OFA3 = fa(Openess,3)
corrplot(OFA3$loadings)
#MR1: Ideas, MR2: Vocab, MR3: Disinterest in Abstraction

