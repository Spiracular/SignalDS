# Self Assessment 3
# Megan Crawford

#Start: 10:00am (Computer freeze for ~5 mins)
#+2:30
#Break: 12:35pm-2pm
#+1:30
#Break2: 3:35pm-4:30pm
#+1:50
#End: 6:20

#5hrs 50mins

#Election of 1992: Bill Clinton vs. George H. W. Bush, with Ross Perot as third-party Independent

library("dummies")
library("dplyr")
library("pROC")
library("caret")
library("glmnet")
library("corrplot")
library("psych")

CSVElect1992 = read.csv(file = "nes_cleaned_1992.csv")
# head(Election1992)
# sapply(Election1992, class)
#Elect1992 = Election1992

Elect1992$gender  = as.integer(Elect1992$gender) - 1
# 0 = Female 1 = Male
Elect1992$vote  = as.integer(Elect1992$vote) - 1

#Subset to only those who voted
Elect1992 = Elect1992[Elect1992$vote==1 , ]


##Remove 

dummy()


qplot(Elect1992$presvote)
qplot(Elect1992$occup1)

levels(Elect1992$vote)
as.logical(c(1,0,0,0,1,0,1,1,1))

?dummy

Elect1992 = dummy.data.frame(Elect1992, names="presvote")
#Subset to only those who voted democrat or republican
Elect1992  = Elect1992[as.logical(Elect1992$presvotedemocrat) || as.logical(Elect1992$presvoterepublican),]
Elect1992 = select(Elect1992, -presvoteother, -presvoteNA, -presvotedemocrat)
table(Elect1992$occup1)

lapply(Elect1992, table)

Elect1992 = dummy.data.frame(Elect1992, names = c("race", "educ", "urban", "region", "income", "religion", "occup1"))
Elect1992$union = as.integer(Elect1992$union) - 1
names(Elect1992)
#Removing white from race
#Removing GradeSchool from educ
#Removing rural from urban
#Removing west from region
#Removing 0-16 %ile from income
###Note: skilled expands to  "Skilled, semi-skilled, or service worker". Clerical expands to "clerical and sales", professional to "professional and managerial."
#Removing the "skilled" category
#Turn union into a boolean
#Removing other from religion
#Take out the vote column (no longer needed)

Elect1992 = select(Elect1992, -raceWhite, -educGradeSchool, -urbanrural, -regionwest, -occup1skilled, -religionother)
grep("0-16 %ile", names(Elect1992))
Elect1992 = Elect1992[-15]

#Oops, I should have kept republican. Easy enough to reacquire...
Elect1992["presvoterepublican"] = !as.logical(Elect1992$presvotedemocrat)
Elect1992$presvoterepublican = as.integer(Elect1992$presvoterepublican)
grep("presvotedemocrat", names(Elect1992))
Elect1992= Elect1992 [-29]


###Run logistic Regression

logreg = glm(presvoterepublican ~ ., family="binomial", Elect1992)
sort(coef(logreg), decreasing = TRUE)

coef(logreg)[order(abs(coef(logreg)), decreasing=TRUE)]
#Looks like blacks were extremely unlikley to vote Republican, protestants were reasonably likely to, asians has fair odds, upper-middle class were somewhat likely, jews were unlikely

#Middle-income was relatively unpredictive. I'm tempted to rerun that with the middle income excluded.



pred = predict(logreg, Elect1992) #Note: gave a warning about rank-deficient fit, although I thought I had removed a variable from each so that's troubling...

##stick in actual and predicted
plot(roc(Elect1992$presvoterepublican, pred))

##I'm not very sure how you're proposing to do a cursory check for overfitting before the roc curve?


supportsmod = pred>0.5 #people predicted to support republican candidate, if predictions should gesture at 0 for democrat and 1 for republican
sum(supportsmod) / length(supportsmod) # 34.9% support for Bush predicted
1- (sum(supportsmod) / length(supportsmod)) # 65% support for Clinton predicted

#https://en.wikipedia.org/wiki/United_States_presidential_election,_1992
##Actuality: Bill Clinton got 43% of the vote, Bush got 37.4% of the vote, and Ross Perot got 18.9% of the vote.
##(Do note: 43% + 18.9%  = 62%, so if Ross Perot was pulling from people who would otherwise be Clinton supporters this would still make sense?)

TwinCSV = read.csv(file = "NMSQT.csv")

head(TwinCSV)

TwinCSV$SEX = as.integer(TwinCSV$SEX) - 1
#female = 0 male = 1
TwinCSV$ZYG = as.integer(TwinCSV$ZYG) - 1
#fraternal = 0 identical = 1

#Select training subset
TwinTrain = select(TwinCSV, starts_with("V11"))

?glmnet

set.seed(5)

#Going to use caret package to do repeated 10-fold cross validation and testing for alpha and lambda parameters (I changed the alpha parameters to be a cleaner 10^-0.5 increment)
#For the linear regression of Psych Inventory answers vs. National Merit Test Score.


alphalam = expand.grid(alpha= 1:10*0.1 , lambda = 10^seq(3, 0, length.out=7))
#alphas: between 0 and 1 for intermediates between lasso and ridge regression
#lambdas: supply decreasing values for glmnet. Penalty term. The total of lambda will be divied up according to the alpha parameter
control = trainControl(method="repeatedcv", number=10, repeats=3, verboseIter=TRUE)
caret_fit = train(x=TwinTrain, y=TwinCSV$NMSQT, method="glmnet", tuneGrid = alphalam, trControl=control, preProc = c("center", "scale"))
#Do I need preProc = c("center", "scale") given 0,1 values? There may be a bias in one direction or another on some of the Qs... I'll leave it in.

caret_fit$bestTune 
### alpha = 0.4, lambda = 1
##Hm... lambda picked the value 1 (from 10^seq(0,-5,length.out = 11)).
##Do I want to hand it some higher values of lambda and run it again?

###alpha=0.1, lambda=3.162278

# Warning message:
#   In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
#           There were missing values in resampled performance measures.

#Percentage of variance explained by the best fit model

min(caret_fit$results$RMSE) # = 15.225 (last was 15.3 or so)

names(caret_fit)

##Hahaha... there's a fishnet class associated with the poisson distribution.




######This is only here so I can look at how caret works again
#Set grid parameters
param_grid = expand.grid(alpha = 1:10 * 0.1, lambda = 10^seq(-4,0,length.out=10))
#Set 10-fold cross-validation (repeat x3)
control = trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)
#Search over grid
caret_fit = train(x=hobbies, y=o_collect[[1]], method="glmnet", tuneGrid=param_grid, trControl=control, preProc=c("center", "scale"))
#View optimal vals of alpha, lambda
caret_fit$bestTune
#View cross-val RMSE estimates
######

order(caret_fit$results$RMSE) #index of best RMSE: 2

#Percent variance explained by model: 48.92%
caret_fit$results$Rsquared[2]

#Get best coefficients

Vcoefs= coef(caret_fit$finalModel, caret_fit$bestTune$lambda)
#remove intercept
Vcoefs = Vcoefs[2:nrow(Vcoefs),]


#Next: str_join the coeficient to the question text in the codebook csv

codebook  = read.csv(file="NMSQTcodebook.csv")
head(codebook)

codebook["coeficients"] = Vcoefs

codebook = codebook[order(abs(Vcoefs), decreasing = TRUE), ]

head(codebook)

#Being a slow learner, afraid of earthquakes, and showing a preference for being poor and dependable over wealthy and brilliant predicts low merit scores.
#While reading >10 books per year, wanting to write a technical book, and doing your best in school predicts high scores.

Twindf = cbind(TwinTrain, "NMSQT"=TwinCSV$NMSQT)

MCAmerit = prcomp(x=TwinTrain) #Choosing not to scale this one

#corrplot(MCAmerit$rotation[1:5,], is.corr=FALSE)

dim(MCAmerit$rotation)#478 x 478 plot, utterly unplottable :P

#sd of PC in decreasing order, screeplot  & factor number guess for fa

###Generate a scree plot of the first 30 standard deviations of PCs
qplot(c(1:30),MCAmerit$sdev[1:30]) #I'm going with 10 FAs

MeritFac = fa(TwinTrain, nfactors=10, rotate="oblimin")

codebook = codebook[order(codebook$var),]

codefacts = cbind(codebook, "FactorLoad" = sapply(1:10, function (i) MeritFac$loadings[,i]))


#Vary off of this to get readings on the high and low-scoring 
codeorder = codefacts[order(codefacts$FactorLoad.10, decreasing = TRUE),]
codeorder[1:10,]
tail(codeorder)

#Factor 1: I'm nervous around new people
#Factor 2: Neuroticism (self-hatred, atypicality, and strange ideation)
#Factor 3: Orderliness
#Factor 4: "Factor 4: Adventure Mechanic!"
#Factor 5: Most people are self-interested utilitarian crooks with no morals.
#Factor 6: I love discussion, study and lectures, poetry, music, and science!
#Factor 7: I don't like my parents. They drive me nuts and I want to get away from them.
#Factor 8: WOO! PARTY!
#Factor 9: everything is awful... my stomach hurts... i'm scared
#Factor 10: A strongheaded leader, and a winner

factornames = c("Shy", "Neurotic", "Orderly", "Masculine", "Randian", "Aesthete", "Runaway", "PARTY", "mope", "Leader")
grep("FactorLoad", names(codefacts))
names(codefacts)[5:14] = factornames
head(codefacts)

#Scale the factors so the units are sds, and aggregate the factors by gender using aggregate()

factscale =  scale(codefacts[5:14])
colSums(factscale)

#scoring function: single instance
function(Twins ,scaled){ 
  for (ti in 1:ncol(Twins)){
    factormix = Twins[] * 
  }
}

sum(TwinCSV$SEX == 1) / nrow(TwinCSV) #40% male

head(TwinCSV)
#5:482

codefacts$var-TwinCSV

482-4

dim(TwinTrain)
dim(TwinCSV)
dim(factscale)


for (i in 1:478){
  #multiply column of factscale by row of TwinTrain
  
  #Nah, sum score in twintrain per column (after separation into groups)
  #Then, multiply that row of sums by a column of factscale
  #Then sum that to get the factscore
  
}

get_factscore = function(dfact, scaled = factscale){
  scores = numeric(ncol(scaled))
  names(scores) = colnames(scaled)
  sumcollect = colSums(dfact)
  for (i in 1:ncol(scaled)){
    scores[i] = sum(sumcollect*scaled[,i])
  }
  scores/nrow(dfact)
}

#aggregate(x=TwinTrain, by=list(TwinCSV$SEX), FUN=get_factscore) ###Oh, darn, it's feeding in columns. Augh.

#Aggregate feels like the wrong tool at this point.

malefacts = get_factscore(dfact= TwinTrain[TwinCSV$SEX==1,])
malefacts

femefacts = get_factscore(dfact= TwinTrain[TwinCSV$SEX==0,])
femefacts

genderfacts = rbind(malefacts, femefacts)

genderfacts
### Males self-report as: slightly less shy, less Neurotic, less orderly, much more Masculine, more Randian, a bit less Aesthete,
### about the same Runaway tendency, less PARTY, slighly less mope, and more Leader.

genderdif = malefacts - femefacts

genderdif #male minus female scores
#      Shy  Neurotic   Orderly Masculine   Randian  Aesthete   Runaway     PARTY      mope    Leader 
#-5.173965 -9.872694 -9.643715 43.804136 13.547078 -3.918635  1.460707 -8.778780 -3.043817 10.802901 

###Finding the percent variance explained by additive genetics

#2 x (corr(identical) - corr(fraternal))

#combine factors and NMSQT, and compute percent variance explained by additive genetic effects. Interpret.

#separate into 2 or 4 tables of twin sets
#-1. generate/fuse together the starting table
#0. Scramble rows
#1. Order by twinpair identifier
#2. count twinpair identifier instances (table); rm any singles
#3. select out fraternal vs. identical
#4. apply c(TRUE, FALSE) and inverse to make tables
#4. corr

#Fusing together the starting df

head(TwinCSV)
StartTwin= TwinCSV[,c(1,2,4)]

count(StartTwin$ID) #Every last ID has 2, great.

#TwinTrain
dim(TwinTrain)
dim(factscale)
#for a row dfact

matfac = matrix(nrow=1504, ncol=10)
matfac = as.matrix(TwinTrain) %*% as.matrix(factscale)
colSums(matfac) #I can't help but think I should have scaled against something else, but nothing to do at this point except scale it all again.
matfac = scale(matfac)

StartTwin = cbind(StartTwin, matfac)

StartTwin = StartTwin[sample(1:nrow(StartTwin), nrow(StartTwin)), ]
StartTwin = StartTwin[order(StartTwin$ZYG, StartTwin$ID),]
table(StartTwin$ZYG)
length(StartTwin$ZYG)
StartTwin = StartTwin[-2]

Fraternals = StartTwin[1:606,]
Identicals = StartTwin[607:1504,]


TF = c(TRUE, FALSE)

VarInherited = 2* ( diag(cor(Identicals[TF, ], Identicals[!TF, ])) - diag(cor(Fraternals[TF,], Fraternals[!TF,])) )
 
 #ID         NMSQT           Shy      Neurotic       Orderly     Masculine       Randian      Aesthete       Runaway         PARTY          mope 
 #-2.220446e-16  4.858618e-01  6.547482e-01  5.818718e-01  3.519187e-01  1.542688e-01  4.594226e-01  4.225037e-01  4.895306e-01  7.594043e-01  6.537947e-01 
 #Leader 
 #7.134607e-01 


#ID: Not inherited (consistent with desired outcome; it's a 100% match in both fraternal and dentical)
#NMSQT: 48.6%, Shy: 65.5%, Neurotic: 58.9%, Order:35.2%, Masculine:15.4%, Rand: 45.9%, Aes: 42.2%, Runaway: 49.0%, Party: 7.59%, mope:6.54%, Leader: 7.13%



