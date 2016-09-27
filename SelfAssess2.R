#Self assessment 2
#Start: 11:20am

#install.packages("corrplot")
library("corrplot")
library("dplyr")
library("readr")
library("glmnet")
library("caret")
library("ggplot2")
library("psych")

?msq

#Elastic net regression over [active through scornful]. Predicting Extraversion and Neuroticism.
#2 hyperparameters: alpha and lambda

df= msq


length(names(df))
#92 total length

grep("active", names(df))
#1 38
#Oh! 1=active, 38=inactive
grep("scornful", names(df))
#75

grep("Extraversion", names(df))
#80
grep("Neuroticism", names(df))
#81

msqpredictors = df[1:75]
targetE  = df[80]
targetN = df[81]

?glmnet
?cv.glmnet
??caret
#caret = Classification And REgression Training

set.seed(1)

#Scale predictors
msqscalepreds = scale(msqpredictors)
dim(msqscalepreds)
#Quick note: after scaling, it's a matrix.
#mean(msqscalepreds[,4])
msqscalepreds[is.na(msqscalepreds)]  = 0

#Fill in na with the average value of scaled predictors (e.g. = 0)
#mean(msqpredictors[[8]], na.rm = TRUE)

targetE[is.na(targetE)] = mean(targetE[[1]], na.rm=TRUE)
targetN[is.na(targetN)] = mean(targetN[[1]], na.rm=TRUE)

##(na.omit(df) is a thing, but it was better to replace na with 0 here.)


lambdas = 10^(seq(2,-2,length=9))
lambdas
seq(-2,2,length=9)
alphas = seq(0,1,length=10)
srchGrid = expand.grid(alpha = alphas, lambda = lambdas)
srchGrid

# ?trainControl
# control = trainControl(method = "repeatedcv", number = 5, repeats = 3, verboseIter = TRUE)
# #Do change number from 5 to 10 later!
# caret_fit_E = train(x=msqscalepreds, y=targetE[[1]], method = "glmnet", tuneGrid = srchGrid, 
#                     trControl = control, preProc = c("center", "scale"))
# caret_fit_N = train(x=msqscalepreds, y=targetN[[1]], method = "glmnet", tuneGrid = srchGrid, 
#                     trControl = control, preProc = c("center", "scale"))

## Whoops, I'm supposed to reimplement that function without actually caling it.
## Well, I guess I now have something to compare to later.


#Rules: implement something similar to caret's train for glmnet without using cv.glmnet
?glmnet
#quick reminder: glmnet likes to implement its own lambdas.
#If you must use someone else's, put them in decreasing order first.

#Taking break: 12:10pm-12:30pm
#Additional break: 1pm - 2pm

#Implementation:
###Put in a simple RMSE function so you can call it in here later.
#Separate into cross validation subsets


##hm... I could easily implement alphas after calculating L1 and L2, but how do I do this for lambda?
##lambda is part of the penalty function, it does end up influencing the values of coefs.
#glmnet prefers coming up with its own lambdas. Okay, then.

#...

#Desired output: RMSE scores coresponding to each pair of alpha, lambda values. Tested using n-fold cv.

#df of combined RMSE scores at points coressponding to different alpha columns and lambda rows?


#Walking through the bullet points carefully, now.

set.seed(1)

alphas = seq(0,1,0.1)
lambdas = 10^(seq(1, -3, length=50))

for (i in 1:length(msqpredictors)){
  msqpredictors[i][is.na(msqpredictors[i])] = mean(msqpredictors[[i]], na.rm=TRUE)
}

#performed already for targetE and targetN
features = msqpredictors

#precompute subsets for each fold
##train glmnet subset
##test trained model subset
##training target subset
##test target subset

#Iterate through 10 folds and fill in the elements of these 4 lists


####Generate c w/ number copies of 1,1,1,2,2,2,3,3,3 etc. & scramble that. (so I don't have to reorder df)

#We do scaling inside the function. Scale test by the train's results.

#Output: data.frame()  & fill with: alpha value, lambda value, cv RMSE for E, cv RMSE for N

# x=features
# y1 = targetE
# y2 = targetN

#c(1,1,1,1,1,1,1,1,1,1) + c(0,1,2,3,4)
# integer(3)
# x = c(rep(0,11))
# foldhold
# nfold=5

rmse = function(x_hat, x){
  (x_hat - x)^2 %>%
    mean() %>%
    sqrt()
}
###START FUNCTION####
elastic_net_2 = function(x, y1, y2, n_fold = 5, alphas, lambdas){
  
  #Step 0
  lambdas = sort(lambdas, decreasing=TRUE)
  
  #Step 1: Assigning folds
  ##Generating a vector of 0s of length nrow(x), then turn it into a series of repetitions of 1:n_folds .
  foldhold = integer(nrow(x))
  foldhold = foldhold + c(1:n_fold)
  subset = sample(foldhold)
  output = data.frame()
  #Will generate a warning message, but it works as intended.
  
  #Step 2: Assigning cv folds
  for (i in 1:n_fold){
    print (paste("FOLD ", i, collapse=''))
    trainx  = x[subset==i,]
    trainy1 = y1[subset==i,]
    trainy2 = y2[subset==i,]
    testx = x[subset!=i,]
    testy1 = y1[subset!=i,]
    testy2 = y2[subset!=i,]
    
    #Step 2.5: Scaling
    trainx = scale(trainx)
    testx = scale(testx, center = attr(trainx, "scaled:center"), scale = attr(trainx, "scaled:scale"))
    
    #Step 3: fitting w/ glmnet
    #Step 3.5: Grid search
    Efits = vector("list", length = n_fold)
    Nfits = vector("list", length = n_fold)
    
    for (a in 1:length(alphas)){
      print (paste("  alpha: ", alphas[a], collapse=''))
      glmy1 = glmnet(x=trainx, y=trainy1, alpha=alphas[[a]], lambda = lambdas)
      glmy2  = glmnet(x=trainx, y=trainy2, alpha=alphas[[a]], lambda = lambdas)
      Efits[[a]] = glmy1
      Nfits[[a]] = glmy2
      
      predictE = vector("double", length(lambdas))
      predictN = vector("double", length(lambdas))
      
      for (l in 1:length(lambdas)){
        print (paste("    lambda: ", lambdas[l], collapse=''))
        predictE = predict.glmnet(glmy1, testx)
        predictN = predict.glmnet(glmy2, testx)
        Ermse = rmse(predictE, testy1)
        Nrmse = rmse(predictN, testy2)
        
        output = rbind(output, list("alpha" = alphas[a], "lambda" = lambdas[l], "rmse_E" = Ermse, "rmse_N" = Nrmse))
        
      }
    }
  }
  #Reminder: Output = data.frame()  & fill with: alpha value, lambda value, cv RMSE for E, cv RMSE for N
  output
}
####END FUNCTION####


set.seed(1)

alphas = seq(0,1,0.1)
lambdas = 10^(seq(1, -3, length=50))
features = msqpredictors
#Reminder: targetE, targetN

FinalOutput = elastic_net_2(x = features,y1 = targetE,y2 = targetN,n_fold = 10,alphas = alphas,lambdas = lambdas)

head(FinalOutput)

#Recurring error:
# Error: from glmnet Fortran code (error code 7777); All used predictors have zero variance 
#Ultimate cause: using nfolds instead of n_folds
  

FinalOutput[which.min(FinalOutput$rmse_E),]
FinalOutput[which.min(FinalOutput$rmse_N),]

###...my final preference for both was alpha = 1 and lambda = 10 for rmses of 4.143 ish.
unique(FinalOutput$alpha)

subsetalp = FinalOutput[FinalOutput$alpha == 0.1, ]

subsetalp[subsetalp$lambda<0.3,]

arg_min = function(v){
  which.min(v)
}

Eoptrow = FinalOutput[which.min(FinalOutput$rmse_E),]
Noptrow = FinalOutput[which.min(FinalOutput$rmse_N),]

#Favorite for E: alpha = 1 and lambda = 10
#Favorite for N: alpha = 0.1 and lambda = 10

#corrplot(, is.corr=FALSE)

msqscalepreds = scale(msqpredictors)

glmfinalE = glmnet(x=msqscalepreds, y=targetE[[1]], alpha=1, lambda = 10)
#The above results in the retention only of the intercept; nothing further. This presents problems.
glmfinalN = glmnet(x=msqscalepreds, y=targetN[[1]], alpha=0.1, lambda = 10)
coef(glmfinalN)
EN = cbind(c(coef(glmfinalE)), c(coef(glmfinalN)))
names(EN) = c("Extraversion", "Neuroticism")
EN
dim(EN)
NEN= EN[-1,]
NEN  = NEN[NEN[1]==0 && NEN[2]==0]
NEN[,c(1:5)]
corPlot(EN, is.corr=FALSE)
dim(NEN)

#Done around 4:20pm
