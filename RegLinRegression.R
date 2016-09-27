#Regularized Linear Regression

#install.packages("glmnet")
library("glmnet")

#install.packages("caret")
library("caret")

?cv.glmnet
?glmnet

set.seed(1); j= 50; a = 0.25
x = rnorm(j)
error = sqrt(1-a^2)*rnorm(j)
y = a*x + error
x

summary(lm(y~x-1))
#Reminder: -1 = no intercept

#aEst = a_hat

SSE = function(y, y_hat) {sum( (y-y_hat)^2 )}

# cost = function(x,y,aEst,lambda,p=1){
#   #lambda is a regularization parameter
#   #linearmodelformula = y~aEst*x
#   aEst= gridlama$Var2
#   coefs = aEst
#   SSEscore = sapply(x, function(hat) SSE(y, aEst*hat))
#   # for (xes in x){
#   #   SSEadd = SSE(y, coefs*xes)
#   #   SSEscore = c(SSEscore, SSEadd)
#   # }
#   # lambda = 0.25
#   # p=1
#   # lambda*sum(abs(coefs)^p)
#   SSEscore + lambda*sum(abs(coefs)^p)
#   
#   return(SSEscore + lambda*sum(abs(coefs)^p))
# }
#some part of me wants to be able to define the function for estimates of y in the function, but WHATEVER. I'll move on.

cost =  function(x, y, aEst, lambda, p){
  sum((y - aEst*x)^2) + sum(lambda*abs(aEst)^p)
}

cost(1,2,3,4,2)

lambdas = c()
for (n in -2:7){
  lambdas = c(lambdas, 2^n)
}
lambdas

aEsts = seq(-0.1, 0.3, 0.001)

gridlama = expand.grid(lambdas,aEsts)

gridlama[1,]

gridlama["costL1"] = sapply(1:nrow(gridlama), function(i) cost(x, y, aEst = gridlama$Var2[i], lambda = gridlama$Var1[i], p=1))
gridlama["costL2"] = sapply(1:nrow(gridlama), function(i) cost(x, y, aEst = gridlama$Var2[i], lambda = gridlama$Var1[i], p=2))

head(gridlama)

names(gridlama)[1:2] = c("lambdas","aEsts")

get_plot = function(grid = gridlama, lambda, p){
  Aestset = grid[grid[["lambdas"]]==lambda,]$aEsts
  #get aests where $lambdas = lambda
  #costL = paste("costL", p, collapse="")
  
  if (p == 1){
    qplot(x=Aestset, y=grid["costL1"])
  } else if (p==2){
    qplot(x=Aestset, y=grid["costL2"])
  } else{
    print("error")
    return
  }
}
##I'm getting really weird results for this... copying solutions.

#get_plot(grid=gridlama, lambda = lambdas[[1]], p=1)

# get_plot() function
get_plot = function(lambda, p) {
  aEst = grid[grid$lambda == lambda, "aEst"]
  if (p == 1) {
    cost = grid[grid$lambda == lambda, "costL1"]
  } else {
    cost = grid[grid$lambda == lambda, "costL2"]
  }
  qplot(aEst, cost)
}

names(gridlama) = c("lambda","aEst","costL1", "costL2")
grid=gridlama

# Make the lists of plots
plotsL1 = lapply(1:10, function(k) get_plot(lambdas[k], 1))
plotsL2 = lapply(1:10, function(k) get_plot(lambdas[k], 2))

# Calls to multiplot()
#L1 plots
multiplot(plotlist=plotsL1, cols=2)
#L2 plots
multiplot(plotlist=plotsL2, cols=2)

########Regularization vs. Stepwise Regression#########

spdt = read.csv("speed-dating-simple.csv")

allmaledate = filter(spdt, gender==1)
o_collect = allmaledate[2:6]
hobbies = allmaledate[7:23]

malehobbies = cbind(o_collect[1], hobbies)
names(malehobbies)

#glmnet does regularized linear regression. alpha=1 is L1, alpha=0 is L2, other alpha is a mix.

?scale
activities_scaled = scale(hobbies)
head(activities_scaled)

attr(activities_scaled, "scaled:scale")

sapply(activities_scaled,class)

?glmnet
L1lm = glmnet(x=activities_scaled, y=o_collect[[1]], alpha=1)
L2lm = glmnet(x=activities_scaled, y=o_collect[[1]], alpha=0)

names(L1lm)

L1lm["lambda"]
L2lm["lambda"]
L1lm$lambda.min

###RMSE###
rmse = function(x_hat, x){
  (x_hat - x)^2 %>%
    mean() %>%
    sqrt()
}
######

get_rmses = function(fit, df, target){
  rmsecollect = c()
  for (lambda in fit[["lambda"]]){
    predictions = predict(fit, df, s=lambda)
    rmsecollect = c(rmsecollect, rmse(predictions, target))
  }
  rmsecollect
}

rmselist= get_rmses(fit=L1lm, df=activities_scaled, target=o_collect[[1]])

str(L1lm)

L1graph = qplot(L1lm[["lambda"]], rmselist)
L2graph = qplot(L2lm[["lambda"]], get_rmses(fit=L2lm, df=activities_scaled, target=o_collect[[1]]))

multiplot(L1graph, L2graph)

?cv.glmnet()

cvlambda = cv.glmnet(x=activities_scaled, y=o_collect[[1]])

cvlambda1 = cv.glmnet(x=activities_scaled, y=o_collect[[1]], alpha=1)
names(cvlambda)
head(cvlambda1)

plot1=qplot(cvlambda1$lambda, cvlambda1$cvm)

cvlambda2 = cv.glmnet(x=activities_scaled, y=o_collect[[1]], alpha=0)
plot2= qplot(cvlambda2$lambda, cvlambda2$cvm)
plot0 = qplot(cvlambda$lambda, cvlambda$cvm)

multiplot(plot1, plot2, plot0)
cvlambda1$lambda.min

#Stepwise regression tends to overfit bc of probs with multiple hypothesis testing. In practice, prefer regularization.

##Implement 20-fold cross validation
#Note: make the first column the predicted column



names(malehobbies)


#START FUNCTION
nfold_cv_multi = function(df, n_folds) {

  preds = numeric(nrow(df))
  
  ##glmnet application
  scaledf = scale(df[-1])
  #scaletest = scale(df_test[-1], attr(scaletrain, "scaled:scale"))    
  
  L1fit = cv.glmnet(x=scaledf, y=df[[1]], alpha=1, nfolds = n_folds)
  L2fit = cv.glmnet(x=scaledf, y=df[[1]], alpha=0, nfolds = n_folds)
  
  predsL1 = predict(L1fit, scaledf, s=L1fit$lambda.min)
  predsL2 = predict(L2fit, scaledf, s=L2fit$lambda.min)
  
  L1rmse = rmse(x_hat=predsL1, x=df[[1]])
  L2rmse = rmse(x_hat=predsL2, x=df[[1]])
  
  #end glmnet application
  
  # Calculate the different folds for lmnet
  folds = sample(nrow(df)) %% n_folds + 1
  
  for (i in 1:n_folds) {
    df_train = df[folds != i, ]
    df_test = df[folds == i, ]
    
    lmfit = lm(attr_o ~ ., df_train)
    lmstep = step(lmfit, formula(lm(attr_o ~ ., df)), direction = "backward", trace=0)
         
    preds[folds == i] = predict(lmstep, df_test)
    
  }
  lmrmse = rmse(x_hat=preds, x=df[[1]])
  
  list(lm = lmrmse, L1 = L1rmse, L2 = L2rmse, lambdabest=c(L1fit$lambda.min, L2fit$lambda.min))
}  
#END FUNCTION

rmsecollect = nfold_cv_multi(malehobbies, n_folds=20)
rmsecollect

#Set grid parameters
param_grid = expand.grid(alpha = 1:10 * 0.1, lambda = 10^seq(-4,0,length.out=10))
#Set 10-fold cross-validation (repeat x3)
control = trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)
#Search over grid
caret_fit = train(x=hobbies, y=o_collect[[1]], method="glmnet", tuneGrid=param_grid, trControl=control, preProc=c("center", "scale"))
#View optimal vals of alpha, lambda
caret_fit$bestTune
#View cross-val RMSE estimates
caret_fit$results$RMSE

#Caret fit generate a lambda larger than the best fit lamda of L1, and smaller than the best fit lambda of L2 
#Lambdas: caret (w/alpha=0.2): 0.129155 L1: 0.02893414 L2: 0.30311864) 

#We will see penalty functions again in: sparse PCA, penalized SVMs

#glmnet note: features = scaled matrix of predictors, target = numeric vector
##scale returns a matrix, so this transform is semi-automatic if you keep the above in mind.

#glmnet will find your lambda for you, but remember to specify which lambda to use in predict as s=lambda (esp. fit$lambda.min)
#do similar when extracting coef(fit, s=lambda)

cvlambda$lambda.min #lambda minimizing cv MSE
cvlambda$lambda.1se #greatest value of lamda within 1 standard error of lambda.min

# "warm starts" enable glmnet to operate quickly across large numbers of lambda values. If you don't like its default lambda values,
# it's preferred that you change nlambda or lambda.min.ratio to change the range specified, rather than inserting a single value.