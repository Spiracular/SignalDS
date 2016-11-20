#Nonlinear Regression

#Recommended reading: Applied Predictie Modeling, Introduction to Statistical Learning

#Portuguese "Vinho Verde" wine
# Input variables (based on physicochemical tests):
#   1 - fixed acidity
# 2 - volatile acidity
# 3 - citric acid
# 4 - residual sugar
# 5 - chlorides
# 6 - free sulfur dioxide
# 7 - total sulfur dioxide
# 8 - density
# 9 - pH
# 10 - sulphates
# 11 - alcohol
# Output variable (based on sensory data): 
#   12 - quality (score between 0 and 10)

#install.packages("gbm")
#install.packages("caretEnsemble")
library("gbm")
library("dplyr")
library("ggplot2")
library("caret")
library("caretEnsemble")

whitewine = read.csv(file="winequality-white.csv", sep=";")

#Automatically replaces spaces with dots in names; I want to replace that with underscores.
colnames(whitewine) = gsub(pattern="[.]",replacement="_",colnames(whitewine))

head(whitewine)
dim(whitewine) #4898 x 12

sapply(whitewine, class) #Everythign is a numeric, quality is an integer
sapply(whitewine, function(x) sum(is.na(x))) #No NA detected


#for (i in 1:length(whitewine)){
names(whitewine)
qplot(y= quality, x=free_sulfur_dioxide, data=whitewine) + geom_smooth()
#Sys.sleep(5)}

#fixed_acidity: linear downward
#volatile_acidity: candidate for downward semi-linear or wavy correlation
#citric_acid: Normal dist? Log-normal?
#residual_sugar: bimodal; STRONG
#chlorides: peak early, trail downward; weak
#free_sulfur_dioxide: parabolic or bimodal, VERY STRONG correlation in initial parabolic section
#total_sulfur_dioxide: ...hard to describe? upward slope, then curve downward.
#density: logarithmic dip down with no upper plateau; STRONG
#pH: roughly flat correlation
#sulfates: wiggly flat line; assume roughly uncorrelated?
#alcohol: gentle sine upwards, probably meaningful; STRONG

#free_sulfur_dioxide appears to be a good example of a strong nonlinear correlation, alcohol is an even more orderly one.


#run glmnet once to get some initial lambda estimates?

#Elastic Net Regression for Baseline Performance

caret_reg = function(x, y, method, grid, ...) {
  set.seed(1)
  control = trainControl(method="repeatedcv", repeats=1,
                         number=3, verboseIter=TRUE)
  train(x=x, y=y, method=method, tuneGrid=grid,
        trControl=control, metric="RMSE",
        preProcess=c("center", "scale"), ...)
}

gridset = expand.grid(alpha=seq(0,1,0.1), lambda=2^seq(1,-4,length.out=20))
trainwine = select(whitewine, -quality)

glmnet_reg = caret_reg(x=trainwine, y=whitewine$quality, grid=gridset, method="glmnet")

results = data.frame(cbind("method" = NA, "min_rmse" = NA))
results[1,1] = "glmnet"
results[1,2] = min(glmnet_reg$results$RMSE)

#K-Nearest Neighbors

gridk = expand.grid(k=seq(1,20))

glmnet_knn = caret_reg(x=trainwine, y=whitewine$quality, grid=gridk, method="knn")

results[2,]  = list("knn", min(glmnet_knn$results$RMSE))

###Wow, knn definitely outperformed glmnet here. 0.717 is better than 0.755 .

#Multivariate Adaptive Regression Splines (MARS)
###MARS basically functions by shifting the equation used to model curve when predictor values hit certain ranges.

MARSgrid = expand.grid(degree=1:5, nprune=10:30)
#degree = degree of model (lines vs. polynomials), nprune = maximum number of additive terms in model (complexity)
earth_mars = caret_reg(x=trainwine, y=whitewine$quality, grid=MARSgrid, method="earth")

summary(earth_mars$finalModel)

results[3,] = list("MARS", min(earth_mars$results$RMSE))
###min RMSE between KNN and linear, at 0.7215

#Decision Tree
##rpart = Classification And Regression Tree (CART)

#cp = complexity parameter = liit to splitting, usually expressed in units of "impurity"
#impurity can be understood as proportional to variance in target captured by splits in the variable

treegrid  = expand.grid(cp=10^seq(-3,0,length.out=10))

regress_tree = caret_reg(x=trainwine, y=whitewine$quality, grid=treegrid, method="rpart")

plot(regress_tree$finalModel) #Aw, there's a tree but no labels :(

regress_tree$finalModel
#Sorts first by alcohol content. If low, next sort is volatile acidity
#Highest average-value outcome pathway description (7.42 quality, item 253):
##High alcohol (>=0.273), low fixed_acidity(<1.53), residual_sugar >= -0.91, low free_sulfur_dioxide and low sulfates.

results[4,] = list("rpart", min(regress_tree$results$RMSE))

?rpart.object
regress_tree$finalModel$cptable #[[,1]]

###As cp goes up, nsplit goes down and the relative error (fraction of error vs. selecting the overall mean?) increases

#Regression Tree

###Each regression tree is trained on a bootrstrap, and only some of the predictors are allowed each split (to decorrelate the trees).
###Predictions from all trees get aggregated at the end (bagging).

#mtry = size of subset of parameters considered for each split.
#Advised mtry to try w/ parameters p: floor(sqrt(p)), floor(p/3), p

mtrygrid = expand.grid(mtry=2:6)

random_tree = caret_reg(x=trainwine, y=whitewine$quality, grid=mtrygrid, method="ranger", importance="impurity")
#ranger is kinda just better than plain rf; faster and paralellized implementation.

results[5,] = list("ranger rf",  min(random_tree$results$RMSE))
#Performs quite well: min_rmse = 0.62

#Finding calculated impurities
?ranger

##'impurity' measure is the Gini index for classification and the variance of the responses for regression

random_tree$finalModel$variable.importance
#alcohol has the highest response variance, followed by density, volatile_acidity, and free_sulfur_dioxide

oob_error= random_tree$finalModel$prediction.error #0.343, but in MSE units
sqrt(oob_error) #0.586 when converted to RMSE units

### oob error of 0.586 is lower than the calculated RMSE of 0.620

### A note that when you don't have to compare the random forest to other methods, minimizing oob saves some computational time.

###Random Forests w/ mtry floor(sqrt(p)) can apparently be a pretty low-maintenance way to first-pass test on whether nonlinear methods
###cause substantial improvement in prediction quality for a dataset.


#Gradient Boosted Trees

###AdaBoost sounds cool; overweights data misclassified by the current ensemble when fitting new classifiers
###The concept was later developed, from weights to gradient-descent with special loss function, into Gradient Boosting

#Gradient Boosted Trees make for one of the best-performing off-the-shelf ML models, but are more time-consuming than Random Forests

#Trains new trees on the residuals of predictions made by current trees.

?gbm
#n.trees = total number of trees to fit (same as iterations)
#shrinkage = shrinkage parameter applied to each tree (aka learning rate, step-size reduction) [how much smaller each subsequent tree's effect is?]
#interaction.depth = maximum depth of variable interactions (1=additive, 2=...what? ####Super-unclear! Number of splits made total, apparently.
#n.minobsinnode = minimum numeber of observations in terminal leaf nodes

gbmgrid = expand.grid(n.trees=500, shrinkage=seq(0.01,0.1,0.03), interaction.depth=c(1,5,10,20,40), n.minobsinnode=1:3)
#remove interaction depth greater than 50; it's ignored by the code anyway.
featrix = as.matrix(trainwine)

Gradient_trees = caret_reg(x=featrix, y=whitewine$quality, grid = gbmgrid, method="gbm")

##Yes, this is pretty damn time-consuming.

##The gbm programming warning complains "You really don't need that much interaction depth; 5 is usually plenty adequate..."

###So, each tree in an ensemble is sequentially dependent and can't be paralellized, but separate trees can be.
###Paralellization is implemented in xgboost package.

summary(Gradient_trees$finalModel) #alcohol has the strongest influence, followed by free_sulfur_dioxide

Gradient_trees$finalModel #Gradient-boosted model with Gaussian loss function

Gradient_trees$bestTune (n.trees = 500, interaction.depth = 40, shrinkage = 0.04, n.minobsinnode = 2)

nxt_gbmgrid = expand.grid(n.trees=seq(500,5000,100), interaction.depth = 40, shrinkage = 0.04, n.minobsinnode = 2)
Better_G_Tree = caret_reg(x=featrix, y=whitewine$quality, grid=nxt_gbmgrid, method="gbm")
#caret terms this Stochastic Gradient Boosting

Better_G_Tree$bestTune


results[6,] = list("gbm gradient boosted tree",  min(Better_G_Tree$results$RMSE))

#Cubist

###"Cubist is a nonlinear decision tree based regression algorithm... performs approximately as well as a gbm, with 2 hyperparameters.
###...the exact functionality of the algorithm is somewhat opaque due to its formerly proprietary nature, so Cubist receives relatively
###little attention and analysis relative to other methods."

###Makes a tree of linear models, final model smoothed by previous models. (technique: "rule-based model)
###Boosting-like in the use of residuals to train new trees (strangely, calls trees the committees)
###Adjusts final predictions with a complicated kNN derivative?

cubist_grid = expand.grid(committees=seq(30,50,5), neighbors=5:9)
#committees (n.trees analog), neighbors.committees (kNN-variant parameter)

cubist_fit = caret_reg(x=featrix, y=whitewine$quality, grid=cubist_grid, method="cubist")

cubist_fit$bestTune

results[7,] = list("cubist", min(cubist_fit$results$RMSE))

#Stacking: ENR, MARS, and Regression Trees ensemble

?caretEnsemble

ensemble_methods = c('glmnet', 'earth', 'rpart') ###Ensemble methods is not necessary when you have ensemble tunes; this code produces 6 models!!!
ensemble_control = trainControl(method="repeatedcv", repeats=1, number=3, verboseIter=TRUE, savePredictions="final") 
#Note:  it wants me to set resampling indices for consistency?
ensemble_tunes = list(
  glmnet = caretModelSpec(method='glmnet', tuneGrid=gridset), #RERUN
  #tunegrid=gridset ###Okay, fine. In the ensemble implementation, glmnet REALLY wants to tune itself. Oh! I neglected to capitalize Grid.
  earth=caretModelSpec(method='earth', tuneGrid=MARSgrid),
  rpart=caretModelSpec(method='rpart', tuneGrid=treegrid)
)

#Making list of train() fits with caretList()

ensemble_fits = caretList(featrix, whitewine$quality,
                          trControl=ensemble_control,
                          methodList=ensemble_methods,
                          tuneList=ensemble_tunes,
                          preProcess=c("center","scale"))

#runs into problems with fixed_acidity, for some reason...
#complains about the presence of a grid for glmnet... (tried without grid and it tuned itself, so... fine. I'll let it do that.)

fit_ensemble = caretEnsemble(ensemble_fits)#Bunch of rank-deficient fit complaints...
print(fit_ensemble) #RSE: 0.714, Rsquared: 0.349
summary(fit_ensemble)

results[8,] = list("Ensemble1 glmnet earth rpart", fit_ensemble$error$RMSE)

#add k-nearest neighbors to the mix.

ensemble_methods_n = c('glmnet', 'earth', 'rpart', 'knn')
ensemble_tunes_n = list(
  glmnet = caretModelSpec(method='glmnet', tuneGrid=gridset), #RERUN
  earth=caretModelSpec(method='earth', tuneGrid=MARSgrid),
  rpart=caretModelSpec(method='rpart', tuneGrid=treegrid),
  knn = caretModelSpec(method='knn', tuneGrid = gridk)
)

ensemble_fits_n = caretList(featrix, whitewine$quality,
                            trControl = ensemble_control,
                            methodList = ensemble_methods_n,
                            tuneList = ensemble_tunes_n,
                            preProcess=c("center","scale"))

fit_ensemble_n = caretEnsemble(ensemble_fits_n)
print(fit_ensemble_n)
summary(fit_ensemble_n)

results[9,] = list("Ensemble2 glmnet earth rpart knn", fit_ensemble_n$error$RMSE)

#                             method  min_rmse
# 1                           glmnet 0.7553755
# 2                              knn 0.7174000
# 3                             MARS 0.7215107
# 4                            rpart 0.7457397
# 5                        ranger rf 0.6199399
# 6        gbm gradient boosted tree 0.6260960
# 7                           cubist 0.6768097
# 8     Ensemble1 glmnet earth rpart 0.7143414
# 9 Ensemble2 glmnet earth rpart knn 0.6837065


?caretStack
#A note on stacks: not simple linear weights, but more susceptible to overfitting and require large sets of resamples (n=50 or higher for bootstrap)

ensemble_methods_gbm = c('glmnet', 'earth', 'rpart', 'gbm')
ensemble_tunes_gbm = list(
  glmnet = caretModelSpec(method='glmnet', tuneGrid = gridset), #RERUN
  #tunegrid=gridset ###Okay, fine. In the ensemble implementation, glmnet REALLY wants to tune itself. Oh! I neglected to capitalize Grid.
  earth=caretModelSpec(method='earth', tuneGrid=MARSgrid),
  rpart=caretModelSpec(method='rpart', tuneGrid=treegrid),
  gbm = caretModelSpec(method='gbm', tuneGrid=nxt_gbmgrid) #Yes I'm using the shorter grid. I don't want to wait ages.
)


ensemble_fits_gbm = caretList(featrix, whitewine$quality,
                              trControl = ensemble_control,
                              methodList = ensemble_methods_gbm,
                              tuneList = ensemble_tunes_gbm,
                              preProcess=c("center","scale")
                              )


fits_ensemble_glm = caretStack(ensemble_fits_gbm,
                               method="glm",
                               metric= "RMSE",
                               trControl=ensemble_control
                                )

fits_ensemble_gbm = caretStack(ensemble_fits_gbm,
                               method="gbm",
                               metric="RMSE",
                               trControl=ensemble_control
                               )
print(fits_ensemble_glm)
summary(fits_ensemble_glm)

results[10,] = list("Ensemble3: a glm on glmnet earth rpart gbm", fits_ensemble_glm$error$RMSE)

print(fits_ensemble_gbm)
summary(fits_ensemble_gbm)

results[11,] = list("Ensemble4: a gbm on glmnet earth rpart gbm", min(fits_ensemble_gbm$error$RMSE))

fits_ensemble_gbm$error$RMSE

results[12,] = list("rerun Ensemble3: a glm on glmnet earth rpart gbm", fits_ensemble_glm$error$RMSE)
results[13,] = list("rerun Ensemble4: a gbm on glmnet earth rpart gbm", min(fits_ensemble_gbm$error$RMSE))

fit_ensemble_ensemble = caretEnsemble(ensemble_fits_gbm)

results[14,] = list("caretEnsemble that should match E3", fit_ensemble_ensemble$error$RMSE)


# results
#                                             method  min_rmse
# 1                                            glmnet 0.7553755
# 2                                               knn 0.7174000
# 3                                              MARS 0.7215107
# 4                                             rpart 0.7457397
# 5                                         ranger rf 0.6199399
# 6                         gbm gradient boosted tree 0.6260960
# 7                                            cubist 0.6768097
# 8                      Ensemble1 glmnet earth rpart 0.7143414
# 9                  Ensemble2 glmnet earth rpart knn 0.6837065
# 10       Ensemble3: a glm on glmnet earth rpart gbm 0.6242318
# 11       Ensemble4: a gbm on glmnet earth rpart gbm 0.6209175
# 12 rerun Ensemble3: a glm on glmnet earth rpart gbm 0.6190948
# 13 rerun Ensemble4: a gbm on glmnet earth rpart gbm 0.6152693
# 14               caretEnsemble that should match E3 0.6202518 <-Hm...


#gbm stacking had slightly lower RMSE, at 0.621 vs. 0.624

#General tips for predictive regression modeling:
#-radom forest is a good first-pass to test for nonlinearity
#-gradient boosted trees generally performs even better
#-(not yet covered methods: neural nets, support vector regression)
#-We used grid search, but for large complicated methods it's often better to use *random* search
###, or alternatively: grid search and racing
# BUT may be inefficient to use random search for gbt, random forests, etc. in caret's implementation w/ sub-model trick
#-Get baseline linear performance from elastic net regularized linear regression