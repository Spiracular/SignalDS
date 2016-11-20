#Kaggle Bikes

library("dplyr")
library("caret")
library("caretEnsemble")
#install.packages("lubridate")
#install.packages("scales")
library("lubridate")
library("scales")

kbtrain = read.csv("KaggleBikeSharing/train.csv")
kbtest = read.csv("KaggleBikeSharing/test.csv")

#Output prediction with format: datetime, count

head(kbtrain)
head(kbtest)

sapply(kbtrain, class) #Note: datetime is a factor at the moment
# #Right... datetime is a factor, I need to make it a character, THEN some of this stuff should work.
# kbtrain$datetime= as.character(kbtrain$datetime)
# kbtrain$datetime = as.POSIXct(strftime(kbtrain$datetime))
# kbtrain["hour"] = hour(kbtrain$datetime) #Still not working

kbtrain$hour  = hour(ymd_hms(kbtrain$datetime)) #Good, it's 24-hour time.
kbtest$hour  = hour(ymd_hms(kbtest$datetime))

unique(kbtrain$hour)

###Additional note: datetime will probably be a PAIN to handle with this kind of regression, and may even have to be ignored entirely.
###I could at least go after getting the 24-time hour into a column, I guess... (which SHOULD be a factor, lets be honest)
###Wish I could throw on something to handle time better...

#as.Date() is a thing
?as.Date

#Know what? I'd be happy with having a factor for day of the week, and an integer for time of day (if I can get nonlinear modeling to cooperate with it)

head(kbtrain)

ggplot(data = kbtrain, aes(x=hour,y=count)) + geom_point() + geom_smooth() #+ geom_jitter()
ggplot(data = kbtrain, aes(x=hour,y=count, color=weather)) + geom_point() +geom_jitter() + geom_smooth() #Count vs. hour, Bimodal
ggplot(data = kbtrain, aes(x=atemp,y=count, color=weather)) + geom_point() +geom_jitter() + geom_smooth() #atemp vs. count, Reasonably linear
ggplot(data = kbtrain, aes(x=atemp,y=count, color=season)) + geom_point() + geom_smooth()
ggplot(data = kbtrain, aes(x=atemp,y=count, color=season)) + geom_point() + geom_jitter() + geom_smooth()

ggplot(data = kbtrain, aes(x=temp,y=count)) + geom_point() + geom_smooth()#temp and atemp are comparable, and roughly linear
ggplot(data = kbtrain, aes(x=humidity,y=count, color=temp)) + geom_point() + geom_smooth() #Humidity: roughly linear downwards, with bizarrely low vaues if 0?
ggplot(data = kbtrain, aes(x=windspeed,y=count, color=temp)) + geom_point() + geom_smooth() #Looks like NO EFFECT (almost perfectly flat)


ggplot(data = kbtrain[kbtrain$holiday==1,], aes(x=hour,y=count, color=weather)) + geom_point() + geom_smooth()
#On holidays, it's a beautiful bell curve centered on noon
ggplot(data = kbtrain[kbtrain$workingday==0,], aes(x=hour,y=count, color=weather)) + geom_point() + geom_smooth()
#Non-working-days (aka weekends) get essentially the same curve as holidays, with a lot more data
ggplot(data = kbtrain[kbtrain$workingday==1,], aes(x=hour,y=count, color=weather)) + geom_point() + geom_smooth()
#On workdays, usage peaks at 7(7am) and 18 (6pm)

###I'm not seeing anything majorly skewed


ggplot(data = kbtrain[kbtrain$holiday==1,], aes(x=hour,y=count, color=weather)) + geom_point() + geom_smooth()

#I am tempted to train workingdays separately from non-workindays, and then apply the predicting functions according to those.



#Make matrix of input data
trainmatrix = as.matrix(select(kbtrain, -datetime, -casual, -registered, -count))
testmatrix = as.matrix(select(kbtest, -datetime))




ensemble_control = trainControl(method="repeatedcv", repeats=1, number=3, verboseIter=TRUE, savePredictions="final") 

ensemble_methods = c('glmnet', 'earth', 'rpart', 'ranger')

ensemble_fits = caretList(trainmatrix, kbtrain$count,
                              trControl = ensemble_control,
                              methodList = ensemble_methods,
                              preProcess=c("center","scale")
)
glm_fits_ensemble = caretStack(ensemble_fits,
                               method="glm",
                               metric="RMSE",
                               trControl=ensemble_control
)
gbm_fits_ensemble = caretStack(ensemble_fits,
                                method="gbm",
                                metric="RMSE",
                                trControl=ensemble_control
)

summary(glm_fits_ensemble)
glm_fits_ensemble$error$RMSE #Interesting! The glm fit works better than the gbm. Okay then.

summary(gbm_fits_ensemble)
min(gbm_fits_ensemble$error$RMSE)

predicttrain = predict(glm_fits_ensemble, trainmatrix)

predicttest = predict(glm_fits_ensemble, testmatrix)
predicttest[predicttest<0] =  0 
#(Less than 0 is a nonsense value)

solution = data.frame(datetime=kbtest$datetime, count=predicttest)
write.csv(solution, "solution.csv", row.names=FALSE)

#A Note: this implmentation is only the slightest bit different from Random Forest. It's basically just random forest.



#RANK: 1880 or so. Score of about 51751.


#Next step: generate predictions from 

# ensemble_tunes_gbm = list(
#   glmnet = caretModelSpec(method='glmnet', tuneGrid = glmgrid), #I'm going to be lazy and let it tune itself
#   earth=caretModelSpec(method='earth', tuneGrid=MARSgrid),
#   rpart=caretModelSpec(method='rpart', tuneGrid=treegrid),
#   gbm = caretModelSpec(method='gbm', tuneGrid=nxt_gbmgrid) 
# )







#BoxCox is a transform that generates an approximately-normal dist from many datasets by reducing skewedness.
#Box Cox: {y^lambda - 1} / lambda 
#(or ln(y) for lambda=0)

#Power Transform: T(Y) = (Y^lambda - 1) / {lambda*GM(Y)^(lambda-1)}
#(or, for lambda=0, GM(y)ln(Y))

#Where GM(Y) = geometric mean = Pi_n(x^(1/n))

trainmatrix2 = trainmatrix[,-3]

ensemble_control = trainControl(method="repeatedcv", repeats=1, number=3, verboseIter=TRUE, savePredictions="final") 

ensemble_methods_2 = c('glmnet', 'earth', 'ranger', 'gbm')

workindex = trainmatrix[,3]==1

ensemble_fits_workfree = caretList(trainmatrix2[!workindex,], kbtrain$count[!workindex],
                          trControl = ensemble_control,
                          methodList = ensemble_methods_2,
                          preProcess=c("center","scale")
)
ensemble_fits_workwork = caretList(trainmatrix2[workindex,], kbtrain$count[workindex],
                                   trControl = ensemble_control,
                                   methodList = ensemble_methods_2,
                                   preProcess=c("center","scale") ###YOU CAN PUT BOXCOX INTO PREPROCESSING (for factors)
                                   #"BoxCox"
)

weekend_fit = caretEnsemble(ensemble_fits_workfree)
work_fit = caretEnsemble(ensemble_fits_workwork)

testmatrix2 = testmatrix[,-3]
testworkindex = testmatrix[,3] == 1

summary(weekend_fit)
weekend_fit$error$RMSE

summary(work_fit)
work_fit$error$RMSE

#Okay, it actually performed worse, at 2053 ranking. I did not expect that.
#I guess it already accounts for the impact of workday in the original model.
#(Or adding gbm was a bad idea? *shrug*)


predict2workfree = predict(weekend_fit, testmatrix2[!testworkindex,])
predict2work = predict(work_fit, testmatrix2[testworkindex,])

nrow(kbtest) #6493
solmat = matrix(nrow=6493, ncol=2)

solmat[!testworkindex,2] = predict2workfree
solmat[testworkindex,2] = predict2work
predicttest = solmat[,2]
predicttest[predicttest<0] =  0 

solution2 = data.frame(datetime=kbtest$datetime, count=predicttest)
write.csv(solution2, "solution2.csv", row.names=FALSE)





