#Recommender Systems

##Collaborative Filtering on the MovieLens Dataset

#install.packages("softImpute")

library("dplyr")
library("ggplot2")
library("Rmisc")
library("softImpute")
library("DAAG")
library("pROC")
library("glmnet")
library("corrplot")
library("dummies")

setwd(dir = "SignalDS/")

columnnames = c("UserID", NA, "MovieID", NA, "Ratings", NA, "Timestamp")

ratingdf0 = read.csv("ml-1m/ratings.dat", sep=":", col.names = columnnames, header=FALSE)

?read.csv

Selection = rep(c(TRUE, FALSE), length = length(ratingdf0))
ratingdf = ratingdf0[Selection]
ratingdf = select(ratingdf, -Timestamp)
dim(ratingdf)

head(ratingdf)

sapply(ratingdf, class)
unique(ratingdf$Ratings)
mean(ratingdf$Ratings)
sd(ratingdf$Ratings)

#How do I get average rating for each UserID and histogram it? Slightly harder than I initially though, I'll get back to it.
#histable = 

#UserGroups = group_by(ratingdf, UserID)
#summarise(UserGroups, mean=mean(ratingdf), sd=sd(ratingdf))

UserGroups[2]

#Get count of uniques
sapply(ratingdf, function (x) length(unique(x)))

sapply(ratingdf, max)
###There must be some Movie designations that do not have ratings in this dataset

set.seed(3)

#Set a training set with 80%, test set with 20%.
###Complicated way: createDataPartition in caret
?split()

divis = round(nrow(ratingdf)/4)
sindex = sample(nrow(ratingdf))

test = ratingdf[ sindex[1:divis], ]
train = ratingdf[ sindex[(divis+1):nrow(ratingdf)], ]

#Fake UserID = 6041, Fake Movie ID = 3953
avgRating = mean(ratingdf$Ratings)

fakemovie = cbind("UserID"=unique(ratingdf$UserID), "MovieID" = 3953 , "Ratings"=avgRating)
fakemovie[,3] = fakemovie[,3] + rnorm(nrow(fakemovie), mean=0, sd=0.01)

fakeuser = cbind("UserID" = 6041, "MovieID" = c(1:max(ratingdf$MovieID)), "Ratings" = avgRating)
fakeuser[,3] = fakeuser[,3] + rnorm(nrow(fakeuser), mean=0, sd=0.01)

trainfake = rbind(train, fakemovie, fakeuser)

?Incomplete
sparseInc = Incomplete(trainfake$UserID, trainfake$MovieID, trainfake$Ratings)
dim(sparseInc)

?biScale
scalesparse = biScale(x=sparseInc, maxit=5)

lam0 = lambda0(scalesparse)
#lam0 = 98.35, lowest regularization param that yields a zero-matrix
#log(lam0) = 4.589


lambdas = seq(log(lam0), 0, length.out = 20)

exes = numeric(20)
for (i in 1:20){
  exes[i] = exp(lambdas[i])
}

data.frame()
results = data.frame("lambda" = exes, "rank"=NA, "rmse"=NA)
?softImpute

ranks = integer(20)
rmses = numeric(20)
fits = numeric(20)
SIs = vector("list", length=20) #SIs is now fits.

SIs[[1]] = softImpute(scalesparse, lambda = exes[1], rank.max=30, maxit=1000)

for (i in 2:20){
  SIs[[i]] = softImpute(scalesparse, lambda = exes[i], rank.max=30, maxit=1000, warm.start = SIs[[(i-1)]], trace.it=TRUE)
}

?round

for (i in 1:20){
  ranks[i] = sum(round(SIs[[i]]$d, digits=4)!=0)
}
results$rank=ranks

?impute

###
rmse = function(x_hat,y) sqrt(mean((x_hat-y)^2))
###

for (i in 1:20){
  preds = impute(object=SIs[[i]], trainfake$UserID, trainfake$MovieID)
  rmses[i]= rmse(preds, trainfake$Ratings)
}
results$rmse = rmses
head(results)


rmse_vs_fakes = rmse(x_hat = preds[760139:760149], trainfake$Ratings[760139:760149])
#RMSE vs. fakes (for SI lambda=98) is about 0.84. That's really high! (it's lower than the overall average, though, and rmse gets better for lower lambdas.)

preds = impute(object=SIs[[20]], trainfake$UserID, trainfake$MovieID)
rmse_vs_fakes = rmse(x_hat = preds[760139:760149], trainfake$Ratings[760139:760149])
#For the lowest lambda result (lambda=1), RMSE vs. fakes is about 0.3035 (and the error is about half the intensity of the overall )

##OH! I used training data where I should have used test data. D'oh! Try again.

results2 = data.frame("lambda" = exes, "rank"=NA, "rmse"=NA)

for (i in 1:20){
  ranks[i] = sum(round(SIs[[i]]$d, digits=4)!=0)
}
results2$rank=ranks

for (i in 1:20){
  preds = impute(object=SIs[[i]], test$UserID, test$MovieID)
  rmses[i]= rmse(preds, test$Ratings)
}
results2$rmse = rmses

trainplot = qplot(results$lambda, results$rmse) 
testplot = qplot(results2$lambda, results2$rmse)
multiplot(trainplot, testplot)

best_svd= SIs[which.min(results2$rmse)]

#results$rmse=rmses
#results$fits=fits

# softImpute(scalesparse, lambda = results$lambda[i], rank.max=30, maxit=1000)
# warm.start=
# 
# #("lambda","rank","rmse","fits")

#Evaluation Metrics for Collaborative Filtering

##Mean Absolute Error = mae

mae = function(x_hat, x) sum(abs(x_hat-x))/length(x)
maes = numeric(20)

preds = vector("list", 20)

for (i in 1:20){
  preds[[i]] = impute(object=SIs[[i]], test$UserID, test$MovieID)
  #maes[i]= mae(preds, test$Ratings)
}
results2$mae = maes

ggplot(results2, aes(x=lambda)) + geom_line(aes(y=rmse), col="blue") + geom_line(aes(y=mae), col="red")

##Getting Precision and Recall/Sensitivity
#Precision = T+/test+, Recall= T+/real+
avgRating

Precision = preds[[i]], 

TestTruePlus = sum(test$Ratings>avgRating & preds[[i]]>avgRating)
TestPlus = sum(test$Ratings>avgRating)
RealPlus = sum(preds[[i]]>avgRating)

recalls = numeric(20)
precises = numeric(20)

TestP = test$Ratings>avgRating
for (i in 1:20){
  precises[i] = sum(TestP & preds[[i]]>avgRating)/sum(TestP)
  recalls[i] = sum(TestP & preds[[i]]>avgRating)/sum(preds[[i]]>avgRating)
}
results2$Precision = precises
results2$Recall = recalls

ggplot(results2, aes(x=lambda)) + geom_point(aes(y=Precision), col="yellow") + geom_point(aes(y=Recall), col="green")

results2[which.max(results2$Recall),] #Recall maximized at lambda=18.14
results2[which.max(results2$Precision),] #Precision maximized at lambda=23.1
results2[which.min(results2$rmse),] #RMSE min at lambda=23.1
results2[which.min(results2$mae),] #Ditto for mae (as expected; RMSE is essentially mae with parts squared, no?)

#Asymmetric cost function

Ltp = t(matrix(c(0,0,0,7.5,10,
               0,0,0,4,6,
               0,0,0,1.5,3,
               3,2,1,0,0,
               4,3,2,0,0), ncol=5, nrow=5))
#Note to self: matrices are automatically generated transpose (reading up-down before left-right)

asymcosts = numeric(20)
start = matrix(rep(0,25), nrow=5)
for (i in 1:20){
  roundpreds = round(preds[[i]])
  roundpreds[roundpreds>5] = 5
  roundpreds[roundpreds<1] = 1
  asymcosts[i] = mean(sapply(1:length(roundpreds), function(p) Ltp[test$Ratings[p], roundpreds[p]]))
}
test$Ratings[1]

results2$Asym = asymcosts

results2[which.max(results2$Asym),] #Asymetrical cost minimized at lambda=1

#Alright, lets walk through this again...
#Start by rounding off all the predictions so I can get indices...
#OH! I want to generate a matrix with numbers where the set of 2 coordinates go, +1 for each pairing. Then dot-product w/ cost.HA!

mat25 = matrix(1:25, ncol=5)
mat25[c(1,2,5), c(1,2,2)] = NA
#Yeah, no, this does not work the way I'd need it to. I need to actively slot in singlely, not as vectors.

usrcolnames= c("UserID", "Gender","Age","Occupation","Zip-code")
movcolnames=  c("MovieID","Title","Genres")

movies= read.csv("ml-1m/movies.dat", sep="~", col.names = movcolnames, header=FALSE)
head(movies)
movies$Genres= lapply(movies$Genres, function(g) strsplit(toString(g), split="\\|"))
 ###(Okay, double-backslash not single-backslash)

genreNames = unique(c(unlist(movies$Genres)))

#movies[, genreNames] = 0
GenreIndices = lapply(genreNames, function (gn) grepl(pattern=gn, x=movies$Genres))

movies[, genreNames]= t(GenreIndices)
sapply(movies, class)

str(best_svd)
dim(best_svd$v)
#Oh dear, I need to go inside a list of 1. Changing that...
#Row as movieID, column as Score
best_svd = best_svd[[1]]

#left_join(x,y,by=)

dim(movies)

#MovieID came out correctly, as an integer, for me?

movieVsvd = best_svd$v[movies$MovieID,]
dim(movieVsvd)
edited_movies = cbind(movies, "svd_V" = movieVsvd)

#all the Vs roughly represent 30 factors

DFdrama= select(edited_movies, Drama, starts_with("svd_V"))

head(edited_movies)
###glm logistic regression

Dramaglm = glm(Drama~., family="binomial", data=DFdrama)
?CVbinary()

CVBdrama= CVbinary(Dramaglm, nfolds=10)

?roc

dramapredict = predict(Dramaglm, DFdrama)
ROCurve= roc(DFdrama$Drama, dramapredict)
plot(ROCurve)

ViewingDramaFrame = data.frame("Title"=movies$Title, "Genres" = movies$Genres, "Drama"=movies$Drama, "Probability"=dramapredict)
ViewingDramaFrame= ViewingDramaFrame[order(ViewingDramaFrame$Probability, decreasing=TRUE),]
head(ViewingDramaFrame)

#The "Least likely to be dramatic" ratings are pretty great. Ghostbusters, followed by Austin Powers, followed by Airplane?
#Schindler's List is apparently the movie most likely to predict a dramaphile.
#Also: I think I'm still working with log-odds in the Probability column (just a self-note). I could fix that with this little function

log_odd_to_probs = function(L) exp(L) / (1+exp(L))


###Come back to later: try running a few more genres through the cut###

#usrcolnames
usersdf =read.csv("ml-1m/users.dat", sep="~", col.names = usrcolnames, header=FALSE)
head(usersdf)
sapply(usersdf, class)

userset = usersdf[(usersdf$Age>=35),]

careersort = sort(table(userset$Occupation), decreasing = TRUE)

careerset = names(careersort)[c(1,2,4,6)]

userset = userset[(userset$Occupation %in% careerset),]

#  Occupation is chosen from the following choices:
# *  0:  "other" or not specified
# *  1:  "academic/educator"
# *  2:  "artist"
# *  3:  "clerical/admin"
# *  4:  "college/grad student"
# *  5:  "customer service"
# *  6:  "doctor/health care"
# *  7:  "executive/managerial"
# *  8:  "farmer"
# *  9:  "homemaker"
# * 10:  "K-12 student"
# * 11:  "lawyer"
# * 12:  "programmer"
# * 13:  "retired"
# * 14:  "sales/marketing"
# * 15:  "scientist"
# * 16:  "self-employed"
# * 17:  "technician/engineer"
# * 18:  "tradesman/craftsman"
# * 19:  "unemployed"
# * 20:  "writer"

occjoinprep = select(userset, UserID, Occupation)

cbind()

#Note to self: U from best_svd has dimensions unique(trainfake$UserID) x k-factors. Index probably corresponds to user index.

factorjoinprep= cbind("UserID"= c(1:nrow(best_svd$u)), "svd_u" = as.data.frame(best_svd$u))

FactOcc = left_join(occjoinprep, factorjoinprep, by="UserID")

svd_us = as.matrix(select(FactOcc, starts_with("svd_u")))

sapply(FactOcc, class)

#left_join(x,y,by=)
levels(factor(FactOcc$Occupation))
#1, 6, 7, 17

OccModel = glmnet(x=svd_us, y=factor(FactOcc$Occupation), family="multinomial")

str(OccModel)

#Note to self: still working exclusively with the training+fakes dataset; predictions may be overfitted.

OccPredict = predict(OccModel, svd_us, s=0)
dim(OccPredict) #1127, 4, 1

#sum(log_odd_to_probs(OccPredict[2,]))


#PCA on resulting log odds
PCAloOcc= prcomp(x=OccPredict)

str(PCAloOcc)
head(PCAloOcc$x)
dim(PCAloOcc$x)

dim(PCAloOcc$rotation)

row.names(PCAloOcc$rotation) = c("academic", "doctor", "manager", "tech/eng")

corrplot(PCAloOcc$rotation, is.corr=FALSE)

##Interpretation?

##Full procedure description: take train&fakes matrix. softImpute (fit missing values). take factors from u.
# make a multinomial model from u factors and occupation. plug that into a PCA. Interpret.

##Interpretation: the corrplot above illustrates the 4 Principal Components of the occupation predictions by MNLog regression.
#PCAs could be vaguely termed: 1. "I'm an engineer not an academic/doctor.", 2. Academic, 3. Not an engineer, 4. Definitely not a manager.

#So... there's some meaningful axis of movie taste (PCA1) on which doctors and engineers are opposites, and this makes up the PC1 of the multinomial predictions.

str(OccModel) #Having some trouble locating the coeficients for each job category in the model.
###Go back to this later, I think it would help with testint and interpretation

#Can I look at the movies and persons that score/weight/whatevs best and worst along the factors stored in svd? (P.S. SVD = Singular Value Decomposition)

factorjoinprep[which.max(factorjoinprep$svd_u.V1),]

#v has got the movies, so lets look at svd_vs and their movies. (order by each bar, then spit out the movies from the dataset corresponding?)

#start by joining a sequence of movie numbers with the movies df, insisting on a row for each item.
#Get ordering indices for svd_v values
#Subset the moviesdf variant with the resultant indices

#~~Fuse to svd$v values and apply orderings?~~

###Oh nevermind, right, I have the subsetted svds coresponding to movies anyway.
###I could also just order edited_movies.

easy_view_movie = select(movies, MovieID, Title, Genres)

ordering = order(edited_movies$svd_V.12)

head(easy_view_movie[ordering,])
tail(easy_view_movie[ordering,])

easy_view_movie[ordering,][1:20,]

#svd_v.1 = ACTION THRILLER (anti-comedy)
#svd_v.2 = Cult classic (esp. Fight Club) to generally classic? (Hard to place, tbh)
#svd_v.3 = ACTION (to Comedy/Romance/Drama) (loves Braveheart, Terminator2, Start Wars, Die Hard...)
#svd_v.4 = Children's (to child-unfriendly; min is La Vita est Bella)
#svd_v.5 = Happy Adventure! (to depressing fatalism? Thriller Crime Drama Horror)
#svd_v.6 = Old Classics? (to late-80s-onward comedy dramas)
#svd_v.7 = Irreverent Comedy (max by space balls, then blues brothers, then princess bride)
  #(to thriller and drama, max by Sixth Sense, American Beauty, Pulp Fiction)
#svd_v.8 = Star Wars and Jurasic Park, (to obscurity)



dim(best_svd$v)
#Fake UserID = 6041, Fake Movie ID = 3953

###Estimating Different Career's Genre Preferences

#Next: calculating a vector of characteristic factor scores for each genre.
###Run unreg log regression of genre ~factors
genrelist = names(edited_movies)[4:21]
#genrelist[2] = "Children"
Mfactors = select(edited_movies, starts_with("svd_V"))
lmcollect = vector("list", 18)

Mfactors[[31]] = NA

for (gi in 1:length(genrelist)){
  g=genrelist[gi]
  #f=formula(paste0(g, "~."))
  Mfactors[[31]] = edited_movies[[g]]
  lmcollect[[gi]] = glm(V31~., family="binomial", data=Mfactors)
}

names(lmcollect) = genrelist
names(lmcollect[[1]])

svd_V_coefs = lapply(lmcollect, function(l) l$coefficients)
head(svd_V_coefs)

dfcoef = as.data.frame(svd_V_coefs)
colnames(dfcoef) = genrelist

corrplot(as.matrix(dfcoef), is.corr=FALSE)

set.seed(5)

?CVbinary() #get CV p-estimates for genre membership for each movie in dataset, and convert to log odds
pvals = vector("list", 18)
for (i in 1:18){
  pvals[[i]]= CVbinary(lmcollect[[i]])
}
names(pvals) = genrelist
str(pvals[[1]])
str(pvals[[1]]$cvhat)

#cvhat = probability of being the movie with that index?

LogOdd = function(P) log(P/(1-P))

#lapply(pvals, function(P) LoggOdd(P$cvhat))

#For each genre, multiply the factor scores by the log-odds of genre membership, then take the sum of all scaled vects of factor scores
Mfactors= Mfactors[,-31]

#3953 as highest-numbered movie, 3883 in data
LC = matrix(nrow=3883, ncol=30)
LinCom = matrix(nrow=3883, ncol=18)
for (gi in 1:18){
  #LinCom[,gi]= sapply(1:nrow(Mfactors), function(MID) sum(Mfactors[MID,]*LogOdd(pvals[[gi]]$cvhat[MID])))
  #LinCom[, gi]= 
  LC = sapply(seq(1,30), function(n) Mfactors[,n]*LogOdd(pvals[[gi]]$cvhat))
  LinCom[, gi] = sapply(1:nrow(LC), function(n) sum(LC[n,]))
}

colnames(LinCom) = genrelist

genre_scores = LinCom


#I could do... Mfactor cols * LogOdd(pvals[[]])

col.names(LinCom) = genrenames

?sum

careernames = paste("career", seq(0:20), sep="_")

dummycareer= dummy(usersdf$Occupation)
colnames(dummycareer) = careernames

#Next target: get factor scores for career (fit factors to careers?)
###-log regression(w/) vs. careers
###-CVbinary
###-Linear combination (& logodd conversion)
###-Get score data!

#Mfactors
#genrelist
careerlist = c(1:20)
lmcollect = vector("list", 21)

for (ci in 1:21){
  Mfactors[[31]] = dummycareer[[ci]]
  lmcollect[[ci]] = glm(V31~., family="binomial", data=Mfactors)
}

Mfactors= Mfactors[,-31]

pvals = vector("list", 21)
for (i in 1:21){
  pvals[[i]]= CVbinary(lmcollect[[i]])
}

#3953 as highest-numbered movie, 3883 in data

carLC = matrix(nrow=3883, ncol=30)
carLinCom = matrix(nrow=3883, ncol=21)
for (gi in 1:21){
  carLC = sapply(seq(1,30), function(n) Mfactors[,n]*LogOdd(pvals[[gi]]$cvhat))
  carLinCom[, gi] = sapply(1:nrow(LC), function(n) sum(LC[n,]))
}

career_scores = carLinCom


pairings = matrix(nrow=21, ncol=18)

dim(genre_scores)
dim(career_scores)

#first number collapses rows, second number cols.

#Career_Genres = (matrix(genre_scores) %*% t(matrix(career_scores)))*best_svd$d <- dangerously long-running code. DO NOT RUN.




#genre_scores


corrplot(pairings, is.corr=FALSE)