#install.packages("pROC")
#install.packages("dummies")
library("pROC")
library("dplyr")
library("readr")
library("ggplot2")
library("glmnet")
library("corrplot")

spdt = read.csv("speeddating-aggregated.csv")
FNA = complete.cases(spdt) #informs which rows have NAs
count(FNA)
spdt = na.omit(spdt)

orspdt = spdt
hobbies = spdt[9:25]
grep("yoga", names(spdt))

##HEY, so some of the below will run wrong because I was previously grabbing race and career_c in the predictions.
#I shouldn't have been. Oops.

#glm(y ~ x, family="binomial")
#And y must be either 0 & 1 or factor type.

?roc #ROC curve
#plot(roc(P()))

sapply(spdt, class)
unique(spdt$gender)

hobgen = cbind(hobbies, "gender" = spdt$gender)
binfit = glm(gender ~ . , family="binomial", hobgen)
coef(binfit)
sort(coef(binfit))

logo_to_probs = function(L){
  exp(L) / (1+exp(L))
}


binpred = logo_to_probs(predict(binfit, hobgen))
p = roc(hobgen$gender, binpred)
plot(p)



##acabis = filter(spdt, select(spdt, career_c==2 | career_c == 7))
acabis = cbind(hobbies, "career_c" = spdt$career_c)
acabis = acabis[(acabis$career_c==7 | acabis$career_c == 2),]
head(acabis)
unique(acabis$career_c)
class(acabis$career_c)

acabis$career_c = acabis$career_c==7


head(acabis)
#makes 7s into 1, 2s are 0
##7=business -> 1, 2=academia -> 0
#as.factor()
#labels(acabis$career_c)
sapply(acabis, class)

abfit = glm(career_c ~ . , family="binomial", acabis)
sort(coef(abfit))

cpred = logo_to_probs(predict(abfit, acabis))
p2 = roc(acabis$career_c, cpred)
plot(p2)


RaceCA = cbind(hobbies, "race" = spdt$race)
RaceCA = RaceCA[(RaceCA$race==2 | RaceCA$race == 4),]
RaceCA$race = RaceCA$race==4
#Asians are 1, Caucasians are 0
cafit = glm(race ~ . , family="binomial", RaceCA)
sort(coef(cafit))

rpred = logo_to_probs(predict(cafit, RaceCA))
p3 = roc(RaceCA$race, rpred)
plot(p3)


?select
?filter
?glm

#count(spdt$career_c==7 | spdt$career_c == 2)

###Regularized Linear Regression###

#Using L^p regularization wtih logistic regression cost function.


#strsplit
###Got to speedDatingDecisionStarter!
###Go back here for "Analyzing the speed dating dataset!" (the other doesn't have hobbies)

careers = c("Lawyer", 
            "Academic", 
            "Psychologist", 
            "Doctor", 
            "Engineer",
            "Creative",
            "Business",
            "RealEstate",
            "IntRelations",
            "Undecided",
            "SocialWork",
            "Speech",
            "Politics",
            "Athletics",
            "Other",
            "Journalism",
            "Architecture")

sapply(spdt, class)

#NEVERMIND GOD.

#Making careers into names over here
spdt$career_c = factor(spdt$career_c, labels = careers)
head(spdt)

Top4careers = names(sort(table(spdt$career_c), decreasing = TRUE))[1:4]
Top4careers
df4c = spdt[spdt$career_c %in% Top4careers,]
df4c$career_c = factor(df4c$career_c)
head(df4c)
###NOTE: DO NOT USE as.factor. Use factor. It's just better in the vast majority of circumstances.
##(In this case, as.factor manages to retain factor levels that are NO LONGER IN THE DATASET PROVIDED TO IT.)
##((It's super weird!))

levels(df4c$career_c)
sapply(df4c, class)
length(df4c)

allvars = df4c[c(2, 8, 9:25)]
nrow(allvars)
allvars = allvars[complete.cases(allvars),]
nrow(allvars)
head(allvars)

xdf = allvars[-2]
ydf = allvars[[2]]
ydf[1:5]
class(ydf)

nrow(xdf)

sapply(allvars, class)
head(xdf)

#Okay, back to working here.

###Multinomial Logistic Regression###

#glmnet(x,y,family="multinomial")
##x as scaled matrix
##y as categorical variable (factors?)
#We will be doing a weird thing and using *UNREGULARIZED* logistic regression here.
#set s to 0 when doing predict

careerMLG = glmnet(x=as.matrix(xdf), y = ydf, family="multinomial")
#Reminder: if you're not scaling, do something else to make it into a matrix

names(careerMLG)

str(coef(careerMLG)[["Creative"]])
###

coefs = coef(careerMLG, s=0)
coefs_final = as.matrix(do.call(cbind, coefs))
colnames(coefs_final) = c("Lawyer", "Academic", "Creative", "Business")
coefs_final  =coefs_final[-1,]
corrplot(coefs_final, is.corr=FALSE)


#grabnames=names(spdt[9:25])

predicty = predict(careerMLG, as.matrix(xdf), s=0)
dim(predicty) = c(389,4)
dim(predicty)
head(predicty)
class(predicty)
nrow(predicty)
ncol(predicty)

# length(ydf)
# nrow(predicty)
# dim(predicty)
# head(predicty)
# qplot(as.numeric(ydf), predicty[,4,1])

probabilities = function(preds, rownum){
  #factnum = ncol(preds)
  denom =sum(exp(preds[rownum,]))
  out = exp(preds[rownum, ]) / denom
  out
}
dim(predicty)

sum(probabilities(preds=predicty, rownum=9))
#Yep!

corrplot()