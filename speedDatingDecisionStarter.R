library(dplyr)
library(glmnet)
library(pROC)
library(dummies)
df = read.csv("speeddating-full.csv")

#Create data frame with decisions, average decision frequencies, careers and races
df = select(df, gender, iid, pid, wave, dec, attr, race, career_c)
genders = c("female", "male")
df$gender = factor(df$gender, labels = genders)
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
races = c("Black", "White", "Latino", "Asian", "Other")
# df$gender = factor(df$gender, labels = genders)
# df$race = factor(df$race, labels = races)
# df$career_c = factor(df$career_c, labels = careers)
agged = aggregate(df["dec"], df["iid"], FUN = mean, na.rm = T)

colnames(agged) = c("iid", "decAvg")
df = inner_join(df, agged)
agged = aggregate(df[c("dec", "attr")], df["pid"], FUN = mean, na.rm = T)
colnames(agged) = c("pid", "decPartnerAvg", "attrPartnerAvg")
df = inner_join(df, agged)
agged = aggregate(df[c("race", "career_c")], df["iid"], FUN = mean)
agged$race = factor(agged$race, labels = races)
agged$career_c = factor(agged$career_c, labels = careers)
names(agged)
df = inner_join(df[!(names(df) %in% c("race", "career_c"))], agged)
colnames(agged)[1:3] = c("pid", "race_Partner", "career_c_Partner")
df = inner_join(df, agged)



#Cross validate regularized logistic regression at the level of waves

crossValidate = function(features, 
                         target, 
                         waves = df$wave,
                         lambdas = (1.2)^(10:(-30)), 
                         alphas = seq(0, 0.24, 0.03)){
  s = scale(features)
  s = s[,!is.nan(colSums(s))]
  rocs = expand.grid(lambda = lambdas, alpha = alphas)
  rocs$logLoss = 0
  rocs$ROC = 0 
  for(alpha in alphas){
    print(alpha)
    l = lapply(1:21, function(wave){
      trainFeatures = s[waves != wave,]
      testFeatures = s[waves == wave,]
      set.seed(1); m = glmnet(trainFeatures, target[waves != wave], 
                              alpha = alpha, 
                              lambda = lambdas,
                              family = "binomial")
      as.data.frame(predict(m, testFeatures))
    })
    predictions = do.call(rbind, l)
    predictions = exp(predictions/(1 + predictions))
    rocTemp = sapply(predictions, function(cv){
      as.numeric(roc(target,cv)$auc)
    })
    rocs[rocs$alpha == alpha,"ROC"] = rocTemp[length(rocTemp):1]
  }
  rocs
}

####END STARTER####

sapply(df, class)

unique(df$dec)

dums1= dummy.data.frame(df[c("race","career_c")], names=c("race", "career_c"))
head(dums1)
dums2= dummy.data.frame(df[c("race_Partner","career_c_Partner")], names=c("race_Partner", "career_c_Partner"))

dums = cbind(dums1, dums2)

#Okay, so I suspect what it wants is for me to grab all "race*" from dum1  and multiply them by attrPartnerAvg.
#
# #Okay, how do I make the column names a hybrid of the names of the variables inputted to make them?
# interacts = cbind(n1, n2, n3, n4, paste())

#Interaction Terms
n1 = dums1[grep("race*", names(dums1))]*df$attrPartnerAvg
names(n1) = paste(names(dums1)[grep("race*", names(dums1))], "attrPartnerAvg", sep=":")
n2 = dums1[grep("career_c*", names(dums1))]*df$attrPartnerAvg
names(n2) = paste(names(dums1)[grep("career_c*", names(dums1))], "attrPartnerAvg", sep=":")

#Problem: it's automatically only checking matching race, not doing cross-race multiplications

grep("race*", names(dums1))

#n3 = dums1[grep("race*", names(dums1))]*dums2[grep("race*", names(dums2))]
# for (x in dums1[grep("race*", names(dums1))]){
#   n3 = c(n3, x * dums2[grep("race*", names(dums2))])
#   }



###Pseudocode for below###
# for column in n3
#   for row in n3
#     name=paste(rowname, columnname)
#     cbind(n3[row, column])

###More Interaction Terms: Crossing the dums
n3= sapply(dums1[grep("race*", names(dums1))], function(x) x*dums2[grep("race*", names(dums2))])
###Yields matrix w/ vectors 
head(n3)
dim(n3)
str(n3)


finaln3 = matrix(nrow = 8368, ncol = 36)
n3names = vector("character", 36)
for (c in 1:ncol(n3)){
  for (r in 1:nrow(n3)){
    name = paste(colnames(n3)[c], rownames(n3)[r], sep=":")
    #print(name)
    colnum = (c-1)*nrow(n3) + r
    n3names[[colnum]] = name
    finaln3[,colnum] = n3[[r,c]]
  }
}
colnames(finaln3)  = n3names

head(finaln3)


#names(n3) = sapply(function(x) paste(x, ))
#  paste(names(dums1)[grep("race*", names(dums1))], names(dums2)[grep("race*", names(dums2))], sep=":")
#n4 = dums1[grep("career_c*", names(dums1))]*dums2[grep("career_c*", names(dums2))]
#names(n4) = paste(names(dums1)[grep("career_c*", names(dums1))], names(dums2)[grep("career_c*", names(dums2))], sep=":")

n4= sapply(dums1[grep("career_c*", names(dums1))], function(x) x*dums2[grep("career_c*", names(dums2))])
###Yields matrix w/ vectors 
dim(n4)


finaln4 = matrix(nrow = 8368, ncol = 324)
n4names = vector("character", 324)
for (c in 1:ncol(n4)){
  for (r in 1:nrow(n4)){
    name = paste(colnames(n4)[c], rownames(n4)[r], sep=":")
    #print(name)
    colnum = (c-1)*nrow(n4) + r
    n4names[[colnum]] = name
    finaln4[,colnum] = n4[[r,c]]
  }
}
colnames(finaln4)  = n4names

finaln4[1,]

finaldums = cbind(dums1, dums2, n1, n2, finaln3, finaln4)
finaldums = finaldums[,colSums(finaldums)>20]
names(finaldums)
#164 names left after exclusion

features = cbind("decAvg" = df$decAvg, "decPartnerAvg" = df$decPartnerAvg, "attrPartnerAvg" = df$attrPartnerAvg ,finaldums)

names(features[1:10])


#STARTING CROSSVALIDATION (finally)#
cv= crossValidate(features = features,target = df$dec)

maldex = df$gender=="male"
femdex = df$gender=="female"
mfeatures = features[maldex,]
ffeatures = features[femdex,]
mtarget = df$dec[maldex]
ftarget = df$dec[femdex]

mwave = df$wave[maldex]
fwave = df$wave[femdex]

cvm= crossValidate(features = mfeatures,target = mtarget, wave=mwave)
cvf= crossValidate(features = ffeatures,target = ftarget, wave=fwave)

#plot(cvm) is really pretty, even if it is relatively nonsensical (lambda vs. alpha vs. logloss vs. ROC, all combos)
which.min(cvm[["ROC"]])
min(cvm[["ROC"]])
cvm[99,]

which.min(cvf[["ROC"]])
min(cvf[["ROC"]])
cvf[66,]


#Note: ROC represents area under the ROC curve

###Multinomial Logistic Regression###
#glmnet(x,y,family="multinomial")
##x as scaled matrix
##y as categorical variable (factors?)
#We will be doing a weird thing and using *UNREGULARIZED* logistic regression here.
#set s to 0 when doing predict

###SWITCHING BACK TO LogisticRegression.R

Top4careers = names(sort(table(df$career_c), decreasing = TRUE))[1:4]
Top4careers
df4c = df[df$career_c %in% Top4careers,]
df4c$career_c = factor(df4c$career_c)
###NOTE: DO NOT USE as.factor. Use factor. It's just better in the vast majority of circumstances.
##(In this case, as.factor manages to retain factor levels that are NO LONGER IN THE DATASET PROVIDED TO IT.)
##((It's super weird!))

levels(df4c$career_c)
sapply(df4c, class)

# names(df)
# 
# dftemp = read.csv("speeddating-full.csv")
# dftemp  = dftemp[grabnames]
# df = cbind(df, dftemp)
# ##I did an ugly thing and ran this in an order where this got inserted BEFORE the na.rm=T activated. :P
# 
# head(df)
