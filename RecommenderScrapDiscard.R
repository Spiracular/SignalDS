

######THIS IS WHERE THINGS ARE BREAKING (I need to fix dummycareer to cooperate; dc is 6040 rows where it needs to be 3883)
for (ci in 1:21){
  #Mfactors["V31"] = dummycareer[,ci]#Okay, yeah, this thing is bad and foolish. I'll try to reread.
  
  lmcollect[[ci]] = glm(V31~., family="binomial", data=Mfactors)
  print(head(Mfactors))
}

Mfactors= Mfactors[,-31]

pvals = vector("list", 21)
for (i in 1:21){
  pvals[[i]]= CVbinary(lmcollect[[i]])
}

#3953 as highest-numbered movie, 3883 in data

# carLC = matrix(nrow=3883, ncol=30)
# carLinCom = matrix(nrow=3883, ncol=21)
# for (gi in 1:21){
#   carLC = sapply(seq(1,30), function(n) Mfactors[,n]*LogOdd(pvals[[gi]]$cvhat))
#   carLinCom[, gi] = sapply(1:nrow(LC), function(n) sum(LC[n,]))
# }

#cLC = matrix(nrow=3883, ncol=30)
carLinCom = matrix(nrow=30, ncol=21)
for (gi in 1:21){
  carLinCom[,gi] = sapply(seq(1,30), function(n) sum(Mfactors[,n]*LogOdd(pvals[[gi]]$cvhat)))
  #carLinCom[, gi] = sapply(1:30, function(n) sum(LC[,n]))
}


career_scores = carLinCom




dim(genre_scores)
dim(career_scores)
#Same result on every row; clearly screwed up somewhere.


pairings = matrix(nrow=21, ncol=18)

careernames = c("other","academic/educator","artist","clerical/admin","college/grad student","customer service",
                "doctor/health care","executive/managerial","farmer","homemaker","K-12 student","lawyer","programmer",
                "retired","sales/marketing","scientist","self-employed","technician/engineer","tradesman/craftsman","unemployed","writer")


pairings = t(as.matrix(career_scores)) %*% diag(best_svd$d) %*% as.matrix(genre_scores)
rownames(pairings) = careernames

#first number collapses rows, second number cols.

#DANGER: DO NOT RUN: Career_Genres = (matrix(genre_scores) %*% t(matrix(career_scores))*best_svd$d


#genre_scores


corrplot(pairings, is.corr=FALSE)
#...no. Nope nope nope nope. Looking at the answers.



##FRom somethign else
for (i in 1:ncol(factscale)){
  scores[i] = sum(TwinTrain[ri, ]*factscale[,i])
}
scores

#install.packages("fasttime")
#library("fasttime")
#?fastPOSIXct

?POSIXct
#Make sure to get hour; it has a major effect on the output.
kbtrain$Hour = hour(fastPOSIXct(kbtrain$datetime, "GMT"))
kbtrain= mutate(kbtrain, hour=hour(datetime))#AUGH I DON'T KNOW WHY THIS DOESN'T WORK
head(hour(kbtrain$datetime))

kbtrain["Hour"] = hour(kbtrain$datetime)

hour(kbtrain$datetime[[1]])
class(kbtrain$datetime)
