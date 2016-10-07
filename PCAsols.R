library("ggplot2")
library("dplyr")
library("psych")

df = msq
df = select(df,Extraversion, Neuroticism, active:scornful)
#That up there is a nice way to subset things. I will do it that way in the future.

sort(colSums(is.na(df)))

df = df[,colSums(is.na(df))<=500]
df = df[rowSums(is.na(df))==0,]

#This function prints out top 10 loadings of nth PCAs, by absolute value
top = function(n,p, num=10){
  v= p$rotation[,n]
  v[order(abs(v), decreasing = TRUE)][1:num]
}

features = select(df, -Extraversion, -Neuroticism)
p = prcomp(features, scale=TRUE)
?prcomp #Principal Components Analysis, from stats package

#Plotting PCA loadings for first 5 to 10 PCAs

loadings = p$rotation[,1:5]
corrplot(loadings, is.corr=FALSE)

#PC1: energetic happiness
#PC2: inverse unhappiness
#PC3: disquiet/unsettled (reversed serenity)
#PC4: I'm not tired
#PC5: "I'm not angry!" D:<

top(1,p)
top(2,p)
top(3,p)
top(4,p)
top(5,p)
top(6,p, num=20) #Internal nervousness? Still but scared?

p$sdev
#First 3 clearly important. Drop off by 7 on importance? Stronger elbow at 10?

qplot(,p$sdev)

n = ncol(p$rotation)
rmses_extra = numeric(n)
rmses_neuro = numeric(n)

rmse = function(x,y) sqrt(mean((x-y)^2))

testDF = select(df, Extraversion:Neuroticism)

for (i in seq(n)){
  #Iterate through ncols
  testDF =cbind(testDF, p$x[,i])
  colnames(testDF)[ncol(testDF)] = paste0("PC",i)
  
  #Run an lm w/ PCA
  sumss = 0 #just to avoid error in cv.lm
  
}

#NOTE TO SELF: USE paste0 if you don't want separators!