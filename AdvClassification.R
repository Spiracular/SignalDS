#Advanced Classification Techniques

#Start with logistic regression, follow historical move from generative to predictive models.
###Discriminant Analysis to SVMs

#Theory discussed in Notes on Classification Theory

#install.packages("klaR")
#install.packages("MASS")
#install.packages("tictoc")

library("ggplot2")
library("dplyr")
library("klaR")
library("MASS")
library("tictoc")


#I want a method to ask R what libraries it currently has active, but having trouble finding one. Moving on.

# lin_pair = function(m,b=0,label=1){
#   x = runif(1)
#   f=m*x+b
#   if (label == 1){
#     if (b>=1){
#       print("b is greater than 1; this code would run forever.")
#       return()
#     }
#     if (f>1){
#       #Could throw in another exception here, but not going to.
#       x=runif(1,0,((1-b)/m))
#       f=m*x+b
#     }
#     y = runif(1, f, 1)
#   }else if(label == -1){
#     y = runif(1, 0, min(f,1))
#   }else{
#     print("Error: label must be 1 or -1")
#     return()
#   }
#   c(x,y)
# }



# 
# #
# #y=a(x-b)^2 + c
# quad_pair = function(a,b,c,label=1){ #Warning: has the potential to run forever if you give the wrong inputs.
#   x=runif(1)
#   f=a*(x-b)^2 + c
#   print(x)#
#   print(f)#
#   if (label==1){
#     while (f>1){
#       x_max=min( (sqrt((1-c)/a) + b) , (sqrt((1-c)/a) - b) )
#       if(x_max < 0){
#         print("Unsolvble; x_max < 0")
#         return()
#       }
#       x=runif(1,0,x_max) #x values that make f less than 1
#       f=a*(x-b)^2 + c
#       print(x)#
#       print(f)#
#       if (f>1){
#         print ("I may be wrong about some things...")
#         return()
#       }
#     }
#     y=runif(1, max(f,0), 1)
#   }else if(label==-1){
#     # while(f<0){
#     #   x_min = max( (sqrt(-c/a) + b)
#     #   if (x_min>1){
#     #     print("Unsolvable; x_min > 1")
#     #     return()}
#     # x=runif(1,x_min, 1)
#     # f=a*(x-b)^2 + c
#     y=runif(1,0,min(f,1))
#     }
#   c(x,y)
# }

#Going with the solutions, because my answer is ugly as sin.

lin_pair = function(m, b, label) {
  cond = FALSE
  while (!cond) {
    x = runif(1)
    y = runif(1)
    cond = label * y > label * (m*x + b)
  }
  c(x, y)
}

quad_pair = function(a, b, c, label) {
  cond = FALSE
  while (!cond) {
    x = runif(1)
    y = runif(1)
    cond = label * y > label * (a*(x - b)^2 + c)
  }
  c(x, y)
}

#quad_pair(2, 0.5,1, label=1)#Okay, I just picked an impossible pair of numbers? Whoops?

qplot(rbind(replicate(20, quad_pair(2, 0.5,1))))

#y=a(x-b)^2 + c

#Linear Discriminant Analysis for Iris dataset (Which was apparently partially the work of R.A. Fischer?)

df_iris = iris
?iris
sapply(df_iris, class)

ggplot(df_iris, aes(Sepal.Length, Sepal.Width, color=Species)) + geom_point()
#setosa is highly distict; versicolor and virginica are hard to separate on the basis of these factors alone.

wine_df= read.csv(file="wine.data.csv")
head(wine_df)
sapply(wine_df, class) #Good, everything comes in numeric.

#aggregate(by=Type)

aggregate(.~Type, data=wine_df, FUN=sum)
#Aggregate is so much easier to use in this format. Sticking to this method in the future.

aggregate(Dilution~Type, data=wine_df, FUN = length)
#Verified the count mentioned in wine.names.txt. Types = cultivar.


###Discriminant Analysis
#Linear Discriminant Analysis (LDA) and Quadratic Discriminant Analysis (QDA)
#Both assume classes are generated from multivariate normal dists
#QDA allows the two to have different covariances, generates quadric surface separation vs. linear hyperplane



highlabel= replicate(100, lin_pair(0.75, 0.05, 1))
lowlabel = replicate(100, lin_pair(0.75, -0.1, -1))
linear_dots = as.data.frame(rbind(t(highlabel), t(lowlabel)))
linear_label = factor(c(rep(1,100), rep(-1,100)))
qplot(x=linear_dots[,1], y=linear_dots[,2], color=linear_label) #Pretty!

quadhigh= replicate(100, quad_pair(4,0.5,0.4,1))
quadlow = replicate(100, quad_pair(4,0.5,0.38,-1))
quad_dots = as.data.frame(rbind(t(quadhigh), t(quadlow)))
quad_label = factor(c(rep(1,100), rep(-1,100)))
qplot(x=quad_dots[,1], y=quad_dots[,2], color=quad_label)

?partimat #Graphs decision boundary

lda_part = partimat(linear_dots, linear_label, method="lda")
#Eugh, it doesn't maximize the margin like SVMs do. It's right up against the 1 labels!
qda_part = partimat(quad_dots, quad_label, method="qda")

#Cross-application
ql_part = partimat(linear_dots, linear_label, method="qda")
lq_part = partimat(quad_dots, quad_label, method="lda") #Indeed, quite bad

#Multiclass Classification

?lda()

linear_matrix = as.matrix(cbind(linear_dots, "Labeling" = as.numeric(as.character(linear_label))))

wine_features = select(wine_df, -Type)
wine_types = factor(wine_df$Type)


lda_wine = lda(wine_features, wine_types)
wine_pred= predict(lda_wine)

ldahist(wine_pred$x[,"LD1"],wine_types) #Group 2 is intermediate in value on axis LD1 (higher than 1 but lower than 3)...
ldahist(wine_pred$x[,"LD2"],wine_types) #But lower in value on axis LD2 than either of the others. It can be identified when you combine the two.


#LDA geometric interpretation: generative technique where p features are projected onto a k-1 dimension space
#LDA assumes all data is generated by multivariate normal, and uses that assumption when trying to minimize variance in the projection while trying
#to ensure different groups end up with broadly-separated means

wine_pred_df = as.data.frame(wine_pred$x)

#ggplot(wine_pred_df, aes(LD2, LD1))#don't know why this doesn't work...

qplot(wine_pred$x[,"LD1"], wine_pred$x[,"LD2"], color=wine_types)

pca_wine = prcomp(wine_features)
pc_wine_pred = predict(pca_wine)

head(pc_wine_pred)

qplot(pc_wine_pred[,"PC1"],pc_wine_pred[,"PC2"], color=wine_types)
#This is far less effective at separating out the wine types, as might be expected. Moderate separation of 1, but 2&3 blurr together.

#Perceptrons
###Predictive model: simply intends to separate 2 classes of data as well as possible.

#It just asked me to repeat the process for generating the linearly-separable linear_dots
qplot(x=linear_dots[,1], y=linear_dots[,2], color=linear_label)

#Adding column of 1s to allow intercept term generation

percep_mat = as.matrix(cbind(linear_dots, "int_term" = 1))

#Perceptron updates vector of weights w at each step, adjusting on misclassificaton.

dot = function(x,y){
  x %*% y
  #I'm just going to use %*% instead of dot.
}


#I strongly dislike the effects of the seed implementation as initially presented. I'll just use seed outside of the function.
perceptron = function(xs, y, w, rate){ 
  #set.seed(seed)
  pred = numeric(1)
  reindex = sample(1:nrow(xs), nrow(xs))
  re_xs = xs[reindex,]
  re_ys = y[reindex]
  for (i in 1:nrow(xs)){
    pred = re_xs[i,] %*% w
    pred = sign(pred)
    pred = sign(re_ys[i]-pred)
    w = w + pred*rate*re_xs[i,]
  }
  w
}

slow_perceptron = function(xs, y, w, rate, indices=1){
  #set.seed(seed)
  pred = numeric(1)
  reindex = sample(seq(1,nrow(xs)), indices)
  re_xs = xs[reindex,]
  re_ys = y[reindex]
  if (indices==1){
    pred = re_xs %*% w
    pred = sign(pred)
    pred = sign(re_ys-pred)
    w = w + pred*rate*re_xs
  }else {
  for (i in seq(1,indices)){
    pred = re_xs[i,] %*% w
    pred = sign(pred)
    pred = sign(re_ys[i]-pred)
    w = w + pred*rate*re_xs[i,]
  }}
  w
}

label_charge = linear_label %>% as.character %>% as.numeric

wset = perceptron(xs=percep_mat, y=label_charge, w=c(1,1,0), rate=0.1, seed=1)#-0.17, 0.97, -0.20 #on seed=10

#The actual function is: 1.5x + 0.2 = y-gen & 1, 1.5x + 0.05 = y-gen & -1

#Slope = -w[1]/w[2], intercept = w[3]/w[2]; make perpendicular

perceptron_plot = function(xs, y, w){
  qplot(x=xs[,1],y=xs[,2], color=y) + geom_abline(intercept = (w[3]/w[2]), slope=(w[1]/w[2]))
}

set.seed(6)
w=slow_perceptron(percep_mat, label_charge, w=c(0,0,0), rate=1, indices=1)
w
perceptron_plot(percep_mat, label_charge, w=w)
w=slow_perceptron(percep_mat, label_charge, w=w, rate=1, indices=1)
w
perceptron_plot(percep_mat, label_charge, w=w)

set.seed(6)
w=perceptron(percep_mat, label_charge, w=c(0,0,0), rate=1)
perceptron_plot(percep_mat, label_charge, w=w) #Reaches this in a single full run of the dataset.
w #Final converge to around -0.328, 0.480, 0
w=perceptron(percep_mat, label_charge, w=w, rate=1)



#Prediction:
#-Perceptron on 20 points will result in higher variation in answers

perceptron_conv = function(xs, y, rate, seed){
  i=1
  w2 = c(NA,NA,NA)
  w1 = perceptron(xs, y, w=c(0,0,0), rate, seed)
  percept_list = list(w1)
  while (w1 != w2){
    i = i+1
    w2 = perceptron(xs, y, w=w1, rate, seed)
    
  }
  #Oh, I need to yield a list of final weights.
  
}

tic()

toc()
