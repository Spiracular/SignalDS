#Resampling

#Basic Resampling Methods
#n-fold cross-validation vs. overfitting, bootstrapping to quantify uncertainty
getwd()
setwd("/Users/Spirapple/Desktop/Projects/PythonDataSci/SignalDS/")
library("dplyr")

spdt = read.csv("speed-dating-simple.csv")
#A note: female = 0

head(spdt)

linearattr = lm(attr_o ~ ., spdt)
sort(coef(linearattr))

set.seed(5)

#maledate = spdt[spdt$gender == 1]

maledate = filter(spdt, gender==1)
head(maledate)
sapply(maledate,class)

omissions = c(names(maledate)[1:6])
allmaledate = maledate
maledate = omismale

linmale = lm(attr_o ~ ., maledate)
head(omismale)
linmaleom  = lm(attr_o ~ ., omismale)
sort(coef(linmaleom))
coef(linmale)[order(abs(coef(linmale)), decreasing = TRUE)]
sort(coef(linmale))

split_data_b = function(df){
  size = as.integer((nrow(df)/2))
  rownum = c(1:nrow(df))
  ind = sample(rownum,size=size, replace=FALSE)
  train = df[ind,]
  test = df[-ind,]

  list(train=train, test=test)
}

split_data = function(df) {
  groups = sample(1:nrow(df)) %% 2
  train = df[groups == 1,]
  test = df[groups == 0,]
  split_list = list("train"=train, "test"=test)
}

trates = split_data(maledate)
trates$train


omissions = c(names(maledate)[1:6])

attrmodel = lm(attr_o ~ . - c(names(maledate)[3:6]), maledate)
typeof(3:6)
typeof(c(3:6))


split_predict = function(train, test){
  attrmodel = lm(attr_o ~ ., train)
  prediction0 = predict(attrmodel, train)
  prediction1 = predict(attrmodel, test)
  
  list(train=prediction0, test=prediction1)
}

predicts = split_predict(trates$train, trates$test)
mean(predicts$train)
mean(predicts$test)

mean(maledate$attr_o)

nrow(trates[[1]])
nrow(trates[[2]])

#root mean square error
rmse = function(x_hat, x){
  (x_hat - x)^2 %>%
    mean() %>%
    sqrt()
    
}

read_rmse = function(predicts, splits){
  # x_hat_train = predicts$train
  # x_train = splits$train[["attr_o"]]
  
  x_hat_test = predicts$test
  x_test = splits$test[["attr_o"]]
  
  # list(rmse(x_hat_train, x_train), rmse(x_hat_test, x_test))
  rmse(x_hat_test, x_test)
}


rmsetrain = rmse(x_hat = predicts$train, x= trates$train[["attr_o"]])
rmsetest = rmse(x_hat = predicts$test, x= trates$test[["attr_o"]])

rmsetrain
rmsetest

rmsetest/sd(maledate$attr_o)



#x_hat = predicts$train, x= trates$train[["attr_o"]]

typeof(trates$train[["attr_o"]])

?sample

rep(maledate$attr_o, )

rsplitrmse = function(n, df){
  vectrain = numeric(n)
  vectest = numeric(n)
  for (i in 1:n){
    split = split_data(df)
    predic = split_predict(split$train, split$test)
    rmse0 = rmse(x_hat = predic$train, x= split$train[["attr_o"]])
    rmse1 = rmse(x_hat = predic$test, x= split$test[["attr_o"]])
    vectrain[i] = rmse0
    vectest[i] = rmse1
  }
  
  data.frame(train = vectrain, test=vectest)
}


omismale = maledate[-c(1, 3:6)]
aggrsme = rsplitrmse(100, df=omismale)
head(aggrsme)
ggplot(data=aggrsme) + geom_histogram(aes(train), fill='red', alpha=0.4) + geom_histogram(aes(test), fill='blue', alpha=0.4) + xlab("rmse")
ggplot(data=aggrsme, aes(test)) + geom_histogram()
?ggplot


sd(aggrsme$train)
sd(aggrsme$test)
mean(aggrsme$train)
mean(aggrsme$test)

#  partsize = as.integer((nrow(df)/n_folds))
# 
# nfold_cv = function(df, n_folds = 5){
#   resample = sample(1:nrow(df)) %% n_folds
#   create_fold = function(i) {
#     df[resample == i,]
#     }
#   all_folds = lapply(0:(n_folds-1), function(i) create_fold(i))
#   partsize = as.integer((nrow(df)/n_folds))
#   
#   predict_x = c()
#   real_x = c()
#   
#   for (j in 1:n_folds){
#     exclude = all_folds[j]
#     train_folds = all_folds[-j]
#     print(str(train_folds))
# #    train_data = (rbind(unlist(train_folds, recursive = FALSE)))
#     for (x in train_folds){
#       train_data = rbind(train_data,train_folds)
#     }
#     print(str(train_data))
#     print(class(train_data))
#     print(train_data)
#     # Reduce(function(x, y) merge(x, y, all=TRUE), train)
#     
#     model = lm(attr_o ~ ., train_data)
# 
#     predict_x = c(predict_x, predict(model, exclude))
#     real_x = c(real_x, exclude$attr_o)
#   }
#   rmse(x_hat = predict_x, x = real_x)
# }


# n-fold cross validation
nfold_cv = function(df, n_folds) {
  # Make empty predictions vector
  preds = numeric(nrow(df))
  
  # Calculate the different folds
  folds = sample(nrow(df)) %% n_folds + 1
  
  for (i in 1:n_folds) {
    # Get subsets of the data
    df_train = df[folds != i, ]
    df_test = df[folds == i, ]
    
    # Fit linear model to training set
    fit = lm(attr_o ~ ., df_train)
    
    # Make predictions for test set
    preds[folds == i] = predict(fit, df_test)
  }
  
  # Return RMSE
  sqrt(mean((preds - df$attr_o)^2))
}



nfold_cv(maledate, 5)

# list(mtcars, mtcars, mtcars)
# exclude = all_folds[j]
# train = all_folds[-j]
# train = Reduce(function(x, y) merge(x, y, all=TRUE), train)


fold2 = sapply(1:100, function(i) nfold_cv(maledate, 2))
fold10 = sapply(1:100, function(i) nfold_cv(maledate, 10))

nfold_test = data.frame(two = fold2, ten = fold10)

ggplot(data=nfold_test) + geom_histogram(aes(fold2), fill='red', alpha=0.4) + geom_histogram(aes(fold10), fill='blue', alpha=0.4) + xlab("rmse")

mean(fold2)
sd(fold2)
mean(fold10)
sd(fold10)


backward_step = function(df) {
  n_removed = c()
  rmse_cv = c()
  rmse_nocv = c()
  n = 0
  
  while (length(df) >= 2) {
    n_removed = c(n_removed, n)
    
    rmse_cv = c(rmse_cv, nfold_cv(df, 10))
    
    model = lm(attr_o ~ ., df)
    predictions = predict(model, df)
    rmse_nocv = c(rmse_nocv, rmse(predictions, df$attr_o))
    
    # remove cofficient with the highest p-value
    coefs = as.data.frame(summary(model)$coefficients[-1, , drop = FALSE])
    ordered_coefs = coefs[order(coefs[4], decreasing = TRUE), ]
    ordered_coefs[[1]]
    to_remove = rownames(ordered_coefs)[1]
    df = select(df, -one_of(to_remove))
    n = n + 1
  }
  data.frame(var.removed = n_removed, rmse_cv = rmse_cv, rmse_nocv = rmse_nocv)
}

backward_data = backward_step(maledate)

ggplot(data = backward_data, aes(x=var.removed)) + geom_point(aes(y = rmse_nocv), color = "red") + geom_point(aes(y = rmse_cv), color = "blue") + ylab("rmse")

head(allmaledate)
o_collect = allmaledate[2:6]
head(o_collect)
hobbies = allmaledate[7:23]
length(allmaledate)


df = cbind(o_collect[1], hobbies)
model_init = lm(attr_o ~ ., df)
model = formula(lm(attr_o ~ ., df))
step_reg = step(model_init, model, direction = "backward")


df = cbind(o_collect[2], hobbies)
model_init = lm(sinc_o ~ ., df)
model = formula(lm(sinc_o ~ ., df))
step_reg = step(model_init, model, direction = "backward")

df = cbind(o_collect[3], hobbies)
model_init = lm(intel_o ~ ., df)
model = formula(lm(intel_o ~ ., df))
step_reg = step(model_init, model, direction = "backward")

df = cbind(o_collect[4], hobbies)
model_init = lm(fun_o ~ ., df)
model = formula(lm(fun_o ~ ., df))
step_reg = step(model_init, model, direction = "backward")


coef(model_init)[order(abs(coef(model_init)), decreasing = TRUE)]


df = cbind(o_collect[5], hobbies)
model_init = lm(amb_o ~ ., df)
model = formula(lm(amb_o ~ ., df))
step_reg = step(model_init, model, direction = "backward")

coef(model_init)[order(abs(coef(model_init)), decreasing = TRUE)]

###Bootstrapping
#Training model on bootstrap to predict original will result in seriously underestimated RMSE

is.integer(1)

bootstrap_bad = function(df, approach = 1, n_times=100){
  
  rmsehold = numeric(n_times)
  rownum = c(1:nrow(df))
  size = nrow(df)
  
  if (!is.numeric(approach)){
    print ("Approach must be the integer 1 or 2")
    return()

  }else if (approach==1){
    
    for (i in 1:n_times){
      ind = sample(rownum, size=size, replace=TRUE)
      
      train = df[ind,]
      test = df
      
      lmodel = lm(attr_o ~ ., train)
      predict_attr = predict(lmodel, test)
      rmsehold[i]= rmse(predict_attr, test$attr_o)
    }
    
  } else if (approach==2){
    
      for (i in 1:n_times){
        ind = sample(rownum, size=size, replace=TRUE)
        
        train = df
        test = df[ind,]
        
        lmodel = lm(attr_o ~ ., train)
        predict_attr = predict(lmodel, test)
        rmsehold[i]= rmse(predict_attr, test$attr_o)
      }

  } else {
    print ("Error: approach must be the integer 1 or 2")
    return()
  
  }
  
  mean(rmsehold)
  
}

approach1 = bootstrap_bad(maledate, approach=1)
approach2 = bootstrap_bad(maledate, approach=2)

mean(approach1)
mean(approach2)


backward_step_2 = function(df) {
  n_removed = c()
  rmse_cv = c()
  rmse_nocv = c()
  boot1 = c()
  boot2 = c()
  n = 0
  
  while (length(df) >= 2) {
    n_removed = c(n_removed, n)
    
    boot1 = c(boot1, bootstrap_bad(df, approach=1, n_times=100))
    boot2 = c(boot2, bootstrap_bad(df, approach=2, n_times=100))
    
    rmse_cv = c(rmse_cv, nfold_cv(df, 10))
    
    model = lm(attr_o ~ ., df)
    predictions = predict(model, df)
    rmse_nocv = c(rmse_nocv, rmse(predictions, df$attr_o))
    
    # remove cofficient with the highest p-value
    coefs = as.data.frame(summary(model)$coefficients[-1, , drop = FALSE])
    ordered_coefs = coefs[order(coefs[4], decreasing = TRUE), ]
    ordered_coefs[[1]]
    to_remove = rownames(ordered_coefs)[1]
    df = select(df, -one_of(to_remove))
    n = n + 1
  }
  data.frame(var.removed = n_removed, rmse_cv = rmse_cv, rmse_nocv = rmse_nocv, boot1 = boot1, boot2 = boot2)
}

backstoreboot = backward_step_2(maledate)

#Steal from this graph in the future, it is good.
ggplot(data = backstoreboot, aes(x=var.removed), color=variable) + ylab("rmse") +
  geom_line(aes(y=rmse_nocv, col="nocv")) + 
  geom_line(aes(y=rmse_cv, col="cv")) +
  geom_line(aes(y=boot1, col="boot1")) +
  geom_line(aes(y=boot2, col="boot2")) 


bootstrap_good = function(df, n_times=100){
  
  rmsehold = numeric(n_times)
  rownum = c(1:nrow(df))
  size = nrow(df)
  
  for (i in 1:n_times){
    ind = sample(rownum, size=size, replace=TRUE)
    
    train = df[ind,]
    test = df[-ind,]
    
    lmodel = lm(attr_o ~ ., train)
    predict_attr = predict(lmodel, test)
    rmsehold[i]= rmse(predict_attr, test$attr_o)
  }

  mean(rmsehold)
  
}


backward_step_3 = function(df) {
  n_removed = c()
  rmse_cv = c()
  rmse_nocv = c()
  boot = c()
  n = 0
  
  while (length(df) >= 2) {
    n_removed = c(n_removed, n)
    
    boot = c(boot, bootstrap_good(df, n_times=100))
    
    rmse_cv = c(rmse_cv, nfold_cv(df, 10))
    
    model = lm(attr_o ~ ., df)
    predictions = predict(model, df)
    rmse_nocv = c(rmse_nocv, rmse(predictions, df$attr_o))
    
    # remove cofficient with the highest p-value
    coefs = as.data.frame(summary(model)$coefficients[-1, , drop = FALSE])
    ordered_coefs = coefs[order(coefs[4], decreasing = TRUE), ]
    ordered_coefs[[1]]
    to_remove = rownames(ordered_coefs)[1]
    df = select(df, -one_of(to_remove))
    n = n + 1
  }
  data.frame(var.removed = n_removed, rmse_cv = rmse_cv, rmse_nocv = rmse_nocv, boot = boot)
}

goodboot = backward_step_3(maledate)

ggplot(data = goodboot, aes(x=var.removed), color=variable) + ylab("rmse") +
  geom_line(aes(y=rmse_nocv, col="nocv")) + 
  geom_line(aes(y=rmse_cv, col="cv")) +
  geom_line(aes(y=boot, col="boot"))

#Goodboot (use of out-of-bag data points for test values) overestimates the RMSE

#Bootstrapping is great for estimating variance of parameter estimates.

calc_alpha = function(X,Y) {var(Y)/(var(X) + var(Y)) }

bootstrap_sample = function(X, Y, n_times=1000) {
  alphahold = numeric(n_times)
  length = length(X)
  
  for (i in 1:n_times){
    ind = sample(1:length, size=length, replace=TRUE)
    
    bootstrap_x = X[ind]
    bootstrap_y = Y[ind]
    
    alphahold[i]= calc_alpha(bootstrap_x, bootstrap_y)
  }
  alphahold
}


gen_alphas = function(sdX, sdY, n=100){
  X = rnorm(n, mean=10, sd=sdX)
  Y = rnorm(n, mean=10, sd=sdY)
  
  bootstrap_sample(X, Y)
}


one_three = gen_alphas(1,3)
three_one = gen_alphas(3,1)
one_one = gen_alphas(1,1)

mean(one_three)
sd(one_three)
mean(three_one)
sd(three_one)
mean(one_one)
sd(one_one)


one_two = gen_alphas(1,2)
one_three = gen_alphas(1,3)
one_four = gen_alphas(1,4)

alpha_estimates = data.frame(two = one_two, three = one_three, four = one_four)
head(alpha_estimates)

ggplot(data = alpha_estimates, aes()) + xlab("alpha_estimates") +
  geom_histogram(aes(two), fill = "blue", alpha=0.3, show.legend=TRUE) +
  geom_histogram(aes(three), fill = "green", alpha=0.3, show.legend=TRUE) +
  geom_histogram(aes(four), fill = "red", alpha=0.3, show.legend = TRUE)

?aes
str(alpha_estimates)
