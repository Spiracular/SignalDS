df = read.csv("speed-dating-simple.csv")
names(df)
# Select only self-assessments and gender
df_attr = select(df, -intel_o, -amb_o, -fun_o, -sinc_o)
names(df_attr)
# Select males only
df_attr = filter(df_attr, gender==1)
# Select everything but gender
df_attr = select(df_attr, -gender)
df_attr
linear_extra = lm(formula = attr_o ~ ., data = df_attr)
summary(linear_extra)
coef(linear_extra)

split_data = function(df) {
  groups = sample(1:nrow(df_attr)) %% 2
  train = df[groups == 1,]
  test = df[groups == 0,]
  split_list = list("train"=train, "test"=test)
}

df_split = split_data(df_attr)
df_split
names(df_split)
df_split$train
typeof(df_split$test)

split_predict = function(train, test) {
  model = lm(formula = attr_o ~ ., data = train)
  train_predicted = predict(model,train)
  test_predicted = predict(model,test)
  pred_list = list("train"=train_predicted, "test"=test_predicted)
}

predictions = split_predict(df_split$train, df_split$test)
predictions

rmse = function(x,y) {
  (x - y)^2 %>%
    mean() %>%
    sqrt()
}

rmse2 = function(x,y) {
  sqrt(mean((x - y)^2))
}

rmse_train = rmse(df_split$train, predictions$train)
rmse_train

rmse_test = rmse(df_split$test, predictions$test)
rmse_test

nIter = 100
rmse_train = rep(0,nIter)
rmse_test = rmse_train
for (i in 1:nIter) {
  df_split = split_data(df_attr)
  predictions = split_predict(df_split$train, df_split$test)
  rmse_train[i] = rmse(df_split$train, predictions$train)
  rmse_test[i] = rmse(df_split$test, predictions$test)
}

ggplot() + geom_histogram(mapping = aes(rmse_train),color = "blue") + geom_histogram(mapping = aes(rmse_test),color = "green")
