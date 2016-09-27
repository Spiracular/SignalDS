#Intro to Linear Regression - Sept. 12

### UN INFANT MORTALITY DATA ###

# Write code here to load packages and data
#packset = c("car", "Ecdat", "HistData", "ggplot2", "dplyr", "Rmisc", "GGally")
#for (name in packset){
#  install.packages(name)
#}

library("car")
library("ggplot2")
library("GGally")

df=UN

head(UN)
View(UN)
df["gdp"]
# Write code here to calculate correlations
correlation = cor(df["infant.mortality"], df["gdp"], use="complete.obs")

roundcor <- function(cormat){
  return (round(cormat*100))
}
# Write code here to make a new dataframe with incomplete rows omitted
UNdf = df[complete.cases(df),]
df2 = UNdf
# Write code here to examine the distribution of the data
ggpairs(UNdf)
#df["infant.mortality"], df["gdp"])
# Write code here to take the log transformation of the data
ldf = log(UNdf)
# Write code here to examine the distribution of the log-transformed data
ggpairs(ldf)

# Calculate linear fit of infant mortality vs. GDP
linear_fit = lm(infant.mortality ~ gdp, df)

# Calculate linear fit of log(infant mortality) vs. log(GDP)
loglog_fit = lm(infant.mortality ~ gdp, ldf)

#summary(linear_fit)

# Plot the linear fit of infant mortality vs. GDP
ggplot(df2, aes(gdp, infant.mortality)) + geom_point() + geom_smooth(method = "lm")
#Without modification, geom_smooth method is auto to "loess" b/c dataset n<1000. >1000 defaults to gam, which looks similar to lm.

#Plotting linear fit of logs (log-linear?)
ggplot(ldf, aes(gdp, infant.mortality)) + geom_point() + geom_smooth(method = "lm")

# Plot of linear fit residuals
qplot(df2$gdp, linear_fit$residuals)
#Heck yes there's heteroskedasticity! The residuals vary a lot by the value of the gdp, with low gdp showing really variable residuals.

# Plot of linear fit residuals after log transformation of GDP and infant mortality
qplot(df2$gdp, df2$infant.mortality - exp(fitted(loglog_fit)))
#The log transform is an improved fit; the residuals center around 0 now.
#(Before, residuals had a consistent predictable skew direction dependeding on what the gdp was. log-linear is a far better fit than linear was.)
#However, it looks like there's still heteroskedasticity; variability around 0 GDP is still higher than elsewhere.

#Find and get adjusted r squared
names(summary(linear_fit))

summary(linear_fit)
linear_fit[[1]][[1]]
summary(linear_fit)[9]
summary(linear_fit)$adj.r.squared
