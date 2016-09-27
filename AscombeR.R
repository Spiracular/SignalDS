library("car")
library("ggplot2")
library("GGally")
library("dplyr")

df = read.csv(file = "ascombe.csv")

summary.data.frame(df)

names= colnames(df)
for (i in 1:8){
  vals = df[[i]]
  print(names[i])
  print (paste("Row", i))
  print (paste("Mean:", mean(vals)))
  print (paste("Var: ", var(vals)))
}

xdf = df[1:4]
ydf = df[5:8]

cor(xdf, ydf)

#Not needed, but figured I'd do it anyway.
lm_fit = lm(y1 ~ x1+x2+x3+x4, df)
summary(lm_fit)

lm_fit1 = lm(y1~x1, df)
summary(lm_fit1)
coefficients(lm_fit1)
plot(df$x1, df$y1)
ggplot(df, aes(x1, y1)) + geom_point() + geom_smooth(method = "lm")

# # GET EQUATION AND R-SQUARED AS STRING
# # SOURCE: http://goo.gl/K4yh
# 
# lm_eqn <- function(x, y, df){
#   m <- lm(y ~ x, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
#                    list(a = format(coef(m)[1], digits = 2),
#                         b = format(coef(m)[2], digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));
# }
# p = ggplot(df, aes(x1, y1)) + geom_point() + geom_smooth(method = "lm")
# p1 <- p + geom_text(x = 25, y = 300, label = lm_eqn(x1, y1, df), parse = TRUE)
# # Okay, this isn't working. I'll try again later, maybe.

lm_fit2 = lm(y2~x2, df)
summary(lm_fit2)
coefficients(lm_fit2)
plot(df$x2, df$y2)
ggplot(df, aes(x2, y2)) + geom_point() + geom_smooth(method = "lm")

lmfit3 = lm(y3~x3, df)
coefficients(lmfit3)
plot(df$x3, df$y3)
ggplot(df, aes(x3, y3)) + geom_point() + geom_smooth(method = "lm")

plot(df$x4, df$y4)

lm_fit
?lm

#Exes[i] ~ Eyes[j]
$lm(infant.mortality ~ gdp, ldf)