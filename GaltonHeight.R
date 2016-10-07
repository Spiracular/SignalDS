#Galton's Height Data
library("HistData")
library("dplyr")
library("Rmisc")
library("ggplot2")
df = GaltonFamilies

View(df)
?GaltonFamilies

hist(df)
colnames(df)


df["gender"]= as.numeric(df$gender)
df$gender = df$gender-1
#as.numeric produced Male=1, Female=2. Subtract 1.
#(this even working is scandulous, btw)
#Male=0, Female=1

typeof(NA)

hist(df$childHeight)
hist(df$mother, col='red')
hist(df$father, col='blue')


ggplot(df, aes(x)) + geom_histogram(fill = "#CC0000", colour = "#AA0000")
#ggplot(df, aes()) + geom_histogram()