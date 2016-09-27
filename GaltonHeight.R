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
#names(summary(linear_fit))
###To get adjusted r squared you can do this to a fitted linear model
#summary(linear_fit)[9]
#summary(linear_fit)$adj.r.squared

df["gender"]= as.numeric(df$gender)
df$gender = df$gender-1
#as.numeric produced Male=1, Female=2. Subtract 1.
#(this even working is scandulous, btw)
#Male=0, Female=1

typeof(NA)

hist(df$childHeight)
hist(df$mother, col='red')
hist(df$father, col='blue')


ggplot(df, aes(childHeight, fill=gender)) + geom_histogram(alpha=0.2)
  #geom_density(alpha=0.2)

#aggregate data by family (a series of numebers in the $family column)
famgroup = group_by(df,family)
famgroup
mean(df$childHeight)
dplyr::summarise(famgroup)

KidHeightAvgs= dplyr::summarise(famgroup, child = mean(childHeight), na.rm=TRUE)
KidHeightAvgs

#Don't know why, but this one works *shrug*
ddply(df, "family", summarise, fun=mean(childHeight))
?summarize

summarise(famgroup, mean(childHeight))
famgroup = group_by(df, family)

# summarise(mtcars, mean(disp))
# head(mtcars)
# by_cyl <- group_by(mtcars, cyl)
# head(arrange(by_cyl))
# head(by_cyl)
# by_cyl  summarise(mtcars, 
# #Augh, I don't understand how to make group_by behave and spit out separate means for different fams.
# 
# #kid's average vs., mom, dad, midparent each get 
# fammeans<- aggregate(df, by=list(family), FUN=mean, na.rm=TRUE)
famgroup = group_by(df,family)
dplyr::summarise (famgroup, avg = mean (childHeight))

ddply(df, "family", summarise, fun=mean(childHeight))

# %>% is a piping thing

famgroup

msf = dplyr::summarise(famgroup, avg = mean(childHeight), midp = mean(midparentHeight),
                 father = mean(father), mother = mean(mother), childnum = mean(children))
names(multisumfam)

sapply(df, class)
View(df)


# head(fammeans)
# mean(famgroup$childHeight)
# midp = ggplot(famgroup, aes = (midparentHeight)) + 
# plot(midp)

midp = ggplot(msf, aes(x=midp, y=avg)) + geom_point() + geom_smooth(method=lm)
mom = ggplot(msf, aes(x=mother, y=avg)) + geom_point()
dad = ggplot(msf, aes(x=father, y=avg)) + geom_point()
histo = ggplot(msf, aes(x=childnum)) + geom_histogram()
multiplot(midp, mom, dad, histo, cols = 2)

mflm = lm(avg ~ mother + father, data=msf)
mplm = lm(avg~midp, data=msf)

#predictor1 = (mother + father) - fitted(mflm)
#predictor2 = midp - fitted(mflm)

summary(mflm)$r.squared
#^bigger number
summary(mplm)$r.squared

summary(mflm)$adj.r.squared
summary(mplm)$adj.r.squared
#^bigger number

summary(mflm)
summary(mplm)

qplot(msf$avg, (msf$mother + msf$father) - fitted(mflm))
qplot(msf$avg, msf$midp - fitted(mplm))

