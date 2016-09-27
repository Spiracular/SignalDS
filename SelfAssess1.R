#Self-assessment 1
#Start time: 10:35am
#Part 1: R and Probability
#install.packages("psych")
library("ggplot2")
library("dplyr")
library("psych")

#Bayes as they say in LaTex: P(A|B) = \dfrac{P(B|A)P(A)}{P(B)} (might not be relevant)
# E(X|Y=y); my estimated answer is x-hat = 2y? (this turns out to be wrong)
# E(y|x) = x/2
#

#MCsimulations!

xysampler = function(k=100){
  x = runif(k,0,1)
  y = vector("double", length = length(x))
  for (i in 1:length(x)){
    y[i] = runif(1,0,x[i])
  }
  xydf = data.frame(x=x,y=y)
  return(xydf)
}


xy10k = xysampler(10000)

qplot(x=x,y=y,data=xy10k)

#Separate y into bins by value using a width variable w. Then take the means of each bin

biny = function(df, w=0.1){
  thresholds = seq(0,1,w)
  bins = vector(mode="double", length=(length(thresholds)-1))
  for (n in 2:length(thresholds)){
    TFin = df$y>thresholds[n-1] & df$y<thresholds[n]
    bins [n-1] = mean(df[TFin,]$x)
    names(bins)[n-1] = paste(thresholds[n-1],thresholds[n], sep=" to ")
  }
  return (bins)
}

testdf= biny(xy10k, w=0.5)
head(testdf)

realdf = biny(xy10k, w=0.01)
length(realdf)
#checks out

binamed = qplot(x=names(realdf), y=realdf)

#Actual answer: E(X|Y=y) = \dfrac{y-1}{ln(y)}

estix = function(y){
  xhat = (y-1)/(log(y))
  xhat
}
realdf = data.frame(realdf)

realdf["estimx"]= sapply(_______, estix)


df = xysampler()
xy10k["estimx"] = sapply(xy10k$y, estix)

yex = qplot(xy10k$y, xy10k$estimx)
yac = qplot(xy10k$y, xy10k$x)

multiplot(binamed, yex, yac, cols = 2)
#Yes, this appears consistent. (I wish I had kept the average value of things in bins, but it's not worth changing at this point)

#Finished Section 1 by roughly 11:40am

###Section 2###
#Part 2: Data Analysis
help(msq)
#Oh cool! I know this questionaire, or a close variant!
#Compute fraction missing values for each feature, sorted in descending order.
?sort
head(msq)
names(msq)

namsq = vector("double", length=length(msq))
for (i in 1:length(namsq)){
  namsq[i] = sum(is.na(msq[i]))/ nrow(msq)
}
names(namsq) = names(msq)
namsqsort = sort(namsq,decreasing = TRUE)


c(1,2,3,NA, NA) %>% is.na %>% sum

?summarise
###dplyr::summarise(famgroup, avg = mean(childHeight)

grep("scornful", names(msq))
#A: 75
newdf = cbind(msq[1:75], msq[c('Extraversion', 'Neuroticism')])
head(newdf)

?replace

for (i in 1:length(newdf)){
  newdf[i][is.na(newdf[i])] = mean(newdf[[i]], na.rm=TRUE)
#  newdf$c = replace(newdf$c, list=is.na(newdf$c), values=mean(newdf$c, na.rm=TRUE))
}
head(newdf)

ggplot(data=newdf, aes(Extraversion)) + geom_histogram()
ggplot(data=newdf, aes(Neuroticism)) + geom_histogram()
ggplot(data=newdf, aes(Extraversion)) + geom_density()
ggplot(data=newdf, aes(Neuroticism)) + geom_density()
ggplot(data=newdf, aes(Extraversion, Neuroticism)) + geom_point() + geom_smooth()

lfit_N = lm(Neuroticism ~ ., data=newdf[-76])
lfit_E = lm(Extraversion ~ ., data=newdf[-77])

Neurcor = sort(coef(lfit_N), decreasing = TRUE)
Extrcor = sort(coef(lfit_E), decreasing = TRUE)

names(Extrcor[1:10])
names(Neurcor[1:10])

#Okay, yes, I could do the absolute value variety by applying abs before sort. I think the results make more sense without it, though.
aNeurcor = sort(abs(coef(lfit_N)), decreasing = TRUE)
aExtrcor = sort(abs(coef(lfit_E)), decreasing = TRUE)

names(aExtrcor[1:10])
names(aNeurcor[1:10])

#-1 in lm formula just means "omit intercept". (possibly explaining iris dataset function stuff from earlier?)
#scrap
sort()

?lm
?lm
grep("Extraversion", names(newdf))

#Finished by around 12:30pm
###Part3: SQL Queries###
#Q: Difference between WHERE and HAVING
#A: WHERE is placed early and uses a criterion to subset data.
# HAVING is similar, but acts on aggregate functions (ex:mean), and is placed at the end of a query after a group_by.

###PAUSE 12:45-2:15

##Q:2nd highest Salary
# #METHOD 1
# SELECT MAX(Salary) from 
#   (SELECT Salary from Employees
#     WHERE Salary != MAX(Salary)) x
# #METHOD 2
# 
# SELECT Salary from Employees
#   ORDER BY Salary
#   DESC LIMIT 2


#Q: LEFT JOIN, RIGHT JOIN, INNER JOIN?
#A: Left join will use the rows from the first table, add corresponding rows from the second table, and leave NAs in the 2nd table's rows where
#there isn't a corresponding table2 row. Right join will do the same, but switch table1 and table2.
# Inner join (aka JOIN) will give the union of the 2 tables, excluding any rows that only appear on 1 table.
# Q

# SELECT course_id, course_name FROM COURSES
#   INNER JOIN 
#   ON course_id = course_id
###(Incomplete; had not reached Join yet)
  
#So, what I need to do is fuse COURSE_FACULTY via course_id = course_id for COURSES and via faculty_id = faculty_id for FACULTY.
#Then group_by course_id and get aggregate faculty_name for each grouping.
#I did not reach join, but could finish this when I do finish Join.

#End: 2:30pm
#10:35 to 12:45 + 2:15 to 2:30
#2h10m + 15m = 2h25m

