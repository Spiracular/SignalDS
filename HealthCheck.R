library("dplyr")
library("plyr")

deathcsv= read.csv("data/HealthData/Multiple Cause of Death, 1999-2014.txt", sep="\t")
selectDeathcsv= read.csv("data/HealthData/2Multiple Cause of Death, 1999-2014.txt", sep="\t")
View(selectDeathcsv)
reselectDeathcsv = selectDeathcsv[order(selectDeathcsv$Year, decreasing = TRUE),]
View(reselectDeathcsv)
#Augh, it still does not include details on the specific cause of death.
#And top-15-causes is a group_by that does not cooperate with other group_by s.
#I give up, not doing this one. (0.3% is your highest percentage here)
cityDeath = read.csv("data/HealthData/CityMultiple Cause of Death, 1999-2014.txt", sep="\t")

names(cityDeath)=c(lapply(names(cityDeath), function(x) gsub("[.]","_",x) ))

View(cityDeath)

cityDeath$Rate = as.numeric(as.character(cityDeath$Crude_Rate))

city2013Tester = filter(cityDeath, Year == 2013)

sapply(city2013Tester, class) #Crude_Rate is a factor, and this is a problem

city2013Tester$Rate = as.numeric(as.character(city2013Tester$Crude_Rate))

unique(city2013Tester$Ten_Year_Age_Groups_Code)
#1 , 1-4 , 5-14 , 15-24 , 25-34 , 35-44 , 45-54 , 55-64, etc.
#up to 85+

city2013Tester = filter(city2013Tester,  Ten_Year_Age_Groups_Code %in% c("15-24","25-34","35-44", "45-54"))

View(city2013Tester)
names(city2013Tester)

microtest = filter(city2013Tester, UCD___ICD_10_113_Cause_List_Code=="GR113-016")

microtest= microtest[order(microtest$Rate, decreasing = TRUE),]

microtest= select(microtest, one_of(c("Census_Region", "X2013_Urbanization", "Ten_Year_Age_Groups", "Rate", "Deaths", "Population")))

View(microtest)

#Rate: 0.000145 becomes 14.5; most likely rate per 10^5? (Verified: 10^5)

#Compare 


#So... 

#groupby aggregate(max)

cityDeath$ c("Ten_Year_Age_Groups", "Census_Region")

cityGroup = group_by(cityDeath, Ten_Year_Age_Groups, Census_Region)

indexer = summarise(cityGroup, maxin = max(Rate), )#which.max(Rate)) #Right... doesn't provide ICD Cause, which I want. Hm...
maxed = select(cityDeath[c(indexer$maxin),], one_of(c("UCD___ICD_10_113_Cause_List", "Deaths", "Population", "Rate")))
dim(indexer)#45 x 3
dim(maxed) #45 x 4

TopDeath = cbind(maxed, "Region" = indexer$Census_Region, "Age" = indexer$Ten_Year_Age_Groups, "maxin" = indexer$maxin)
View(TopDeath)

#Baby gun-related suicide seems insane; check that the selection in maxed worked correctly.
#May be slicing the smaller values first; see if ordering it results in a difference


nin = indexer$maxin[order(indexer$maxin)]
select(cityDeath[nin,], matches("UCD___ICD_10_113_Cause_List"))[1:20,] #Nope, looks different

cityDeath[72,]#Not matching: 25-34 year olds had the gun-related suicide rate mentioned.

###It's either grouped, or not grouped, and either way the maxin you're getting is wrong.

#"UCD___ICD_10_113_Cause_List","Region","Age", "Deaths", "Population", "Rate"
#c("Year","Census_Region","X2013_Urbanization", "Ten_Year_Age_Groups", "UCD___ICD_10_113_Cause_List","Rate", "Deaths", "Population")

newdf = ddply(cityGroup, .(Census_Region, Ten_Year_Age_Groups, Year), function(x) x[which.max(x$Rate),] )

topRAY = select(newdf, one_of(c("Year","Census_Region","X2013_Urbanization", "Ten_Year_Age_Groups", "UCD___ICD_10_113_Cause_List","Rate", "Deaths", "Population")))
newdf$

View(topRAY)

#For pretty much every age group, accidents(unintentional injuries) and malignant neoplasms dominate.
#(out of the things I selected, anyway)
#For 85+, majority is on other diseases I didn't select.

#Test: see if you can get significant differences in malignant neoplasm or accident(unintentional) rates
#Regress against location, overall population (norm'd), age group (rm 85+), urbanization (-1), year

print(names(TopDeath))

order(c(3,1,7, 4))#ascending order indices

##order(c(3,4,1)) #Yields 312, which seems wrong. Huh. But it sorts out right when you stick it in brackets...

max(Rate)


##New idea: check highest death-rate causes by groupby causes & ages, sum (rates? deaths?), and sort.
##Also look at urban vs. rural death causes.

group = group_by(
