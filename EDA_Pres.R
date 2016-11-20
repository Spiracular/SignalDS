
install.packages("rworldmap")
library("rworldmap")

library("dplyr")

#I want to use the second row as the names
#skip=1 fixes the problem of reading in the first row, when the second has the better headers.
pastTurnout=read.csv("data/ElectionData/1980_2014NovTurnout.tsv", sep="\t", skip=1)
nowTurnout =read.csv("data/ElectionData/2016NovGenElectTurnout.tsv", sep="\t", skip=1)

dim(pastTurnout)#936, 17
dim(nowTurnout)#53, 16

sapply(pastTurnout, class)
sapply(nowTurnout, class)

yearow = vector()

#Add column year to nowTurnout:2016 (53)
ye = rep.int(2016, 53) #vector(mode="integer", length=53)
emptyRows = rep(NA, 53)

nowTurnout$Year = ye

nowTurnout[c("ICPSR.State.Code","Alphanumeric.State.Code")] = emptyRows

#rename Total.Ballots.Counted to 

###AAAUUGGH okay so the 2016 is an estimate, not an actuality. I'll have to find out in a couple of weeks.

setdiff(names(pastTurnout),names(nowTurnout))
#"ICPSR.State.Code"        "Alphanumeric.State.Code" "Total.Ballots.Counted"  ##All in pastTurnout


vector(mode="integer", length=53) #defaults to 0, not NA




#rbind (not merge! it's row attatchment)
allTurnout= rbind(pastTurnout, nowTurnout)#default by = intersect(names(x),names(y))

#use a function to interpret percentages

#First chart: get the total voters by year for the United States

#select =="UnitedStates"

