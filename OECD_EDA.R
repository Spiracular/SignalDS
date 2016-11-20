library("dplyr")
library("ggplot2")
library("glmnet")

#Migration Education Location: Dataset w/ low/med/high education & employment rates
#Full name: "Employment rates by place of birth and educational attainment (25-64)"
#MigEdLoc = read.csv(file = "data/MIG_EMP_EDUCATION_01112016205346463.csv")
#Note: final line was incomplete (may have exceeded permitted data points for download)
#AUGH, I got a broken dataset :(

csvtrim = function(filename, namevec) {
  df = read.csv(filename)
  df = select(df, one_of(namevec))
  df
}

filenames = c("")
namevec = list(c(""))

df0 = csvtrim(filenames[[1]], namevec[[1]])

#Hm... I can't just fuse it on country name, can I? Ugh.

for (i in seq(2,5)){
  df = csvtrim(filenames[[i]], namevec[[i]])
}

InternetAccess = read.csv("data/ICT_HH2_01112016213439759.csv")
head(InternetAccess)
names(InternetAccess)
IntAcc = InternetAccess

unique(IntAcc$INDIC) #A1, B1, B21, B21A, B21B, HOU

unique(IntAcc$Unit)#Percentage, NUMBERS

unique(InternetAccess$Breakdowns)
head(filter(InternetAccess, BRKD=="HH_DEG_INT"))

#COU: Country abbreviation
#INDIC: Indicator being measured (ex: household computer access, household internet access, etc.)
#BRKD: Income quantile of household
#TIME: year (2005-2015)
#Unit.Code : self-explanatory. Percentage or NUMBERS
#Reference period: unused (assume annual)
#Value: actual value


IntAcc = select(InternetAccess, one_of(c("country","INDIC", "BRKD", "Time", "Unit.Code", "Value")))
sapply(IntAcc, class)

#Quick graph practice: sort out 1 country, map indicator values (PC) by year for a quantile.

?ddply

?split
?tapply

couSplits = split(IntAcc, IntAcc$country)

tail(couSplits[[3]]) #Yep, separated out as 

str(couSplits)

#Check which factor values of BRKD are available in all countries (via lapply?)

listlevel = lapply(couSplits, function(cou) unique(levels(cou$BRKD)))
intersect(listlevel)
listlevel0 = unlist(listlevel, recursive = FALSE)
#I'm thinking of Reduce, which works on vectors. Won't quite do what I want.
Reduce(intersect, lapply(listlevel())



?intersect

Patent1= read.csv("data/PAT_DEV_02112016012949822.csv")
Patent1 = read.csv("data/Tri_PAT_DEV_02112016015359572.csv")

head(Patent1)
names(Patent1)

unique(Patent1$DOM)
unique(Patent1$Technology.domain)

PatentSelect1=select(c(""))

subPatent1= select(Patent1, one_of(c("Inventor.country", "SIZE", "DOM", "Year", "Unit", "Value")))

subPatent1= filter(subPatent1, DOM=="TOT" & Year==2010)
head(subPatent1)

names(subPatent1)[[1]] = "INVcountry"
names(subPatent1)[[6]] = "Patents"

subPatent1 = select(subPatent1, one_of(c("INVcountry", "SIZE", "Patents")))

length(subPatent1$INVcountry) #217???
unique(subPatent1$Unit)

write.csv(subPatent1, file="data/LonePatent2010.csv", row.names=FALSE)


#Actually... yeah, I want to grab specific categories from somewhere
#So I may need to grab it from the original source.

#Nope, the original source is not easy to work with.

df2 = read.csv("data/BENCHMARK_STIO_02112016021819368.csv")

head(df2) #Public R&D expenditures

names(df2)
#[1] "LOCATION"   "Country"    "INDICATOR" 
#[4] "Indicator"  "TIME"       "Time"      
#[7] "Value"      "Flag.Codes" "Flags" 


df3 = read.csv("data/ICT_HH2_01112016213439759.csv")
#Internet access
head(df3)

rm(df3)


namevec1 = c("")
= select(df, one_of(namevec))

#for 2: replace time with 2010 or ignore, keep Indicators and Country, Indicator, and value


?summarise(group_by(df, col), mean(col2))



timegroup = group_by(IntAcc, Time)

summarise(timegroup, mean(Value))
summarise(timegroup, sum(Value))
summarise(timegroup, sum(is.na(Value)))
sum(is.na(IntAcc$Value))#...no NAs? Weird... makes things easy I guess? Check if they used null instead...

summarise(timegroup, length(Value))###REUSE THIS ONE

#...augh. That's just confusing... okay, so there's significant difference in number of points for different...
#...OH NO. I need to sort by Unit, durhurhur.



#Okay, subset to only some of the categories so the results will make some semblance of sense...
#(P.S. The "Indicators.pdf" file I downloaded explains what each of the unique(IntAcc$INDIC) #A1, B1, B21, B21A, B21B, HOU  factors mean.)


#A1 = "Computer at House %"
#B1 = "Internet at House %" 
#B21x = "House with broadband internet (x=none), fixed broadband (x=a), mobile broadband(x=b) %"
#HOU = "Household total numbers"
#Honestly? Pick out just A1 or B1, they're going to be super-correlated...

levels(IntAcc$INDIC)
#Houses = filter(IntAcc,  INDIC=="HOU") #Oh wow, this is only available in a very few countries... nevermind. Chunk it.

CompHome = filter(IntAcc, INDIC == "A1")
head(CompHome)
unique(CompHome$country) #35/36 countries. Pretty good! 

IntHome = filter(IntAcc, INDIC == "B1")
unique(IntHome$country)#All 36.

#intersect()

Net2010 = filter(IntHome, Time==2010)
High2010 = filter(Net2010, BRKD=="HH_Q1")#Not available for all countries, note :(
Net2010 = filter(Net2010, BRKD=="HH_TOTAL")

countryNet = group_by(Net2010, country)
summarise(countryNet, length(Value)) #31 countries

#grab country and Value, which has already been sorted into exclusively 2010 and HH_TOTAL

filter(Net2010, country=="Denmark")$BRKD
#8 possible value types: HH_TOTAL      HH_Q1         HH_Q4         HH_Q3         HH_Q2         HH_DEG_INT    HH_DEG_PRURAL HH_DEG_PURBAN
#total, quantiles, rural, urban(large), INT = small urban areas (what?)

Net2010 = select(Net2010, one_of(c("country", "Value")))

#Save the pruned dataset Net2010

write.csv(Net2010, file="data/Net2010.csv", row.names=FALSE)

head(read.csv("data/Net2010.csv"))


#Observation: 2014 and 2015 have unusually low (probably incomplete?) data. Maybe I should throw them out entirely...

###Action: throw out 2015 and 2014

IntAcc = filter(IntAcc, Time<2014)

###DO IT ABOVE HERE!


cougroup=group_by(IntAcc, country)

summarise(cougroup)

sapply(IntAcc, class)

###Lets figure out which year we want to grab
df2 = read.csv("data/BENCHMARK_STIO_02112016021819368.csv")
#Public R&D expenditures
unique(df2$Time) #2010 or latest


#####################
#R&D and education

RDEdu = read.csv("data/Cou_REGION_INNOVATION_03112016191834653.csv")
head(RDEdu) #AUGH, it doesn't include codes, I can't easily separate into percent and total number.

unique(RDEdu$PowerCode)

coreRDEDu = select(RDEdu,one_of(c("Region", "VAR", "Indicator", "Year", "Value")))

indic = as.character(unique(coreRDEDu$Indicator))
varin = as.character(unique(coreRDEDu$VAR))

VARlookup= cbind(varin, indic)
VARlookup

unique(coreRDEDu$Region) #39 countries total

patentSorted = filter(coreRDEDu, VAR == "PCT_MILLION")
patentSorted = patentSorted[order(patentSorted$Value, decreasing = TRUE), ]

coreRDEDu = select(coreRDEDu, one_of(c("Region", "VAR", "Year", "Value")))

EduPercents = filter(coreRDEDu, VAR =="EDU_LF_ISCED34_SH" | VAR == "EDU_LF_ISCED58_SH")

patentRD = filter(coreRDEDu, VAR == "RD_EXP_TOT" | VAR == "RD_PER_TOT" | VAR == "PCT_MILLION")

edu = filter(coreRDEDu, VAR == "EDU_LF_T" | VAR == "EDU_LF_ISCED34" | VAR == "EDU_LF_ISCED58")


write.csv(patentRD, file="data/RD2010.csv", row.names=FALSE)
write.csv(EduPercents, file="data/EduP2010.csv", row.names=FALSE)
write.csv(edu, file="data/EduLF2010.csv", row.names=FALSE)
write.csv(VARlookup, file="data/VarLookup.csv", row.names=FALSE)



RD_expenditures = read.csv("data/BENCHMARK_STIO_02112016021819368.csv") #R&D expenditure per GDP



FDI = read.csv("data/FDIINDEX_03112016193313622.csv")
#BTW: Primary =  things like mining and fishing. Probably not relevant?

head(FDI)

names(FDI)[[4]] = "Ind_Sector"
#remove Type.of.restriction

sapply(FDI, class)#Darn, sector is integer when it should be factors.

FDI = select(FDI, one_of(c("Country", "SECTOR", "Ind_Sector", "Year", "Value")))

write.csv(FDI, file="data/FDI.csv", row.names=FALSE)



demograph = read.csv(file="data/HEALTH_DEMR_04112016002717356.csv")
head(demograph)

demograph = select(demograph, one_of(c("VAR", "Measure", "Country", "Value")))

popula = filter(demograph, VAR == "DEMODOMP")
emply = filter(demograph, VAR == "JOBSCIVL")

popula = select(popula, -VAR)
emply = select(emply, -VAR)

names(popula)[[3]] = "Population"
names(emply)[[3]] = "Employed"


write.csv(popula, file="data/population.csv", row.names=FALSE)
write.csv(emply, file="data/employed.csv", row.names=FALSE)

#write.csv(RD2010, file="data/RD2010.csv", row.names=FALSE)


WorldBank = read.csv(file = "data/Data_Extract_From_World_Development_Indicators/d11ba2b7-1629-4545-84e3-e1df70a9a048_Data.csv")

names(WorldBank)[[5]] = "Value"
WorldBank = select(WorldBank, Country.Name, Series.Code, Value)

varin = unique(as.character(WorldBank$Series.Code))
indic = unique(as.character(WorldBank$Series.Name))

WBlookup= cbind(varin, indic)

write.csv(WBlookup, file="data/WBlookup.csv", row.names=FALSE)

#13 or 14 different metrics picked
#(AUGH, these names are so kludgy!)

#?readline(prompt=)

WBlookup = read.csv(file="data/WBlookup.csv")

#Lets grab Urban_pop , Urban_mil_pop, Edu_spendP , top_20_inc

View(WBlookup)

names(WorldBank)[[5]] = "Val"

Urbane = filter(WorldBank, Series.Code == "SP.URB.TOTL")#oops, I should have done...
Urbane = filter(WorldBank, Series.Code == "SP.URB.TOTL.IN.ZS")
Urbane = select(Urbane, Country.Name, Val)
names(Urbane)[[2]] = "UrbanPer"

Met = filter(WorldBank, Series.Code == "EN.URB.MCTY.TL.ZS")
Met = select(Met, Country.Name, Val)
names(Met)[[2]] = "Metro_pop"



Edu = filter(WorldBank, Series.Code == "SE.XPD.TOTL.GD.ZS")
Edu = select(Edu, Country.Name, Val)
names(Edu)[[2]] = "Edu_Spend" # Loses 4. Fine. Will fill in later.


City = base::merge(Urbane, Edu, by="Country.Name")
#City$Edu_Spend = sapply(City$Edu_Spend, function(x) round(x, 4)) #Damnit, I can't seem to do rounding

NewCity = base::merge(City, Met, by="Country.Name") #Metro is not available for all locations, though (lose 8).
#Possibly drop it... but it's probably better to impute the mean after importing it into the Rmkdwn


Top20 = filter(WorldBank, Series.Code=="SI.DST.05TH.20")
Top20 = select(Top20, Country.Name, Val)
names(Top20)[[2]] = "Top_20" #loses 1.

NewCity = base::merge(NewCity, Top20, by="Country.Name")

NewCity = base::merge(NewCity, Urbane, by="Country.Name")


write.csv(NewCity, file="data/citydata.csv", row.names=FALSE)
#After: grab GDP, GDP_pC , Tax_rev 

 

GDPs = filter(WorldBank, Series.Code == "NY.GDP.MKTP.KD")
GDPpC = filter(WorldBank, Series.Code == "NY.GDP.PCAP.KD")
Tax = filter(WorldBank, Series.Code == "GC.TAX.TOTL.GD.ZS")
names(GDPs)[[3]] = "GDP_tot"
names(GDPpC)[[3]] = "GDP_pC"
names(Tax)[[3]] = "Taxp"


Moneytrack = merge(GDPs, GDPpC, by="Country.Name", all=TRUE)
Moneytrack = merge(Moneytrack, Tax, by="Country.Name", all=TRUE)
#Was going to look at qplot histogram of GDP and GDP_pC, then check correlation with pop size
Moneytrack = Moneytrack[-(grep("Series.Code", names(Moneytrack)))]

write.csv(Moneytrack, file="data/GDP.csv", row.names=FALSE)

#Finally: Business_time , Business_cost 

many_GDP = read.csv("data/GDP_From_World_Development_Indicators/9a3f8901-0e58-4054-8ea5-bb6e65899feb_Data.csv")


