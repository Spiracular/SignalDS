getwd()
setwd("/Users/Spirapple/Desktop/Projects/PythonDataSci/SignalDS/")
getwd()

#Suggestion: use = preferentially to <-, whitespace doesn't break it and you can define thigns locally without problems.

#Note: default numeric is double, integer appended with L
typeof(5)
typeof(5L)
#Addition of integers keeps integer, division of integers to decimals makes double

is.numeric(3)

typeof('querty')

typeof(TRUE)

#Atomic vectors can contain only a single type, formed with c()

mixed= c(TRUE, FALSE, 5L)
typeof(mixed)
mixed
#Great! It turns True into 1 and False into 0. That's a nice behavior.

boolsy = c(TRUE, FALSE, FALSE)
typeof(boolsy)
boolsy

numerals = c(1,2)
typeof(numerals)
numerals

#null vector
nullard = c()
typeof(nullard)
nullard

stringbros = c("John", "Adam", "David")
typeof(stringbros)
stringbros

broken = c(TRUE,"john", FALSE, 5L)
broken
#Turns into character type for everything

is.atomic(nullard)

v = c(1,2,3)
v = c(v,4)
v

#Ordering narrow to specific: logical[->0,1], numeric[->"numeric"], character
logicbroke = as.logical(broken)
logicbroke
brokenum = as.numeric(broken)
brokenum
#NA introduced, loses the stringy booleans. NA's default type is logical, but you can make NA_real_ and NA_character_
is.atomic(brokenum)

#Calculate number and proportion of TRUE values in a logical vecotor
numtrue = sum(boolsy)
nummean = mean(boolsy)

numimean = mean(boolsy)
numitrue = numimean*length(boolsy)

numtrue == numitrue
nummean == numimean

c(list(1), "a")
#...makes matrix?

c(TRUE, 1L)

#Logical vectors can interact with all the other types; they are the most general type?
x=1
x = runif(1)
if (x<0.5){
  print(x)
}  else {
    print(x)
}

#runif samples from a uniform distribution between 1 and the argument given

numerals
paste("label ", numerals)
holster = c()

for (n in c(1:30)){
  p = paste("label ", n)
  holster = c(holster, p)
}
holster

#get 10 rnorm values
#in rnorm, n = number of samples (on a mean=0, std=1 distribution) [there are other norm types that take in different args, cool]
x  = rnorm(10)

for (n in c(1:length(x))){
  if (x[n]<0.5){
    print(x[n])
  }  else {
    print('big')
  }
}

#Getting sums
#Answer to sum Q 1
held = c()
for (i in c(10:100)){
  track = i^3 + 4*i^2
  held = c(held, track)
}
sum(held)

#Answer to sum Q 1
held = c()
for (i in c(10:100)){
  track = i^3 + 4*i^2
  held = c(held, track)
}
sum(held)

###
#Timing calling of items
x = 1:10
tail(x,5)
system.time(replicate(1000, tail(x,5)))
system.time(replicate(1000, length(x)))
system.time(replicate(1000, seq(1:10)))
#Tail is much harder than even full seq?
?tail
?length
?seq
###
#e^x cos(x) functions

z = c()
for (n in seq(3, 6, 0.1)){
  b = exp(x)*cos(x)
  z = c(z, b)
}
head(z)
length(z)
#...this is troublingly long, but.. augh, whatever.
#Print out all pairs of integers from 1 to 20 without repeats
a = 1:20
last = 0
for (n in a){
  for (d in n:20){
#    print (d)
    print (paste(n, d, sep = ', '))
  }
}
#Note: seq did not behave as desired here, yielding 1, 2 for seq(18:20)


3 %% 2

0 == FALSE

###
collatz <- function(n) {
  i = as.integer(n)
  if (!(i %% 2)){
    i = i/2
  } else {
    i = i*3+1
  }
  return(i)
}
collatz(3)
###
#Histogram
tracking = c()
for (x in 1:100){
  track = 0
  coll = as.integer(x)
  while ((coll != 1)){
    coll = collatz(coll)
    track = track+1
  }
  tracking = c(tracking, track)
}
head(tracking)
hist(tracking)
###


###Start Taxicheck###
taxicheck <- function(n){
  i = as.integer(n)
  track = 0
  hsq = as.integer(i/3)
  
  if (!is.integer(i)){
    print ("Give an integer!")
    return (0)
    stop()
  }
  
  for (x in 1:hsq){
    for (y in x:hsq){
      z = x^3+y^3
      if ((z==i)){
        track = track + 1
      } else if ((z>i)){
        break
      }
    }
  }
  
  return(track)
}

taxicheck(1729)

###End Taxicheck###

###Start Scratch###
i = 9
hsq = as.integer(i/3)
track  = 0

for (x in 1:hsq){
  for (y in x:hsq){
    z = x^3+y^3
    print (z)
    if ((z==i)){
      track = track + 1
    } else if ((z>i)){
      break
    }
  }
}
print (track)

########
###
#if (coll <= 0){
#  print ("Use a positive integer")
#  break
#}

?system.time

#install.packages("")

##################
###Fibonacci Generator###
#(partial, incomplete)#
fib_nums = numeric(500)
fib <- function(n){
  if (fib_nums(n)>0){
    return (fib_nums(n))
  } else {
    fib_nums(n) <<- fib(n-1) + fib(n-2)
  }
}


#Initializing an empty list: vector(mode="list", length = number)

?is.atomic
is.atomic("n")
?is.recursive
l=list(list(4),2)
is.recursive(l[[1]][[1]])
list(1,2,3)

go_deeper = function(Lis, max_depth = 0, depth = 0){
  tracker = depth
  max_tracker = max_depth
  for (x in 1:length(Lis)) {
    if (is.recursive(Lis[[x]])) {
      tracker = depth + 1
      go_deeper(Lis[[x]], max_depth = max_tracker, depth = tracker)
    } else if (tracker > max_tracker) {
      max_tracker = tracker
      tracker=0
    } else {
      tracker = 0
    }
  }
  return (max_tracker)
}


nestlist = list(list(list(c(1,2,3)), list(1,2,list(1,2))))
go_deeper()
  
}

