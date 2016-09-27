namevec = c(name1=1, name2=2)
names(namevec)  = c("nomen1", "nomen2")
namevec

namevec[c("nomen1", "nomen1")]

mixed = list(1,2, TRUE, FALSE, "apple", "orange")
nullist = list()
typeof(mixed)
typeof(nullist)

light = unlist(mixed[1:4])
typeof(light)

nope = as.vector(mixed[1:4])
typeof(nope)
#Okay, I really don't know what happened here other than generating something 1x4 matrix and df-like.

#x["A"] = list of 1 item. x$A returns the values in A.

combine = function(a,b){
  ab= c(a,b)
  lab = list(ab)
  return (lab)
}

mixlight = combine(light, mixed)
typeof(mixlight)

#Prediction: vector in list: stays vector-in-list. list in vector, decompress or converts whole into list (probs the later).

nestlist = list(1,1,2,3,5)
nestvec = c(6,4,2,1)

newly = c(nestlist, nestvec)
typeof(newly)

length(nestlist)

nestlist[1] = 4
nestlist[3] = list(nestvec)
nestlist
str(nestlist)
?names
#str = structure
#List will not accept vector to nestle in list, but will accept list to nestle in list.
names(df)[1]
?as.data.frame()
?do.call
?rep
?letters
?data.frame
?unique
x = NA
is.na(x)

x=list(1:5)
x[1]
x[[1]]

#lapply : l as in lambda? NOPE! l as in list. sapply is simpler
#check out mapply later.

lapply(1:10, log, 10)

if (T, T, F){
  print ("ha!")
}

?gsub_
?gsub([:space:], ".", )
