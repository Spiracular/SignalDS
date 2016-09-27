go_deeper = function(Lis, max_depth = 0, depth = 0){
  tracker = depth
  max_tracker = max_depth
  for (x in 1:length(Lis)) {
    if (is.recursive(Lis[[x]])) {
      tracker = depth + 1
      if (tracker > max_tracker) {
        max_tracker = tracker
      }
      print ("track")
      print (tracker)
      print ("max")
      print (max_tracker)
      
      go_deeper(Lis[[x]], max_depth = max_tracker, depth = tracker)
    }
  }
#  return (max_tracker)
}

test2 = list(list(list(2,2)))
go_deeper(test2)
is.recursive(list(2,2))
is.recursive(2)
is.recursive(test2[[1]][[1]])




dominolist = list(list(1,1), list(1,2), list(2,2))

circle_domino = function(doms, n=NULL){
  #Check validity of the n-domino set vs. n.
  if (n!=NULL){
    check = uniques(unlist(doms))
    if (max(check) > n){
      print ("There is an invalid domino.")
      return (FALSE)
    }
  }
  
  lendom = length(check)
  tabs= table(unlist(doms))
  #Check that there are even numbers of all present domino numbers.
  if  any((tabs %% 2)!=0){
    print ("Odd numbered count.")
    return (FALSE)
  }
  #The twos clause
  
  
  
  #There are 2 domino configurations for each domino (up and down).
  #Starting domino has inflexible orientation which determines relative orientation for all other dominoes.
  #Track just head and tail. Remove dominoes from local pool as they get used to replace tail values.
  #For last domino, check if tail number matches head number.
  `
  while (length(dominoes) != 0){
    head = 
  }
  
}

grab_partner = function(tuple, num){
  if (tuple[[1]] == num) {
    return (tuple[[2]])
  } else if (tuple[[2]] == num){
    return (tuple[[1]])
  } else {
    print ("error")
  }
}


check_domino = function(dominoes){
  unlisted = unlist(dominoes)
  i = 1
  j = length(unlisted)
  if (unlisted[i] != unlisted[j]){
    return (FALSE)
  }
  i = i+1
  while (i < j){
    if (unlisted[i] == unlisted[i+1]){
      i = i+2
    } else {
      return (FALSE)
    }
  }
  return (TRUE)
}

dom = list(list(1,2), list(2,3), list(3,1), list(1,2))
check_domino(dom)


head(mtcars)

x=1:5
x
x[1:2] = c(10,11)
x

x = 1:10
x[x%%2==0] = 100
x
x[NA] != x[NA_real_]

mtcars[1:20]
length(mtcars)

x = c("a","b","a","a","b","x","b","a")
x
dictnames = c("a", "b", "x")
dictvalue  = c("apple", "banana", NA)

# dictionary = data.frame(dictvalue)
# row.names=dictnames
# dictionary

#THIS IS A COOL PROPERTY! Easier than dictionary. Check it out!
fruits = c(a="apple", b="banana")
new = fruits[x]
new

data.frame(new)
aivek = order(new)
bivek = sort(new)
aivek
new[aivek]

sampling = function(df, rows = FALSE){
  long = length(df)
  adf = ( sample(df, long) )
  if (rows){
    rong = nrow(adf)
    adf = adf[sample(rong, size=rong),]
  }
  return (adf)
}

sampling(mtcars, rows = TRUE)

?nrow

grep()
strsplit()

getSamples = function (a,n){
  y = vector(mode="numeric", length = n)
  x = rnorm(n)
  for (i in 1:length(x)){
    y[i] = a*x[i]
  }
  xydata = data.frame('x' = x, 'y' = y)
  return (xydata)
}

aset = seq(0.1, 0.9, 0.1)
nset = 100* (5^seq(0,3))

sampleSet = getSamples(0.1, 100)
head(sampleSet)

library("ggplot2")

df = sampleSet
ggplot(df, aes(x,y)) + geom_point()

