?attributes
cars
attributes(cars)
head(cars)


dbattr = function(nm){
  #For item in attributes$
  outgoing =list()
  for(x in 1:length(nm)){
    y = nm[[x]]
    new = rep(y,2)
    db = paste(new, collapse="")
    outgoing = c(outgoing, db)
  }
  return (outgoing)
}
#x = append(x, new)

#attr() covers a lot of oft-changing variables
#there are also specialized things names(), dim(), class()


abfact = factor(c("a","b","a", "a"))
abfact
levels(abfact)[3] = "z"
levels(abfact)
abfact[1] = "y"
cdfact = factor(c("d","c","d","c"))
abcd = c(abfact, cdfact)
abcd
typeof(abcd)
cdlev = levels(cdfact)
as.numeric(abfact)
as.numeric(cdfact)
#c(factor1, factor2) appends factor2 to the end factor1 as numerics

#read.csv(na.strings=) is a better way of doing this but... coercing from factor to character to double can work. stringsasfactors = FALSE helps

#Warning: doing math on factors when you think they're numeric can result in bad math. Watch out!
sapply(cars, class)
#^This is roughly equivalent to dtypes(df) in pandas

f1 = factor(letters)
f2= rev(factor(letters))
f3 = factor(letters, levels=rev(letters))

fruits = c("apple", "grapefruit", "NA", "apple", "-", "grapefruit", "durian")
newfruit = factor(fruits, exclude=c("NA", "-"))
Nalist = list (NA_character_, NA_integer_, NA_real_, NA_complex_, NA)


char_to_fact = function(char_vec) {
  factor(char_vec,exclude=c())
}

half_factor = function(df) {
  for (i in 1:(ncol(df)/2)) {
    df[[i]] = factor(df[[i]])
  }
  return(df)
}


factorize_upto = function(df,n=5) {
  for (i in 1:ncol(df)) {
    if (length(unique(df[[i]])) < n) {
      df[[i]] = factor(df[[i]])
    }
  }
  return(df)
}

fill_NA = function(df) {
  for (i in 1:ncol(df)) {
    counts = table(df[i])
    counts = counts[order(counts,decreasing=TRUE)]
    most_common_val = names(counts)[1]
    #most_common_val will be a character, convert it back if necessary
    if (is.numeric(df[i])) {
      most_common_val = as.numeric(most_common_val)
    }
    df[i][is.na(df[i])] = most_common_val
  }
  return(df)
}


fill_NA_proportional = function(df) {
  for (i in 1:ncol(df)) {
    num_NA = sum(is.na(df[i]))
    fill_values = sample(df[i][!is.na(df[i])],num_NA)
    df[i][is.na(df[i])] = fill_values
  }
  return(df)
}

replicate(4,fill_NA_proportional(df)) #Does not work, figure out how to do the desired thing later.
df = data.frame(a = c(1,1,2,NA), b=c(3,4,NA,3))

model.matrix( ~ Species - 1, data=iris)
#Grabbed from elsewhere, generates dummy variables for 

head(iris)
mtcars

smiris = sample(iris, 10)

?sample

sindex = sample(1:nrow(iris), 10)
smiris = iris[sindex,]

dummyiris = model.matrix( ~ Species - 1, data=smiris)
#Checked this solution, it generalizes!

sapply(smiris, class)

#Generate the T F for index == factoritem, then convert to numeric.

dummy_var = function(df){
  dfanswer = df
  for (i in 1:ncol(df)){
    if (is.factor(df[[i]])){
      levelset = levels(df[[i]])
      for (x in levelset){
        TFmap = df[[i]]==x
        newcol = as.numeric(TFmap)
        newname = paste(c(names(df[i]), x), collapse=".")
#        print(newname)
#        dfanswer = cbind(dfanswer, newest = newcol)
        dfanswer[newname] = newcol
      }
    }  
  }
  return (dfanswer)
}

dummy_var(factdf)

paste(c( names(factdf[1]) , 1) , collapse = ".")
names(factdf[1])

getwd
load("time.dat")
#loads into df by default. No idea how to change that, but it works.
head(df)
#H2GH42: bedtime weeknights, H2GH43: bedtime during the summer
View(TeenHealth)

#install.packages()

?strftime()
sapply(df, class)
#Darn, they're being stored as factors.
df[] = lapply(df, as.character)


dftime = data.frame(lapply(lapply(df, strptime(df, format = "%I:%M%p"))))
as.Date
  
strptime("9:00AM", format = "%I:%M%p")


mtimestrip = function(a){
  strptime(paste(c(a,"M"), collapse=''), format = "%I:%M%p")
}

dftime = data.frame(lapply(df[[1]], mtimestrip))
head(dftime)
str(dftime)
shortdftime = dftime[1:3]
shortdftime

after_eight = function(time_str) {
  hour = as.numeric(substr(time_str,1,2))
  minute = as.numeric(substr(time_str,4,5))
  for(x in time_str)
  pm = substr(time_str,6,6)
  #print(pm)
  #array TF for pm=="P"
  TFIndex = (pm=="P" || (hour=12 && pm=="A"))
  hour[TFIndex] = hour[TFIndex] + 12
  # if (pm == "P") {
  #   hour = hour + 12
  # }
  return((hour + 4)%%24 + minute/60)
}
df_small = head(df)
df_after8 = data.frame(H2GH42 = lapply(df_small['H2GH42'],after_eight),H2GH43 = lapply(df_small['H2GH43'],after_eight))
df_after8

df_small



############
load("time.dat")
df_after = data.frame(H2GH42 = lapply(df['H2GH42'],after_eight),H2GH43 = lapply(df['H2GH43'],after_eight))
nrow(df)
df[135,1]
df_after[135,1]
#row 135 is garbage, we should remove it
#df_seen = df_after[df_after$H2GH42<25,]
sapply(df_seen,class)
ggplot(df_seen, aes(H2GH42)) + geom_histogram(fill = "#CC0000", colour = "#AA0000")
ggplot(df_after, aes(H2GH43)) + geom_histogram(fill = "#CC0000", colour = "#AA0000")
############

matty = matrix(1:100, nrow=10)

rearrange = function(mx){
  dim(mx) = c(dim(mx)[2],dim(mx)[1])
  return(mx)
}

df = data.frame(matrix(1:100, nrow=10))
df[5,5] = NA
df[6,6] = NA
is.na(df)
df[is.na(df)]

divisible_by_k = function(df,k) {
  return(df[(df %% k) == 0])
}

min_matrix = function(n,m) {
  result = matrix(nrow = n, ncol = m)
  for (i in 1:n) {
    for (j in 1:m) {
      result[i,j] = min(c(i,j))
    }
  }
  return(result)
}

is_symmetric = function(mx) {
  return(all(as.logical(mx == t(mx))))
}

trace = function(mx) {
  return(sum(diag(a)))
}

matrix_mult = function(A,B) {
  if (dim(A)[2] != dim(B)[1]) { stop() }
  result = matrix(nrow=nrow(A),ncol=ncol(B))
  for (i in 1:nrow(A)) {
    for (j in 1:ncol(B)) {
      result[i,j] = sum(A[i,]*B[,j])
    }
  }
  return(result)
}

system.time(replicate(1000, matrix_mult(a,b)))

system.time(replicate(1000, a %*% b))