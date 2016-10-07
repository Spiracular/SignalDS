#Euler Problem 3
##Largest Prime Factor

mk_primes = function(n, test=FALSE){
  allint = 1:n
  #pm = vector("logical", length=(n-1))
  pm = rep(x=c(TRUE, FALSE), length.out=(n))
  #pm[1] = TRUE
  pm[1:2] = c(FALSE,FALSE)
  
  #Note: initializes false
  for (i in allint[pm]){
    for (j in allint[pm & allint>=i]){
        if (i*j<=n){
          pm[i*j] = FALSE
        }
      }
  }
  
  pm[2] = TRUE
  
  if (test==FALSE){
    return(allint[pm])
  } else{
    return(list(pm[n], allint[pm]))
  }
}

primefacs = function(n){
  primeset= mk_primes(ceiling(sqrt(n)))
  tfvec = logical(length(primeset))
  for (i in 1:length(primeset)){
    if (n %% primeset[i] == 0){
      tfvec[i]= TRUE
    }
  }
  return(primeset[tfvec])
}


# maxprimefacs = function(n){
#   i:
#   primeset= mk_primes(ceiling(sqrt(n)))
#   tfvec = logical(length(primeset))
#   for (i in 1:length(primeset)){
#     if (n %% primeset[i] == 0){
#       tfvec[i]= TRUE
#     }
#   }
#   return(primeset[tfvec])
# }

#system.time(replicate(2, mk_primes())) ###NOPE NOPE NOPE BAD.
mk_primes

max(primefacs(600851475143))
#Darn it, inadequately optimized. I should be searching downward and yielding the first success?


#Euler Problem 5: Smallest Multiple

mk_primes(20)
#union of prime factors to each number between 1 and 20. (Oh! Weird thing that I need 4 for 20 and others...)
#Hm... eliminate primes already supplied by existing numbers, descending?
#Then multiply them together.

primefacs(20)
20/primefacs(20)

for (i in seq(n,1,-1)){
  
}


1:20

#Euler Problem 7: 10001st Prime

#Euler Problem 15: Lattice Paths





#Euler 18: Maximum Path Sum
#Structure of data going in

tristr = list()
for (i in 1:15){
  tristr[i] = 
    }


splittable= 
"75
95, 64
17, 47, 82
18, 35, 87, 10
20, 04, 82, 47, 65
19, 01, 23, 75, 03, 34
88, 02, 77, 73, 07, 63, 67
99, 65, 04, 28, 06, 16, 70, 92
41, 41, 26, 56, 83, 40, 80, 70, 33
41, 48, 72, 33, 47, 32, 37, 16, 94, 29
53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14
70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57
91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48
63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31
04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23"

splittable


trianglestr = strsplit(splittable, split="\n")
spacedstrs = lapply(trianglestr, function(x) c(strsplit(x,", ")))
as.integer(spacedstrs[3])


trinumlist = vector("list", length=length(spacedstrs[[1]]))
for (i in 1:length(spacedstrs[[1]])){
  trinumlist[[i]]= as.integer(spacedstrs[[1]][[i]])
}

keeptrinumlist = trinumlist
trinumlist = keeptrinumlist

trinumlist[[5]][1:2]

length(trinumlist)
trinumlist[[15]]


i=1
m=2
max(trinumlist[[(i+1)]][(m-1):m])
trinumlist[[2]][0:1]

#Okay, so the excellent way to do it that Will implemented was to add in the highest touching node to one level higher, and repeat.

n = length(trinumlist)
for (i in seq((n-1),1,-1)){
  for (m in 1:length(trinumlist[[i]])){
    trinumlist[[i]][m] = trinumlist[[i]][m]+max(trinumlist[[(i+1)]][m], trinumlist[[i+1]][m+1])
  }
}

trinumlist[[(i+1)]][m]
trinumlist

lgtrianglestr= readLines("p067_triangle.txt")
# getwd()
# setwd(dir = "SignalDS/")

lgtrianglestr[1:20]

trilines = strsplit(lgtrianglestr, split="\n")
trilines[1:5]
spacecstr = lapply(trilines, function(x) c(strsplit(x," ")))
trinumlist = vector("list", length=length(spacecstr[[1]]))
for (i in 1:length(spacecstr)){
  trinumlist[[i]]= sapply(spacecstr[[i]], as.integer)
}

trinumlist[1:20]

n = length(trinumlist)
for (i in seq((n-1),1,-1)){
  for (m in 1:length(trinumlist[[i]])){
    trinumlist[[i]][m] = trinumlist[[i]][m]+max(trinumlist[[(i+1)]][m], trinumlist[[i+1]][m+1])
  }
}

