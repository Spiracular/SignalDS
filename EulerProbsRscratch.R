#Euler Problem 3: Largest Prime Factor
mk_primes = function(x){
  full = seq.int(3,x,by=2)
  for (i in full){
    for (j in full){
      summy = i*j
      
      full = full[full!=summy]
    }
  }
  full = c(1, 2, full)
  full
}

newprimes = mk_primes(13195)



PrimeFactor = function(x){
  pf = integer()
  high = ceiling(sqrt(x))
  if (x %% 2 == 0){
    pf = c(pf, 2)
  }
  
  for (i in seq.int(3, high, by=2)){
    if (x %% i == 0){
      
    }
  }
  pf[pf != 0]
}

?seq
seq.int(3, high, by=2)

PrimeFactor(13195)


#Euler Problem 5: Smallest Multiple

#Euler Problem 7: 10001st Prime

#Euler Problem 15: Lattice Paths



# trilines[[2]]
# length(trilines)
# 
# 
# spacedstrs = lapply(trianglestr, function(x) c(strsplit(x,", ")))
# as.integer(spacedstrs[3])
# 
# 
# trinumlist = vector("list", length=length(spacedstrs[[1]]))
# for (i in 1:length(spacedstrs[[1]])){
#   trinumlist[[i]]= as.integer(spacedstrs[[1]][[i]])
# }


# newtriangle = vector("list", length=length(trilines))
# for (i in 1:length(trilines)){
#   newtriangle[[i]] = c(strsplit(trilines[[i]]," "))
#   for (j in 1:length(trilines[[i]])){
#     newtriangle[[i]][[j]] = as.integer(newtriangle[[i]][[j]])
#   }
# }
# for (i in 1:length(trilines)){
#   for (j in 1:length(trilines[[i]])){
#     newtriangle[[i]][j] = as.integer(newtriangle[[i]][j])
#   }
# #  newtriangle[[i]][j] = sapply(newtriangle[[i]], as.integer)
# }



newtriangle[[3]][[1]][2]

strsplit  c as.integer




