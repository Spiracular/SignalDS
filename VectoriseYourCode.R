#Vectorise Your Code
#aka Advanced R Day 5

?match
x[is.na(x)] = 0
#replaces all NA values with 0 in an efficient fashion.

#Extracting or replacing values in scattered locations on matrix or df? subset w/integer matrix.

?cut
#Could have been useful for range defining in today's self-assessment...
?findInterval
#Predicts interval numbers from interval-grouped data
?diff
#...don't understand very well, tbh.

#Matrix algebra executed highly efficiently via external library BLAS.

#Downside of vectorization: harder to predict time when things scale up. Could be in a favorable direction, though.

#Can write own vectorised function in C++ w/ Rcpp
#vapply: specify your type before running

#apply always turns its data into a matrix. This makes inserting dfs into it inadvisable from a speed standpoint.


mean(c(1,2,4))
mean.default(c(1,2,4))
#Faster than mean, less failsafe if you feed it non-numerics.

#Faster way to turn numeric vectors into a dataframe (as.data.frame is safe vs. data types, but slow and relies on rbind())
#But if you hand it the wrong data type, you get a corrupted dataframe and a series of warnings.

quickdf = function(n){
  class(n) = "data.frame"
  #Yes! Apparently you can do this!
  attr(n, "row.names") = .set_row_names(length(n[[1]]))
  1
}

?rpois
#Oh! Poisson distribution. (From R Inferno)

?Lognormal
