callAsirules <- function() {
  
  
  b<-c("rchic","-l","-s0","-m1","-n2","-c2",'transaction.tab',"transaction.out")
  a<-length(b)
  .C("asirules",as.integer(a),as.character(b))
}



callSimilarityComputation <- function(similarity_matrix,list.selected.item,list.occurrences.variables)  {
    
    .Call("similarity", similarity_matrix,list.selected.item,list.occurrences.variables)  
}


# conv <- function(a, b)
#   .C("convolve",
#      as.double(a),
#      as.integer(length(a)),
#      as.double(b),
#      as.integer(length(b)),
#      ab = double(length(a) + length(b) - 1)
#      )$ab