callAsirules <- function() {
  
  
  b<-c("rchic","-l","-s0","-m1","-n2","-c2",'transaction.tab',"transaction.out")
  a<-length(b)
  .C("asirules",as.integer(a),as.character(b))
}



callSimilarityComputation <- function(similarity_matrix,list.selected.item,list.occurrences.variables)  {
    
    .Call("similarity", similarity_matrix,list.selected.item,list.occurrences.variables)  
}


callHierarchyComputation <- function(cohesion_matrix,list.selected.item,list.occurrences.variables)  {
  
  .Call("hierarchy", cohesion_matrix,list.selected.item,list.occurrences.variables)  
}

