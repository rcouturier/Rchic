#' @title Calls the C++ ASI rules processor.
#'
#' @description Interface to call the ASI rules processor.
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export

callAsirules <- function(){
  b<-c("rchic","-l","-s0","-m1","-n2","-c2",'transaction.tab',"transaction.out")
  a<-length(b)
  .C("asirules",as.integer(a),as.character(b))
}


#' @title Calls the C++ similarity computation.
#'
#' @description Interface to call the the C++ similarity computation.
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export

callSimilarityComputation <- function(similarity_matrix,list.selected.item,list.occurrences.variables)  {
    
    .Call("similarity", similarity_matrix,list.selected.item,list.occurrences.variables)  
}


#' @title Calls the C++ hierarchy computation.
#'
#' @description Interface to call the the C++ hierarchy computation.
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export

callHierarchyComputation <- function(cohesion_matrix,list.selected.item,list.occurrences.variables)  {
  
  .Call("hierarchy", cohesion_matrix,list.selected.item,list.occurrences.variables)  
}

