#' @title Calls the C++ ASI rules processor.
#'
#' @description Interface to call the ASI rules processor.
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export
#' @useDynLib rchic

callAsirules <- function(){
  b<-c("rchic","-l","-s0","-m1","-n2","-c2",'transaction.tab',"transaction.out")
  a<-length(b)
  .C("asirules",as.integer(a),as.character(b))
}


#' @title Calls the C++ similarity computation.
#'
#' @description Interface to call the the C++ similarity computation.
#' 
#' @param similarity_matrix 			matrix of similarities of the variables.
#' @param list.selected.item 			subset of variables to apply the computation to.
#' @param list.occurrences.variables 	list of the occurrences of the variables.
#' @param verbose                 gives more details
#'
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export
#' @useDynLib rchic

callSimilarityComputation <- function(similarity_matrix,list.selected.item,list.occurrences.variables,verbose)  {
    
    .Call("similarity", similarity_matrix,list.selected.item,list.occurrences.variables,verbose)  
}


#' @title Calls the C++ hierarchy computation.
#'
#' @description Interface to call the the C++ hierarchy computation.
#'
#' @param	cohesion_matrix				cohesion matrix of the variables
#' @param	list.selected.item 			subset of variables to apply the computation to.
#' @param	list.occurrences.variables  list of the occurrences of the variables.
#' @param verbose                 gives more details
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export
#' @useDynLib rchic

callHierarchyComputation <- function(cohesion_matrix,list.selected.item,list.occurrences.variables,verbose)  {
  
  .Call("hierarchy", cohesion_matrix,list.selected.item,list.occurrences.variables,verbose)  
}

