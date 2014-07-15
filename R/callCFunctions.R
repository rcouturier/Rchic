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
#' @param list.occurrences.variables 	list of the occurrences of the variables.
#' @param verbose                 gives more details
#'
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export
#' @useDynLib rchic

callSimilarityComputation <- function(similarity_matrix,list.occurrences.variables,supplementary.variables,matrix.values,verbose)  {
    
    .Call("similarity", similarity_matrix,list.occurrences.variables,supplementary.variables,matrix.values,verbose)  
}


#' @title Calls the C++ hierarchy computation.
#'
#' @description Interface to call the the C++ hierarchy computation.
#'
#' @param  cohesion_matrix				cohesion matrix of the variables
#' @param	list.occurrences.variables  list of the occurrences of the variables.
#' @param verbose                 gives more details
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export
#' @useDynLib rchic

callHierarchyComputation <- function(cohesion_matrix,list.occurrences.variables,verbose)  {
  
  .Call("hierarchy", cohesion_matrix,list.occurrences.variables,verbose)  
}


#' @title Calls the C++ dynamic_cloud.
#'
#' @description Interface to call the the C++ dynamic_cloud.
#'
#' @param  vector				          vector representing the data to split
#' @param	nb.partitions 			    number of partitions
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export
#' @useDynLib rchic


callDynamicCloud <- function(vector, nb.partitions)  {
  
  .Call("dynamic_cloud", vector, nb.partitions)  
}


#' @title Calls the C++ write_transactions.
#'
#' @description Interface to call the the C++ write_transactions.
#'
#' @param  data  			          dataframe representing all the data
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export
#' @useDynLib rchic


callWriteTransactions <- function(data)  {
  #remove 1st column
  M=as.matrix(data[,-1])
  #be sure that we have numeric
  storage.mode(M)<-"numeric"
  .Call("write_transactions", M)  
}

