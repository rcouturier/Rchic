#' @title Calls the C++ ASI rules processor.
#'
#' @description Interface to call the ASI rules processor.
#' 
#' @author Raphael Couturier 
#' @export
#' 
#' @importFrom Rcpp evalCpp
#'
#' @useDynLib rchic
# 
# callAsirules <- function(){
#   b<-c("rchic","-l","-s0","-m1","-n2","-c0",'transaction.tab',"transaction.out")
#   a<-length(b)
#   r<-.C("_asirules",as.integer(a),as.character(b),PACKAGE = 'rchic')
#   
# }


#' @title Calls the C++ similarity computation.
#'
#' @description Interface to call the the C++ similarity computation.
#' 
#' @param similarity_matrix 			matrix of similarities of the variables.
#' @param list.occurrences.variables 	list of the occurrences of the variables.
#' @param supplementary.variables  list of supplementary variables.
#' @param matrix.values       matrix with values of individuals (used to compute the contributions and typicalities).
#' @param contribution.supp    boolean to compute the contribution of supplementary variables
#' @param typicality.supp      boolean to compute the typicality of supplementary variables

#' @param verbose                 gives more details
#'
#' @author Raphael Couturier 
#' @export
#' @useDynLib rchic

callSimilarityComputation <- function(similarity_matrix,list.occurrences.variables,
                                      supplementary.variables,matrix.values,
                                      contribution.supp, typicality.supp,verbose)  {
    
    .Call("_rchic_similarity", similarity_matrix,list.occurrences.variables,supplementary.variables,matrix.values,
          contribution.supp, typicality.supp, verbose)  
}


#' @title Calls the C++ hierarchy computation.
#'
#' @description Interface to call the the C++ hierarchy computation.
#'
#' @param  cohesion_matrix				cohesion matrix of the variables
#' @param	list.occurrences.variables  list of the occurrences of the variables.
#' @param supplementary.variables  list of supplementary variables.
#' @param matrix.values       matrix with values of individuals (used to compute the contributions and typicalities).
#' @param   contribution.supp    boolean to compute the contribution of supplementary variables
#' @param   typicality.supp      boolean to compute the typicality of supplementary variables
#' @param verbose                 gives more details
#' 
#' @author Raphael Couturier 
#' @export
#' @useDynLib rchic

callHierarchyComputation <- function(cohesion_matrix,list.occurrences.variables,
                                     supplementary.variables,matrix.values,contribution.supp, typicality.supp,verbose)  {
  
  .Call("_rchic_hierarchy", cohesion_matrix,list.occurrences.variables,supplementary.variables,matrix.values,
        contribution.supp, typicality.supp,verbose)  
}


#' @title Calls the C++ dynamic_cloud.
#'
#' @description Interface to call the the C++ dynamic_cloud.
#'
#' @param  vector				          vector representing the data to split
#' @param	nb.partitions 			    number of partitions
#' 
#' @author Raphael Couturier 
#' @export
#' @useDynLib rchic


callDynamicCloud <- function(vector, nb.partitions)  {
  
  .Call("_rchic_dynamic_cloud", vector, nb.partitions)  
}


#' @title Calls the C++ write_transactions.
#'
#' @description Interface to call the the C++ write_transactions.
#'
#' @param  data  			          dataframe representing all the data
#' 
#' @author Raphael Couturier 
#' @export
#' @useDynLib rchic


callWriteTransactions <- function(data)  {
  #remove 1st column
  M=as.matrix(data[,-1])
  #be sure that we have numeric
  storage.mode(M)<-"numeric"
  .Call('_rchic_write_transactions', M)  
}


#' @title Calls the C++ write_transactions.
#'
#' @description Interface to call the the C++ write_transactions.
#'
#' @param  x  			          dataframe representing all the data
#' 
#' @author Raphael Couturier 
#' @export
#' @useDynLib rchic
#' 

callAsirules <- function(){ 
  b<-c("rchic","-l","-s0","-m1","-n2","-c0",'transaction.tab',"transaction.out")
  a<-length(b)
  
  call_apriori(as.integer(a),as.character(b))
}
