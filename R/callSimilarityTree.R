#' @title callSimilarityTree
#'
#' @description Reads the data, prepare the data and call similarityTree.
#' 
#' @param   filename             name of the file containing data
#' @param   contribution.supp    boolean to compute the contribution of supplementary variables
#' @param   typicality.supp      boolean to compute the typicality of supplementary variables
#' @param   verbose              boolean to give many details.
#'
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export


callSimilarityTree <- function(filename,contribution.supp=FALSE,typicality.supp=FALSE,verbose=FALSE) {
    
  #transform possible interval variables
  result = readAndAnalyzeData(filename=filename)
  dataCSV=result[[1]]
  supplementary.variables=result[[2]]
  
  #prepare data for apriori
  data2transac(dataCSV)
  #call apriori
  callAsirules()
  #list of variables is needed to keep the same order in the variable when the cohesion matrix is built
  list.variables=names(dataCSV)
  list.variables=list.variables[-1]
  
  
  #matrix to compute the contributions and similarities
  matrix.values=as.matrix(dataCSV[-1])
  storage.mode(matrix.values)<-"numeric"
  row.names(matrix.values)=row.names(dataCSV)
  
  #transform list of supplementary variables into a dataframe and then a matrix  
  supplementary.variables=data.frame(supplementary.variables)
  supplementary.variables=as.matrix(supplementary.variables)
  #add the name of individuals
  if(length(supplementary.variables)>0) {
    row.names(supplementary.variables)=row.names(dataCSV)
    storage.mode(supplementary.variables)<-"numeric"
  }
 
  print(supplementary.variables)
  
  similarityTree(list.variables, supplementary.variables,matrix.values, contribution.supp, typicality.supp, verbose=verbose)
}