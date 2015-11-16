#' @title callSimilarityTree
#'
#' @description Reads the data, prepare the data and call similarityTree.
#' @details 
#' This function allows you to compute the similarity tree. The similarity measure is symetric. At the end of the computation we obtain the similarity tree, 
#' as follows. In this tree, couple of variables are gathered according to their similarity measure. Significant levels
#' are highlighted in red. A significant level means that the considered level is more significant than the previous level
#' and than the next one. In the following figure and in all the cases, an expert must choose according to his knowledge
#' what is the threshold in the similarity tree after which the similarity measure is not significant for the data considered.
#' In general the last classes in the similarity tree are not significant. On this figure, we can remark that the strongest 
#' class is: strong powerful. In this case, this two variables are very similar. The 
#' second strongest class is: large heavy.
#' \if{html}{\figure{similarity.png}}
#' \if{latex}{\figure{similarity.png}{options: width=15cm}}

#' 
#' @param   fileName             name of the file containing the data
#' @param   contribution.supp    boolean to compute the contribution of supplementary variables
#' @param   typicality.supp      boolean to compute the typicality of supplementary variables
#' @param   verbose              boolean to give many details.
#'
#' @author Raphael Couturier 
#' @export

 
callSimilarityTree <- function(fileName,contribution.supp=FALSE,typicality.supp=FALSE,verbose=FALSE) {

  #transform possible interval variables
  result = readAndAnalyzeData(fileName=fileName)
  dataCSV=result[[1]]
  supplementary.variables=result[[2]]
  
  #remove variables with only 0
  dataCSV=dataCSV[, !apply(dataCSV==0,2,all)]
  
  #prepare data for apriori
  data2transac(dataCSV)
  #call apriori
  callAsirules()
  #list of variables is needed to keep the same order in the variable when the cohesion matrix is built
  list.variables<-names(dataCSV)
  list.variables<-list.variables[-1]
  
  
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
 
  print(list.variables)
  
  
  
  similarityTree(fileName,list.variables, supplementary.variables,matrix.values, contribution.supp, typicality.supp, verbose=verbose)
}