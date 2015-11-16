#' @title callHierarchyTree
#'
#' @description Reads the data, prepare the data and call hierarchyTree.
#' @details 
#' This function allows you to compute the hierarchy tree. This tree is built with the cohesion index which is not symmetric.
#' It is very important to keep this in mind. At the end of the computation we obtain the hierarchy tree, 
#' as follows. In this tree, couple of variables are gathered according to their cohesion measure. Significant levels
#' are highlighted in red. A significant level means that the considered level is more significant than the previous level
#' and than the next one. In the following figure and in all the cases, an expert must choose according to his knowledge
#' what is the threshold in the hierarchy tree after which the cohesion measure is not significant for the data considered.
#' In general the last classes in the hierarchy tree are not significant. Comparing the hierarchy tree with the similarity tree,
#' we can see that the hierarchy tree contains a greater number of classes than the similarity tree. As the cohesion is not
#' symmetric, we have an important information on the implication. In this example, we can observe that the strong class is: 
#' craftiness generally implies malignant. The second strongest class is: powerful generally implies strong.
#' \if{html}{\figure{hierarchy.png}}
#' \if{latex}{\figure{hierarchy.png}{options: width=15cm}}
#' 
#' @param   fileName             name of the file containing the data
#' @param   contribution.supp    boolean to compute the contribution of supplementary variables
#' @param   typicality.supp      boolean to compute the typicality of supplementary variables
#' @param   computing.mode       controls the computing mode: 1=classic implication, 2=classic implication+ confidence, 3=implifiance
#' @param   verbose              boolean to give many details.
#'
#' @author Raphael Couturier 
#' @export

 
callHierarchyTree <- function(fileName,contribution.supp=FALSE,typicality.supp=FALSE,computing.mode=1,verbose=FALSE) {
  #transform possible interval variables
  result = readAndAnalyzeData(fileName=fileName)
  dataCSV=result[[1]]
  supplementary.variable=result[[2]]
  
  #remove variables with only 0
  dataCSV=dataCSV[, !apply(dataCSV==0,2,all)]
  
  #prepare data for apriori
  data2transac(dataCSV)
  #call apriori
  callAsirules()
  #list of variables is needed to keep the same order in the variable when the cohesion matrix is built
  list.variables=names(dataCSV)
  list.variables=list.variables[-1]
    
  #matrix to compute the contributions and hierarchies
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
  hierarchyTree(fileName,list.variables,supplementary.variables,matrix.values,contribution.supp, typicality.supp,computing.mode=computing.mode,verbose=verbose)
  
}