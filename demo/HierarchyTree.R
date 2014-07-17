############################################################
#############    COMPUTE THE HIERARCHY TREE    ############
############################################################

require(stringr) || stop("stringr support is absent")
require(tcltk2) || stop("tcltk2 support is absent")


#select file
fileName <- tclvalue(tkgetOpenFile())

if (!nchar(fileName)) {
  tkmessageBox(message = "No file was selected!")
  setwd(initDirectory)
} else{
  
  
  #transform possible interval variables
  result = readAndAnalyzeData(dataCSV)
  dataCSV=result[[1]]
  supplementary.variable=result[[2]]
  
  #prepare data for apriori
  data2transac(dataCSV)
  #call apriori
  callAsirules()
  list.variables=names(dataCSV)
  list.variables=list.variables[-1]
  #list of variables is needed to keep the same order in the variable when the cohesion matrix is built
  verbose=FALSE
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
  hierarchyTree(list.variables,supplementary.variables,matrix.values,Verbose=verbose)
  
}



