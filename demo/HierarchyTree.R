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
  hierarchyTree(list.variables,Verbose=verbose)
  
}



