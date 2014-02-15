############################################################
#############    COMPUTE THE HIERARCHY TREE    ############
############################################################

library(stringr)
require(tcltk) || stop("tcltk support is absent")

#select file
fileName <- tclvalue(tkgetOpenFile())

if (!nchar(fileName)) {
  tkmessageBox(message = "No file was selected!")
  setwd(initDirectory)
} else{
  
  #read the file
  dataCSV<-read.csv(fileName, sep=";")
  data2transac(dataCSV)
  callAsirules()
  list.variables=names(dataCSV)
  list.variables=list.variables[-1]
  #list of variables is needed to keep the same order in the variable when the cohesion matrix is built
  verbose=FALSE
  hierarchyTree(list.variables,verbose=verbose)
  
}