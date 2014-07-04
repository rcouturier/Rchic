##############################################################
#############    COMPUTE THE IMPLICATIVE GRAPH    ############
##############################################################

library(stringr)
require(tcltk2) || stop("tcltk support is absent")

#require(FactoClass) || stop("FactoClass package is absent")

#select file
fileName <- tclvalue(tkgetOpenFile())

if (!nchar(fileName)) {
  tkmessageBox(message = "No file was selected!")
  setwd(initDirectory)
} else {
  
  
 
  #transform possible interval variables
  dataCSV = readAndAnalyzeData(dataCSV)
  #prepare data for apriori
  data2transac(dataCSV)
  #call apriori
  callAsirules()
  list.variables=names(dataCSV)
  list.variables=list.variables[-1]
  #list of variables is needed to keep the same order in the variable when the cohesion matrix is built
  implicativeGraph(list.variables)
  

}
