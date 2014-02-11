############################################################
#############    COMPUTE THE HIERARCHY TREE    ############
############################################################

HierarchyTree <- function() {
  
  library(rchic)
  library(stringr)
  require(tcltk) || stop("tcltk support is absent")
  
  source('R/data2transac.R')
  source('R/callAsirules.R')
  source('R/readRulesComputeAndDisplayHierarchy.R')
  
  
  
  #select file
  fileName <- tclvalue(tkgetOpenFile())
  
  if (!nchar(fileName)) {
    tkmessageBox(message = "No file was selected!")
    setwd(initDirectory)
  } else {
    
    
    
    #loading of all functions
    
    #read of the file
    dataCSV<-read.csv(fileName,sep=";")
    data2transac(dataCSV)
    callAsirules()
    readRulesComputeAndDisplayHierarchy()
    
  }
}