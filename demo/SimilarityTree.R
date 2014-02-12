############################################################
#############    COMPUTE THE SIMILARITY TREE    ############
############################################################

SimilarityTree <- function() {
  
  library(rchic)
  library(stringr)
  require(tcltk) || stop("tcltk support is absent")
  
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
    readRulesComputeAndDisplaySimilarities()
    
  }
}