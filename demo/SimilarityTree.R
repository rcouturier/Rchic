############################################################
#############    COMPUTE THE SIMILARITY TREE    ############
############################################################

require(stringr) || stop("stringr support is absent")
require(tcltk2) || stop("tcltk2 support is absent")

#select file
fileName <- tclvalue(tkgetOpenFile())

if (!nchar(fileName)) {
  tkmessageBox(message = "No file was selected!")
  setwd(initDirectory)
} else {
  
  
  callSimilarityTree(filename=fileName,contribution.supp=TRUE,typicality.supp=FALSE,verbose=FALSE)
  
}