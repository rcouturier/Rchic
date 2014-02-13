##############################################################
#############    COMPUTE THE IMPLICATIVE GRAPH    ############
##############################################################
  
require(tcltk2)

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
  implicativeGraph()
  
}
