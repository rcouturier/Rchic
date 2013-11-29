##############################################
#############    TO BE CHANGED    ############
##############################################
library(rchic)
require(tcltk)

#workingDirectory='/home/couturie/Rchic/R'
#workingDirectory='C:\\Documents and Settings\\coutu\\Mes documents\\Rchic\\R'
#initDirectory=getwd()
#workingDirectory= paste(getwd(),'/R',sep="")
#setwd(workingDirectory)
source('R/data2transac.R')
source('R/callAsirules.R')
source('R/readRulesAndDisplay.R')
#setwd(initDirectory) 


fileName <- tclvalue(tkgetOpenFile()) # Very simple, isn't it?

if (!nchar(fileName)) {
  tkmessageBox(message = "No file was selected!")
  setwd(initDirectory)
} else {
  
  #tkmessageBox(message = paste("The file selected was", dirname(fileName)))
  #tkmessageBox(message = paste("The file selected was", basename(fileName)))
  
  
  #loading of all functions
  
  # 
  # 
  # 
  # #read of the file
  dataCSV<-read.csv(fileName,sep=";")
  # 
  # 
  data2transac(dataCSV)
  callAsirules()
  readRulesAndDisplay()
   
}

