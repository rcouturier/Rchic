##############################################
#############    TO BE CHANGED    ############
##############################################
library(rchic)
require(tcltk)

workingDirectory='/home/couturie/Rchic/R'
#workingDirectory='C:\\Documents and Settings\\coutu\\Mes documents\\Rchic\\R'


setwd(workingDirectory)


fileName <- tclvalue(tkgetOpenFile()) # Very simple, isn't it?

if (!nchar(fileName)) {
  tkmessageBox(message = "No file was selected!")
} else {
  
  #tkmessageBox(message = paste("The file selected was", dirname(fileName)))
  #tkmessageBox(message = paste("The file selected was", basename(fileName)))
  
  
  #loading of all functions
  source('data2transac.R')
  source('callAsirules.R')
  source('readRulesAndDisplay.R')
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

