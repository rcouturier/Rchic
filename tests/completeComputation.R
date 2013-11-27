##############################################
#############    TO BE CHANGED    ############
##############################################
library(rchic)
#workingDirectory='/home/couturie/Rchic/R'
workingDirectory='C:\\Documents and Settings\\coutu\\Mes documents\\Rchic\\R'


setwd(workingDirectory)


#loading of all functions
source('data2transac.R')
source('callAsirules.R')
source('readRulesAndDisplay.R')
# 
# 
# 
# #read of the file
dataCSV<-read.csv('../tests/animaux.csv',sep=";")
# 
# 
 data2transac(dataCSV)
 callAsirules()
 readRulesAndDisplay()
