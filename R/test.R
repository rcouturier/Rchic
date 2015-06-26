require(tcltk) || stop("tcltk support is absent")
require(stringr) || stop("stringr support is absent")
require(tcltk2) || stop("tcltk2 support is absent")


 
tt <- tktoplevel()
topMenu <- tkmenu(tt)
tkconfigure(tt, menu = topMenu)
fileMenu <- tkmenu(topMenu, tearoff = FALSE)
tkadd(fileMenu, "command", label = "Similarity tree", command = function() {
  #select file
  fileName <- tclvalue(tkgetOpenFile())
  
  if (!nchar(fileName)) {
    tkmessageBox(message = "No file was selected!")
    setwd(initDirectory)
  } else {
    
    
    callSimilarityTree(fileName=fileName,contribution.supp=TRUE,typicality.supp=FALSE,verbose=FALSE)
    
  }
})
tkadd(fileMenu, "command", label = "Hierarchy tree", command = function(){
  
  #select file
  fileName <- tclvalue(tkgetOpenFile())
  
  if (!nchar(fileName)) {
    tkmessageBox(message = "No file was selected!")
    setwd(initDirectory)
  } else{
    
    
    callHierarchyTree(fileName=fileName,contribution.supp=TRUE,typicality.supp=FALSE,verbose=FALSE)
  }
})
tkadd(fileMenu, "command", label = "Implicative graph", command = function(){
  
  #select file
  fileName <- tclvalue(tkgetOpenFile())
  
  if (!nchar(fileName)) {
    tkmessageBox(message = "No file was selected!")
    setwd(initDirectory)
  } else {
    
    # to display confidence
    affiche<<-FALSE
    indaff <<- 0
    
    #transform possible interval variables
    result = readAndAnalyzeData(fileName = fileName)
    dataCSV=result[[1]]
    supplementary.variable=result[[2]]
    #prepare data for apriori
    data2transac(dataCSV)
    #call apriori
    callAsirules()
    list.variables=names(dataCSV)
    list.variables=list.variables[-1]
    #list of variables is needed to keep the same order in the variable when the cohesion matrix is built
    implicativeGraph(list.variables)
    
    
  }
  
  
})

tkadd(topMenu, "cascade", label = "Rchic", menu = fileMenu)
tkfocus(tt)