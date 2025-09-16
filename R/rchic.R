#' @title Runs the rchic 
#'
#' @description Runs the rchic GUI (see the following figure) to be able to select all the options and all the computation.
#' 
#'
#' @details With this GUI, one can select the three main types of computation of rchic: 
#' \itemize{
#'    \item{the similarity tree}
#'    \item{the hierarchy tree}
#'    \item{the implicative graph }
#' }
#' \if{html}{\figure{rchic.png}}
#' \if{latex}{\figure{rchic.png}{options: width=5cm}}
#' 
#' In the option submenu, you can select the option of rchic. Currently you can select the type of the implication between:
#' \itemize{
#'    \item{the classical implication}
#'    \item{the classical implication plus the condifence}
#'    \item{the new computation for large dataset: the implifiance}
#' }
#' 
#' @author Raphael Couturier 
#' @export
#' @docType package
#' @name Rchic






rchic <-function() {
  
  require(tcltk) || stop("tcltk support is absent")
  require(stringr) || stop("stringr support is absent")
  require(tcltk2) || stop("tcltk2 support is absent")
  require(Rgraphviz) || stop("Rgraphviz support is absent")
  
  
  
  modalDialog <- function(title, question, entryInit, implicationType,completeGraph,verbose,entryWidth = 20,
                          returnValOnCancel = "ID_CANCEL") {
    dlg <- tktoplevel()
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    tkwm.title(dlg, title)
    textEntryVarTcl <- tclVar(paste(entryInit))
    
    
    rb1 <- tkradiobutton(dlg)
    rb2 <- tkradiobutton(dlg)
    rb3 <- tkradiobutton(dlg)
    rb4 <- tkradiobutton(dlg)
    
    cb1 <- tkcheckbutton(dlg)
    
    cb2 <- tkcheckbutton(dlg)
    
    
    
    rc1 <- tkradiobutton(dlg)
    
    
    rbValueTcl <- tclVar(paste(implicationType))
    cbValueTcl <- tclVar(paste(completeGraph))
    cb2ValueTcl <- tclVar(paste(verbose))
    
    
    tkconfigure(rb1,variable=rbValueTcl,value="Classic")
    tkconfigure(rb2,variable=rbValueTcl,value="Classic+Confidence")
    tkconfigure(rb3,variable=rbValueTcl,value="Implifiance")
    tkconfigure(rb4,variable=rbValueTcl,value="Entropic")
    
    tkconfigure(cb1,variable=cbValueTcl)
    tkgrid(tklabel(dlg,text="Complete graph"),cb1)
    
    
    tkconfigure(cb2,variable=cb2ValueTcl)
    tkgrid(tklabel(dlg,text="Verbose"),cb2)
    
    
    
    tkgrid(tklabel(dlg,text="Classic "),rb1)
    tkgrid(tklabel(dlg,text="Classic + confidence "),rb2)
    tkgrid(tklabel(dlg,text="Implifiance "),rb3)
    tkgrid(tklabel(dlg,text="Entropic "),rb4)
    
    
    ReturnVal <- returnValOnCancel
    
    onOK <- function() {
      ReturnVal <<- paste(tclvalue(rbValueTcl),tclvalue(cbValueTcl),tclvalue(cb2ValueTcl))
      tkgrab.release(dlg)
      tkdestroy(dlg)
      tkfocus(tt)
    }
    onCancel <- function() {
      ReturnVal <<- returnValOnCancel
      tkgrab.release(dlg)
      tkdestroy(dlg)
      tkfocus(tt)
    }
    OK.but <- tkbutton(dlg, text = "   OK   ", command = onOK)
    Cancel.but <- tkbutton(dlg, text = " Cancel ", command = onCancel)
    tkgrid(OK.but, Cancel.but)
    tkgrid(tklabel(dlg, text = "    "))
    
    tkfocus(dlg)
    tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg); tkfocus(tt)})
    
    tkwait.window(dlg)
    
    return(ReturnVal)
  }
  
  
  
  
  tt <- tktoplevel()
  topMenu <- tkmenu(tt)
  tkwm.title( tt , " RCHIC " )
  tkconfigure(tt, menu = topMenu)
  fileMenu <- tkmenu(topMenu, tearoff = FALSE)
  optionMenu <- tkmenu(topMenu, tearoff = FALSE)
  
  tkadd(fileMenu, "command", label = "Similarity tree", command = function() {
    #select file
    fileName <- tclvalue(tkgetOpenFile())
    
    if (!nchar(fileName)) {
      tkmessageBox(message = "No file was selected!")
      setwd(initDirectory)
    } else {
      
      
      
      #retrieve option
      my.option=getMyOption()
      #compute the computing mode
      computing.mode=1
      if(my.option[1,2]=="Classic+Confidence")
        computing.mode=2
      if(my.option[1,2]=="Implifiance")
        computing.mode=3
      verbose=as.numeric(my.option[3,2])
      if(verbose==1)
        verbose=TRUE
      else
        verbose=FALSE
      
      
      callSimilarityTree(fileName=fileName,contribution.supp=TRUE,typicality.supp=FALSE,verbose=verbose)
      
    }
  })
  tkadd(fileMenu, "command", label = "Hierarchy tree", command = function(){
    
    #select file
    fileName <- tclvalue(tkgetOpenFile())
    
    if (!nchar(fileName)) {
      tkmessageBox(message = "No file was selected!")
      setwd(initDirectory)
    } else{
      
      #retrieve option
      my.option=getMyOption()
      #compute the computing mode
      computing.mode=1
      if(my.option[1,2]=="Classic+Confidence")
        computing.mode=2
      if(my.option[1,2]=="Implifiance")
        computing.mode=3
      print("computing mode")
      print(computing.mode)
      
      verbose=as.numeric(my.option[3,2])
      if(verbose==1)
        verbose=TRUE
      else
        verbose=FALSE
      
      
      callHierarchyTree(fileName=fileName,contribution.supp=TRUE,typicality.supp=FALSE,computing.mode=computing.mode,verbose=verbose)
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
      
      #retrieve option
      my.option=getMyOption()
      #compute the computing mode
      computing.mode=1
      if(my.option[1,2]=="Classic+Confidence")
        computing.mode=2
      if(my.option[1,2]=="Implifiance")
        computing.mode=3
      if(my.option[1,2]=="Entropic")
        computing.mode=4
      
      complete.graph=as.numeric(my.option[2,2])
      
      print("computing mode")
      print(computing.mode)
      print("complete graph")
      print(complete.graph)
      
      #list of variables is needed to keep the same order in the variable when the cohesion matrix is built
      implicativeGraph(fileName,list.variables,computing.mode=computing.mode,complete.graph=complete.graph)
      
      
    }
    
    
  })
  
  tkadd(topMenu, "cascade", label = "Rchic", menu = fileMenu)
  
  tkadd(optionMenu, "command", label = "Option", command = function() {
    
    
    my.option=getMyOption()
    
    ReturnVal <- modalDialog("First Name Entry", "Enter Your First Name", "toto", my.option[1,2],my.option[2,2],my.option[3,2])
    if (ReturnVal == "ID_CANCEL") return()
    #tkmessageBox(title = "Greeting",
    #             message = paste("Hello, ", ReturnVal, sep = ""))
    print(my.option)
    val=strsplit(ReturnVal, " ")[[1]]
    print("ici")
    print(val)
    
    print(val[1])
    my.option[1,2]=val[1]
    
    print(val[2])
    my.option[2,2]=val[2]
    
    print(val[3])
    my.option[3,2]=val[3]
    
    
    write.table(my.option,"option.csv",col.names = FALSE,row.names = FALSE,sep=",")
  })
  
  tkadd(topMenu, "cascade", label = "Option", menu = optionMenu)
  tkfocus(tt)
  
}