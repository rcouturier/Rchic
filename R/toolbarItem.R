#' @title Displays the toolbar so select the items to display
#'
#' @description Display the current list of selected items. F7 and F8 can be used to select or deselect all the items respectively
#' @description When the user press OK, the routine mycallPlot is called to update the graph or the tree
#' @description The toolbar is created inside another frame
#' 
#' @param   list.variables     list of the name of the variables
#' @param   list.tcl           list of the selected items in the tcl format
#' @param   mycallPlot         routine to call to update the graph or the tree
#' 
#' @author Raphael Couturier 
#' @export


toolbarItem <- function(list.variables,list.tcl,mycallPlot) {
  
  
  
  frame <- tktoplevel ( )
  
  
  
  
  max.length=max(str_length(list.variables))
  
  #list.selected.item=sample(0:1,size=length(list.variables),replace=T)
  #list.tcl=lapply(list.selected.item,function(i) tclVar(i))
  
  list.buttons = lapply(list.variables,function(i) ttkbutton(frame,text=i,width=max.length))     #,state="disabled"
  
  #lapply(seq_along(list.buttons),function(i) {tkgrid(list.buttons[[i]],row=floor((i-1)/10),column=(i-1)%%10) })
  lapply(seq_along(list.buttons),function(i) {tkgrid(list.buttons[[i]],column=floor((i-1)/30),row=(i-1)%%30) })
  
  
  
  actionOK <- function() {
    print("OK")  
  }
  
  OnOK <- function()
  {
    print("OK")
    #list.selected.item=lapply(list.tcl,function(i) tclvalue(i))
    #print(list.selected.item)
    mycallPlot()
    
  }
  OK.but <- tkbutton(frame,text="OK",command=OnOK)
  tkgrid(OK.but)
  
  
  initState <- function(but,i) {
    if (as.numeric(tclvalue(list.tcl[[i]])))
      tkconfigure(but, state = "normal")
    else
      tkconfigure(but, state = "disabled")
    
  }
  
  lapply(seq_along(list.buttons),function(i) initState(list.buttons[[i]],i))
  
  
  
  
  changeState <- function(W,i) {
    val=as.numeric(tclvalue(list.tcl[[i]]))
    if(val) {
      tkconfigure(W, state = "disabled")
    }
    else {
      tkconfigure(W, state = "normal")
    }
    tclvalue(list.tcl[[i]])=1-val
  }
  
  lapply(seq_along(list.buttons),function(i) tkbind (list.buttons[[i]] , "<Button -1>" , function (W) {changeState(W,i)}))
  
  
  
  selectAll <- function(select) {
    print("inside")
    if(select) {
      lapply(seq_along(list.buttons),function(i) tkconfigure(list.buttons[[i]], state="normal"))
      lapply(list.tcl,function(i) tclvalue(i)=1)
    }
    else {
      lapply(seq_along(list.buttons),function(i) tkconfigure(list.buttons[[i]], state="disabled"))
      lapply(list.tcl,function(i) tclvalue(i)=0)
    }
  }
  
  tkbind ( frame , "<F7>" , function() selectAll(1) )
  tkbind ( frame , "<F8>" , function() selectAll(0) )
  
}