#' @title Displays the toolbar for the implicative graph
#'
#' @description Display the current parameters for the graph: thresholds, checkboxes, colors. 
#' @description When the user press OK, the routine mycallPlot is called to update the graph
#' 
#' @param   frame           frame to display the toolbar, the frame is created first
#' @param   callPlot      routine to compute the graph
#' @param   updatePlot      routine to update the graph
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export


#WARNING  shared variables
#myvalue
#mycbvalue
#mycolor




toolbarGraph <- function (frame,callPlot,updatePlot) {
  
  
  un <<-0
  spin=list()
  cb=list()
  tmp.val=array(0,4)
  
  
  color_well1 <- tkcanvas ( frame , width = 40 , height = 16 ,
                            background = mycolor[[1]] ,
                            highlightbackground = mycolor[[1]] )
  tkgrid(color_well1)
  spin1<-tkwidget(frame,"spinbox", from=50, to=100, increment=1, command=function() changeSpinBox(1),width=3, textvariable=myvalue[[1]])
  tkgrid(spin1)
  cb1 <- tkcheckbutton(frame)
  tkconfigure(cb1,variable=mycbvalue[[1]])
  tkgrid(cb1)
  
  
  
  
  
  
  
  color_well2 <- tkcanvas ( frame , width = 40 , height = 16 ,
                            background = mycolor[[2]] ,
                            highlightbackground = mycolor[[2]] )
  tkgrid(color_well2)
  spin2<-tkwidget(frame, "spinbox",from=50, to=100, increment=1, width=3,command=function() changeSpinBox(2), textvariable=myvalue[[2]])
  tkgrid(spin2)
  cb2 <- tkcheckbutton(frame)
  tkconfigure(cb2,variable=mycbvalue[[2]])
  tkgrid(cb2)
  
  
  
  
  
  
  
  
  
  
  color_well3 <- tkcanvas ( frame , width = 40 , height = 16 ,
                            background = mycolor[[3]] ,
                            highlightbackground = mycolor[[3]] )
  tkgrid(color_well3)
  spin3<-tkwidget(frame, "spinbox",from=50, to=100, increment=1, width=3,command=function() changeSpinBox(3), textvariable=myvalue[[3]])
  tkgrid(spin3)
  cb3 <- tkcheckbutton(frame)
  tkconfigure(cb3,variable=mycbvalue[[3]])
  tkgrid(cb3)
  
  
  
  
  
  
  
  color_well4 <- tkcanvas ( frame , width = 40 , height = 16 ,
                            background = mycolor[[4]] ,
                            highlightbackground = mycolor[[4]] )
  tkgrid(color_well4)
  spin4<-tkwidget(frame, "spinbox",from=50, to=100, increment=1, width=3,command=function() changeSpinBox(4), textvariable=myvalue[[4]])
  tkgrid(spin4)
  cb4 <- tkcheckbutton(frame)
  tkconfigure(cb4,variable=mycbvalue[[4]])
  tkgrid(cb4)
  
  
  
  
  
  
  tkbind ( color_well1 , "<Button -1>" , function (W) {changeColor(W,1)})
  tkbind ( color_well2 , "<Button -1>" , function (W) {changeColor(W,2)})
  tkbind ( color_well3 , "<Button -1>" , function (W) {changeColor(W,3)})
  tkbind ( color_well4 , "<Button -1>" , function (W) {changeColor(W,4)})
  # 
  
  
  changeColor <- function(W,i) {
    mycolor <- tcl ( "tk_chooseColor" , parent = W ,
                   title = "Set box color" )
    mycolor <- tclvalue ( color )
    print ( mycolor )
    if ( nchar ( mycolor ) )
      tkconfigure ( W , background = color )
  }
  
  
  
  
  spin=list(spin1,spin2,spin3,spin4)
  cb=list(cb1,cb2,cb3,cb4)
  
  
  
  OnOK1 <- function()
  {
  
    confidence <<- as.numeric(tclvalue(myvalue1[[1]]))
    callPlot()
    Afficheconf1()
    
    
  }
  dialoguebox <- function () {
    if(un >0 )
   {
      print("je supprime")
      tkdestroy(top1)

   }
   un <<- un+1
    top1 <<- tktoplevel()
    tktitle(top1)<-" confidence"
    myvalue1<<-list(tclVar(80))
    
    
    label.text <- tclVar('choose the confidence value')
    label <- tklabel(top1, text = tclvalue(label.text))
    tkconfigure(label, textvariable = label.text)
    tkgrid(label)
    
    listederoulante<-tkwidget(top1,"spinbox", from=0, to=100, increment=5,width=3, textvariable=myvalue1[[1]])
    bouton1 <- tkbutton(top1,text="OK",command=OnOK1)
    #bouton2 <- tkbutton(top,text="cancel",command=OnOK1)
    tkpack(label,listederoulante,bouton1)
    
    
    #listederoulante<-tkwidget(top,"spinbox", from=50, to=100, increment=5,width=3, textvariable=myvalue[[1]])
  } 
  OnOK <- function()
  {
  
    dialoguebox()

   # callPlot()
    #Afficheconf1()
  
  }
  OK.but <- tkbutton(frame,text="OK",command=OnOK)
  tkgrid(OK.but)
  tkfocus(frame)
  
  
  OnEdit <- function()
  {

    tmp=as.numeric(tclvalue(myedit))
    print("IIIIIIIIIIIIIIIIII")
    print(tmp)
    res=1-tmp
    print(res)
    tclvalue(myedit) <-  res 
    
        
    
    updatePlot()
    Afficheconf1()
    
  }
  

#used to display confidence   
  Afficheconf<- function(){
    
      
    indaff<<-indaff+1
    if(indaff==1)
    {
    affiche <<- TRUE
    }
    else
    {
      if(indaff==2)
      {
        affiche <<- FALSE
        indaff<<-0
      }
    }
    
   
    #for (i in 1:length(var2)) {
      for (i in 1:(num-1)) {
        
      
    # retrieve coordinate of confidence
      
    Xm=coordx1[[i]]
    Ym=coordx2[[i]]
    var=var2[[i]]
    
    if(grepl(affiche, TRUE))
    {
      p1 <- tkcreate(canvas, "text", Xm, Ym, text=var, fill="black",tags="text1")
   
    }
    else
    {
      tkdelete(canvas, "text1")
      
    }

    }

  }
 
  #  is to maintain the confidence by changing settings or switching to edit mode
  Afficheconf1<- function(){
    
   
     
    for (i in 1:(num-1)) {
        
      Xm=coordx1[[i]]
      Ym=coordx2[[i]]
      var=var2[[i]]
      
      if(grepl(affiche, TRUE))
      {
               
        p1 <<- tkcreate(canvas, "text", Xm, Ym, text=var, fill="black",tags="text1")
       # p1 <<- tkcreate(canvas, "text", Xm, Ym, text=var, fill="black",tags="text1")
        
        
      }
      else
      {
        tkdelete(canvas, "text1")
        
      }
      
    }

  }
  
#is used to display the confidence 
  
  
  Edit.but <- tkbutton(frame,text="Edit",command=OnEdit)
  tkgrid(Edit.but)
  
  tkfocus(frame)
  
  
  Affiche.but <- tkbutton(frame,text="Confidence",command=Afficheconf)
  tkgrid(Affiche.but)
  
  tkfocus(frame)
  
  
  
  
  changeSpinBox <- function(spin.nb)  {
   
    
    
    #print(spin.nb)
    for(i in 1:4) {
      tmp.val[i]=as.numeric(tclvalue(myvalue[[i]]))
      #print(tmp.val[i])
    }
    for(i in 1:4)
      if(i>spin.nb)
        if(tmp.val[spin.nb]<tmp.val[i]+i-spin.nb) {
          tmp.val[i]=tmp.val[spin.nb]-(i-spin.nb)
          if(tmp.val[i]<=50)
            tmp.val[i]=50
          tkset(spin[[i]],tmp.val[i])
        }
    for(i in 1:4)
      if(i<spin.nb)
        if(tmp.val[spin.nb]>tmp.val[i]+i-spin.nb) {
          tmp.val[i]=tmp.val[spin.nb]+spin.nb-i
          if(tmp.val[i]>=100)
            tmp.val[i]=100
          tkset(spin[[i]],tmp.val[i])
        }
  }
  
}

