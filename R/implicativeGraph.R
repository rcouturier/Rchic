#' @title Displays the Implicative Graph.
#'
#' @description Reads the ASI rules, selects the rules according to the toolbar and calls rgraphviz before displaying the rules.
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export

implicativeGraph <-function() {
  


  visibleWidth<<-1200
  visibleHeight<<-800
  
  workingWidth<<-1200
  workingHeight<<-800
  
  top <- tktoplevel()
  
  
  
  tt <- ttkframe ( top )#, padding = 0)
  toolbar <- ttkframe ( top )#, padding = 0 )
  
  tkgrid(toolbar,row = 0 , column = 0, sticky = "news")
  tkgrid(tt,row = 0 , column = 1 , sticky = "news")
  
  tkgrid.columnconfigure (top , 1 , weight = 1 )
  tkgrid.rowconfigure (top , 0 , weight = 1 )
  
  
  
  canvas <<- tkcanvas(tt, relief="raised", width=visibleWidth, height=visibleHeight,
                      xscrollcommand=function(...)tkset(xscr,...), 
                      yscrollcommand=function(...)tkset(yscr,...), 
                      scrollregion=c(0,0,workingWidth,workingHeight))
  xscr <<- tkscrollbar(tt, orient="horizontal",
                      command=function(...)tkyview(canvas,...))
  
  yscr <<- tkscrollbar(tt, orient="vertical",
                      command=function(...)tkyview(canvas,...))
  
  
  
  
  value=list(tclVar(99),tclVar(95),tclVar(90),tclVar(85))
  
  cbvalue=list(tclVar(1),tclVar(0),tclVar(0),tclVar(0))
  
  color=list("#FF0000","#00FF00","#0000FF","#00FFFF")
  
  toolbarGraph(toolbar,value,cbvalue,color)
  
  
  
  tkconfigure(xscr, command = function(...) tkxview(canvas, ...))
  tkconfigure(yscr, command = function(...) tkyview(canvas, ...))
  #tkconfigure(canvas, xscrollcommand = function(...) tkset(xscr, ...))
  #tkconfigure(canvas, yscrollcommand = function(...) tkset(yscr, ...))
  
  tkpack(xscr, side = "bottom", fill = "x")
  tkpack(yscr, side = "right", fill = "y")
  tkpack(canvas, side = "left", fill="both", expand=1)
  
  plotFont <<- "Helvetica 8"
  
  
  
  
  
  myreplot(99,sapply(value,tclvalue),sapply(cbvalue,tclvalue),color) 
  
  
}







toolbarGraph <- function (frame,value,cbvalue,color) {
  
  spin=list()
  cb=list()
  tmp.val=array(0,4)
  
  
  color_well1 <- tkcanvas ( frame , width = 40 , height = 16 ,
                            background = color[[1]] ,
                            highlightbackground = color[[1]] )
  tkgrid(color_well1)
  spin1<-tkwidget(frame,"spinbox", from=50, to=100, increment=1, command=function() changeSpinBox(1),width=3, textvariable=value[[1]])
  tkgrid(spin1)
  cb1 <- tkcheckbutton(frame)
  tkconfigure(cb1,variable=cbvalue[[1]])
  tkgrid(cb1)
  
  
  
  
  
  
  
  color_well2 <- tkcanvas ( frame , width = 40 , height = 16 ,
                            background = color[[2]] ,
                            highlightbackground = color[[2]] )
  tkgrid(color_well2)
  spin2<-tkwidget(frame, "spinbox",from=50, to=100, increment=1, width=3,command=function() changeSpinBox(2), textvariable=value[[2]])
  tkgrid(spin2)
  cb2 <- tkcheckbutton(frame)
  tkconfigure(cb2,variable=cbvalue[[2]])
  tkgrid(cb2)
  
  
  
  
  
  
  
  
  
  
  color_well3 <- tkcanvas ( frame , width = 40 , height = 16 ,
                            background = color[[3]] ,
                            highlightbackground = color[[3]] )
  tkgrid(color_well3)
  spin3<-tkwidget(frame, "spinbox",from=50, to=100, increment=1, width=3,command=function() changeSpinBox(3), textvariable=value[[3]])
  tkgrid(spin3)
  cb3 <- tkcheckbutton(frame)
  tkconfigure(cb3,variable=cbvalue[[3]])
  tkgrid(cb3)
  
  
  
  
  
  
  
  color_well4 <- tkcanvas ( frame , width = 40 , height = 16 ,
                            background = color[[4]] ,
                            highlightbackground = color[[4]] )
  tkgrid(color_well4)
  spin4<-tkwidget(frame, "spinbox",from=50, to=100, increment=1, width=3,command=function() changeSpinBox(4), textvariable=value[[4]])
  tkgrid(spin4)
  cb4 <- tkcheckbutton(frame)
  tkconfigure(cb4,variable=cbvalue[[4]])
  tkgrid(cb4)
  
  
  
  
  
  
  tkbind ( color_well1 , "<Button -1>" , function (W) {changeColor(W,1)})
  tkbind ( color_well2 , "<Button -1>" , function (W) {changeColor(W,2)})
  tkbind ( color_well3 , "<Button -1>" , function (W) {changeColor(W,3)})
  tkbind ( color_well4 , "<Button -1>" , function (W) {changeColor(W,4)})
  # 
  
  
  changeColor <- function(W,i) {
    color <- tcl ( "tk_chooseColor" , parent = W ,
                   title = "Set box color" )
    color <- tclvalue ( color )
    print ( color )
    if ( nchar ( color ) )
      tkconfigure ( W , background = color )
  }
  
  
  
  
  spin=list(spin1,spin2,spin3,spin4)
  cb=list(cb1,cb2,cb3,cb4)
  
  OnOK <- function()
  {
    threshold=100
    for(i in 1:4) {
      check <- as.numeric(tclvalue(cbvalue[[i]]))
      print(check)
      val=as.numeric(tclvalue(value[[i]]))
      print(val)
      if(check)
        threshold=val
    }
    print("threshold")
    print(threshold)
    myreplot(threshold,sapply(value,tclvalue),sapply(cbvalue,tclvalue),color)
  }
  OK.but <- tkbutton(frame,text="OK",command=OnOK)
  tkgrid(OK.but)
  tkfocus(frame)
  
  
  
  
  
  
  
  
  changeSpinBox <- function(spin.nb)  {
    #print(spin.nb)
    for(i in 1:4) {
      tmp.val[i]=as.numeric(tclvalue(value[[i]]))
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






myreplot <- function(threshold=99,value,cbvalue,color) {
  
  
  
  rules<-read.table(file='transaction.out',header=TRUE,row.names=1,sep=',')
  n=dim(rules)[1]
  
  listNodes=strsplit(row.names(rules),split=' -> ')
  
  lNodes=character(0)
  for(i in 1:n) {
    
    if(rules[i,5]>threshold) {
      from=listNodes[[i]][1]
      to=listNodes[[i]][2]
      lNodes=c(lNodes,from,to)
    }
  }
  lNodes=unique(lNodes)
    
  g1 <- new("graphNEL", nodes = lNodes,edgemode = "directed")
  
  #for Pablo: try to use graphAM class
  for(i in 1:n) {
    rule=strsplit(row.names(rules)[i],split=' -> ')
    from=rule[[1]][1]
    to=rule[[1]][2]
    if(rules[i,1]<rules[i,2] & rules[i,5]>threshold) {
      g1 <- addEdge(from,to,g1)
    }
  }
  
  #no need to plot thegraph
  #  plot(g1)
  
  
  graph1 <- agopen(g1,"foo")
  
  
  offsetX=40
  
  scalingFactor=0.6
  size.x=slot(slot(boundBox(graph1),'upRight'),'x')
  size.y=slot(slot(boundBox(graph1),'upRight'),'y')
  workingHeight=size.y*scalingFactor+10
  workingWidth=size.x*scalingFactor+offsetX
  
  tkconfigure(canvas, scrollregion=c(0,0,workingWidth,workingHeight))
  
  tkdelete(canvas, "draw")
  
  
  nodes = AgNode(graph1)
  
  
  
  
  
  if(length(nodes)>0) {
    for (i in 1:length(nodes)) {
      node=nodes[[i]]
      coord=getNodeXY(node)
      name=name(node)
      tkcreate(canvas, "text", offsetX+coord$x*scalingFactor, workingHeight+0-coord$y*scalingFactor, text=name,font=plotFont, fill="brown",tags="draw")
    }
    #print (graph1$size)
    
    
    edges = AgEdge(graph1)
    for (i in 1:length(edges)) {
      edge=edges[[i]]
      for (j in 1:numSplines(edge)) {
        coord=getSpline(edge, j)
        if(j==numSplines(edge)) {
          arrow='last'
        }
        else {
          arrow='none'
        }
        lCoord=numeric(8)
        for(k in 1:4) {
          lCoord[2*k-1]=offsetX+slot(cPoints(coord)[[k]],'x')*scalingFactor
          lCoord[2*k]=workingHeight+0-slot(cPoints(coord)[[k]],'y')*scalingFactor
        }
        val=rules[paste(tail(edge),"->",head(edge)),5]
        print(val)
        col="black"
        if(cbvalue[[1]]==1 && value[[1]]<val)
          col=color[[1]]
        else
          if(cbvalue[[2]]==1 && value[[2]]<val)
            col=color[[2]]
          else
            if(cbvalue[[3]]==1 && value[[3]]<val)
              col=color[[3]]
            else
              if(cbvalue[[4]]==1 && value[[4]]<val)
                col=color[[4]]
        tkcreate(canvas, "line", lCoord,width=2,arrow=arrow,smooth='bezier',splinesteps=10,tags="draw",fill=col)
      }
    }
  }
  
  #write the current postscript image in the current directory
  tkpostscript(canvas, file="graph.ps",height=workingHeight,width=workingWidth)
}

