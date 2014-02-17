#' @title Displays the Implicative Graph.
#'
#' @description Reads the ASI rules, selects the rules according to the toolbar and calls rgraphviz before displaying the rules.
#' 
#' @param   rules           dataframe of ASI rules.
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export

implicativeGraph <-function(list.variables) {
  


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
  
  
  
  
  
  #initial value (global variables)
  myvalue<<-list(tclVar(99),tclVar(95),tclVar(90),tclVar(85))
  mycbvalue<<-list(tclVar(1),tclVar(0),tclVar(0),tclVar(0))
  mycolor<<-list("#FF0000","#00FF00","#0000FF","#00FFFF")
  mythreshold<<-tclVar(99)
  
  #currently we consider that all items are selected
  list.selected.item=rep_len(T,length(list.variables))
  list.tcl<<-lapply(list.selected.item,function(i) tclVar(i))
  
  toolbarGraph(toolbar,callPlotImplicativeGraph)
  
  
  
  
  
  tkconfigure(xscr, command = function(...) tkxview(canvas, ...))
  tkconfigure(yscr, command = function(...) tkyview(canvas, ...))
  #tkconfigure(canvas, xscrollcommand = function(...) tkset(xscr, ...))
  #tkconfigure(canvas, yscrollcommand = function(...) tkset(yscr, ...))
  
  tkpack(xscr, side = "bottom", fill = "x")
  tkpack(yscr, side = "right", fill = "y")
  tkpack(canvas, side = "left", fill="both", expand=1)
  
  plotFont <<- "Helvetica 8"
  
  
  
  toolbarItem(list.variables,list.tcl,callPlotImplicativeGraph)  
  
  
  callPlotImplicativeGraph()
}


callPlotImplicativeGraph <- function() {
  print("ICI")
  #currently we consider that all items are selected
  #list.selected.item=rep_len(T,length(list.variables))
  
  #need to check these parameters here
  thres=100
  for(i in 1:4) {
    check <- as.numeric(tclvalue(mycbvalue[[i]]))
    print(check)
    val=as.numeric(tclvalue(myvalue[[i]]))
    print(val)
    if(check)
      thres=val
  }
  print("threshold")
  print(thres)
  #myvalue<<-sapply(value,tclvalue)
  #mycbvalue<<-sapply(mycbvalue,tclvalue)
  #tclvalue(mythreshold)<<-thres
  print(sapply(mycbvalue,tclvalue))
  
  list.selected.item=lapply(list.tcl,function(i) tclvalue(i))
  print(list.selected.item)
  plotImplicativeGraph(thres,sapply(myvalue,tclvalue),sapply(mycbvalue,tclvalue),mycolor,list.selected.item) 
}







plotImplicativeGraph <- function(threshold=99,value,cbvalue,color,list.selected.item) {
  
  
  
  rules<-read.table(file='transaction.out',header=TRUE,row.names=1,sep=',')
  n=dim(rules)[1]
  
  listNodes=strsplit(row.names(rules),split=' -> ')
  
  lNodes=character(0)
  for(i in 1:n) {
    
    if(rules[i,5]>threshold & as.numeric(list.selected.item[[rules[i,1]]]) & as.numeric(list.selected.item[[rules[i,2]]])) {
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
    if(rules[i,1]<rules[i,2] & as.numeric(list.selected.item[[rules[i,1]]]) & as.numeric(list.selected.item[[rules[i,2]]]) & rules[i,5]>threshold) {
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

