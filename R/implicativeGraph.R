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
  
  plotFont <<- "Helvetica 12"
  
  
  
  toolbarItem(list.variables,list.tcl,callPlotImplicativeGraph)  
  
  
  callPlotImplicativeGraph()
}


callPlotImplicativeGraph <- function() {
  
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







plotImplicativeGraph <- function(thres=99,value,cbvalue,color,list.selected.item) {
  
  
  
  rules<-read.table(file='transaction.out',header=TRUE,row.names=1,sep=',',stringsAsFactors = FALSE)
  row=row.names(rules)
  rules=as.data.frame(lapply(rules,as.numeric))
  row.names(rules)=row
  n=dim(rules)[1]
  
  listNodes=strsplit(row.names(rules),split=' -> ')
  
  #determine the list of visible nodes
  lNodes=character(0)
  for(i in 1:n) {
    from=listNodes[[i]][1]
    to=listNodes[[i]][2]
    if(rules[i,5]>thres &  rules[i,1]<rules[i,2] & as.numeric(list.selected.item[[which(list.variables==from)]]) & as.numeric(list.selected.item[[which(list.variables==to)]])) {
      
      lNodes=c(lNodes,from,to)
    }
  }
  lNodes=unique(lNodes)
  
  #create the graph with the nodes
  g1 <- new("graphNEL", nodes = lNodes,edgemode = "directed")
  
  #add the edge of the graph
  for(i in 1:n) {
    rule=strsplit(row.names(rules)[i],split=' -> ')
    from=rule[[1]][1]
    to=rule[[1]][2]
    if(rules[i,5]>thres &  rules[i,1]<rules[i,2] & 
         as.numeric(list.selected.item[[which(list.variables==from)]]) & as.numeric(list.selected.item[[which(list.variables==to)]])) {
      g1 <- addEdge(from,to,g1)
      #print(paste("put ",from,"->",to))
    }
  }
  
  #no need to plot thegraph
  #plot(g1)
  
  #call rgraphviz to draw a nice graph
  graph1 <- agopen(g1,"foo")
  
  
  offsetX=40
  
  scalingFactorX=1.9
  scalingFactorY=0.5
  size.x=slot(slot(boundBox(graph1),'upRight'),'x')
  size.y=slot(slot(boundBox(graph1),'upRight'),'y')
  workingHeight=size.y*scalingFactorY+10
  workingWidth=size.x*scalingFactorX+offsetX
  
  tkconfigure(canvas, scrollregion=c(0,0,workingWidth,workingHeight))
  
  ### WARNING: all tk objects MUST have the draw tag !!!
  tkdelete(canvas, "draw")
  
  #get the list of nodes
  nodes = AgNode(graph1)
  
  lastX=0
  lastY=0
  
  list.spline <<- list()
  list.node <- list()
  r=3
  
  moveDown <- function(i) {
    force(i)
    
    function(x,y){
      
      ## This procedure is invoked when the mouse is pressed over one
      ## of the data points.  It sets up state to allow the point
      ## to be dragged.
      ##
      ## Arguments:
      ## x, y -  The coordinates of the mouse press.
      print("movedown")
      print(x)
      print(y)
      x <- as.numeric(x)
      y <- as.numeric(y)
      tkdtag(canvas, "selected")
      tkaddtag(canvas, "selected", "withtag", "current")
      tkitemraise(canvas,"current")
      lastX <<- x
      lastY <<- y
    }
  }
  
  moveNode <- function(i)
  {
    force(i)
    
    function(x,y){
      
      ## This procedure is invoked during mouse motion events.
      ## It drags the current item.
      ##
      ## Arguments:
      ## x, y -  The coordinates of the mouse.
      print("movenode")
      print(x)
      print(y)
      print(lastX)
      print(lastY)
      x <- as.numeric(x)
      y <- as.numeric(y)
      tkmove(canvas, "selected", x-lastX,y-lastY)
      lastX <<- x
      lastY <<- y
    }
  }
  
  moveEdges <- function(i){
    force(i)
    function(x,y) {
      print("move edge3")
      #print("from")
      #print(list.from[[i]])
      for(e in list.from[[i]]) {
        sp=list.spline[[e]]
        sp$coord[[1]]=as.numeric(x)
        sp$coord[[2]]=as.numeric(y)+15
        print(list.spline)
        list.spline[[e]]<<-sp
        print(list.spline)
        tkcoords(canvas,sp$spline,sp$coord)
      }
      
      
      #print("to")
      #print(list.to[[i]])
      for(e in list.to[[i]]) {
        sp=list.spline[[e]]
        sp$coord[[7]]=as.numeric(x)
        sp$coord[[8]]=as.numeric(y)-15
        print(list.spline)
        list.spline[[e]]<<-sp
        print(list.spline)
        tkcoords(canvas,sp$spline,sp$coord)
      }
    }
  }
  
  
  
  control_point_selected <- function (i) {
    force(i)
    
    function(x,y){
      print("select")
      print(i)
      tkdtag(canvas, "selected")
      tkaddtag(canvas, "selected", "withtag", "current")
      tkitemraise(canvas,"current")
      #tkaddtag ( canvas , "selected" , "withtag" , "current" )
      #tkitemraise ( canvas , "current" )
      last_pos <<- as.numeric ( c ( x , y ) )
    }
  }
  
  control_point_move <- function ( i ) {
    force(i)
    function(x,y) {
      pos <- as.numeric ( c ( x , y ) )
      tkmove ( canvas , "selected" , pos [ 1 ] - last_pos [ 1 ] ,
               pos [ 2 ] - last_pos [ 2 ] )
      last_pos <<- pos
    }
  }
  
  control_point_release <- function ( spline,pos) {
    force(spline)
    function(x,y){
      print("release")
      
      print(spline) 
      print(pos)
      sp=list.spline[[spline]]
      if(pos==2) {
        sp$coord[[3]]=as.numeric(x)
        sp$coord[[4]]=as.numeric(y)
      }
      if(pos==3) {
        sp$coord[[5]]=as.numeric(x)
        sp$coord[[6]]=as.numeric(y)
      }
      list.spline[[spline]]<<-sp
      tkcoords(canvas,sp$spline,sp$coord)
      #tkdtag ( W , "selected" )
     
    }
  }
  
  last_pos <<- numeric( 2 )
  
  createPoint <- function(nb,x,y,r,spline,pos) {
    p <- tkcreate ( canvas , "oval" , x - r , y - r , x + r , y + r ,
                       width = 1 , outline = "black" , fill = "blue",tags="draw" )
    #tkaddtag ( canvas , "point2" , "withtag" , item )
    
    
    
    # global to track position
     tkitembind ( canvas , p , "<1>" , control_point_selected(nb) )
     tkitembind ( canvas , p,"<B1-Motion>" , control_point_move(nb) )
     tkitembind ( canvas , p, "<ButtonRelease-1>" , control_point_release(spline,pos))
    print("spline")
    print(spline)
    print(nb)
    #tkbind ( canvas , "<ButtonRelease-1>" , function ( W ) tkdtag ( W , "selected" ))
    
    
    
  }
  
  
  
  #nodeItem <-  vector("character",length(nodes))
  
  
  
  
  nb.spline=1
  nb.control_point=1
  
  
  #if the list of nodes is not empty
  if(length(nodes)>0) {
    #create the text of the nodes
    for (i in 1:length(nodes)) {
      node=nodes[[i]]
      coord=getNodeXY(node)
      name=name(node)
      p<-tkcreate(canvas, "text", offsetX+coord$x*scalingFactorX, workingHeight+0-coord$y*scalingFactorY, text=name,font=plotFont, fill="brown",tags="draw")
      
      tkitembind(canvas, p, "<1>", moveDown(i))
      tkitembind(canvas, p,"<B1-Motion>", moveNode(i))
      tkitembind(canvas, p, "<ButtonRelease-1>",moveEdges(i))
    
      list.node=c(list.node,name)
    }
    
    list.from = vector("list",length(nodes))  
    list.to = vector("list",length(nodes))  
    
    #get the list of the edges
    edges = AgEdge(graph1)
    #for all edges
    for (i in 1:length(edges)) {
      edge=edges[[i]]
      #for all splines
      print(paste("edge",tail(edge),"->",head(edge)))
      
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
          lCoord[2*k-1]=offsetX+slot(cPoints(coord)[[k]],'x')*scalingFactorX
          lCoord[2*k]=workingHeight+0-slot(cPoints(coord)[[k]],'y')*scalingFactorY
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
        sp=tkcreate(canvas, "line", lCoord,width=2,arrow=arrow,smooth='bezier',splinesteps=6,tags="draw",fill=col)
        
        from=which(list.node==tail(edge))
        to=which(list.node==head(edge))
        
        list.spline <<- c(list.spline,list(list(spline=sp,coord=lCoord,from=from,to=to)))
        
        
        
        if(j==1) {              # from node
          list.from[[from]]=c(list.from[[from]],nb.spline)
          createPoint(nb=nb.control_point,x=lCoord[3],y=lCoord[4],r=r,spline=nb.spline,pos=2)
          nb.control_point=nb.control_point+1
          
        }
        if(j==numSplines(edge)){ # to node
          list.to[[to]]=c(list.to[[to]],nb.spline)
          createPoint(nb=nb.control_point,x=lCoord[5],y=lCoord[6],r=r,spline=nb.spline,pos=3)
          nb.control_point=nb.control_point+1
        }
        nb.spline=nb.spline+1
      }
    }
  }
  
    
  #write the current postscript image in the current directory
  tkpostscript(canvas, file="graph.ps",height=workingHeight,width=workingWidth)
  
  
  
  
}
