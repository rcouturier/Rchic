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
    #print(check)
    val=as.numeric(tclvalue(myvalue[[i]]))
    #print(val)
    if(check)
      thres=val
  }
  #print("threshold")
  #print(thres)
  
  #print(sapply(mycbvalue,tclvalue))
  
  list.selected.item=lapply(list.tcl,function(i) tclvalue(i))
  #print(list.selected.item)
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
  
  #coordinates to compute the delta of a move from the last mouse move
  lastX=0
  lastY=0
  #coordinates to compute the delta of a move from the initial button pressed
  initX=0
  initY=0
  
  list.spline <<- list()
  list.node <- list()
  
  #size of the controle nodes
  r=5

  #mouse pressed on text item
  text_pressed <- function(i) {
    force(i)
    function(x,y){
      x <- as.numeric(x)
      y <- as.numeric(y)
      tkdtag(canvas, "selected")
      tkaddtag(canvas, "selected", "withtag", "current")
      tkitemraise(canvas,"current")
      lastX <<- x
      lastY <<- y
      initX <<- x
      initY <<- y
    }
  }

  #mouse move on the text
  text_move <- function(i)
  {
    force(i)
    function(x,y){
      x <- as.numeric(x)
      y <- as.numeric(y)
      tkmove(canvas, "selected", x-lastX,y-lastY)
      lastX <<- x
      lastY <<- y
    }
  }
  
  #mouse release on the text
  text_released <- function(i){
    force(i)
    function(x,y) {
      #x and y contain the delta move from the button pressed
      x=as.numeric(x)-initX
      y=as.numeric(y)-initY
      #for all edges from this node
      for(e in list.from[[i]]) {
        sp=list.spline[[e]]
        sp$coord[[1]]=sp$coord[[1]]+as.numeric(x)
        sp$coord[[2]]=sp$coord[[2]]+as.numeric(y)
        #update the spline in the list
        list.spline[[e]]<<-sp
        #update the spline on the canvas
        tkcoords(canvas,sp$spline,sp$coord)
      }
      
      #for all edges to this node
      for(e in list.to[[i]]) {
        sp=list.spline[[e]]
        sp$coord[[7]]=sp$coord[[7]]+as.numeric(x)
        sp$coord[[8]]=sp$coord[[8]]+as.numeric(y)#-15
        #update the spline in the list
        list.spline[[e]]<<-sp
        #update the spline on the canvas
        tkcoords(canvas,sp$spline,sp$coord)
      }
    }
  }
  
  
  #mouse pressed on control point
  control_point_pressed <- function (i) {
    force(i)
    
    function(x,y){
      tkdtag(canvas, "selected")
      tkaddtag(canvas, "selected", "withtag", "current")
      tkitemraise(canvas,"current")
      last_pos <<- as.numeric ( c ( x , y ) )
      initX <<- as.numeric(x)
      initY <<- as.numeric(y)
    }
  }
  
  #mouse move on control point
  control_point_move <- function ( i ) {
    force(i)
    function(x,y) {
      pos <- as.numeric ( c ( x , y ) )
      tkmove ( canvas , "selected" , pos [ 1 ] - last_pos [ 1 ] ,
               pos [ 2 ] - last_pos [ 2 ] )
      last_pos <<- pos
    }
  }
  
  #mouse released ono control point
  control_point_released <- function ( spline,pos) {
    force(spline)
    function(x,y){
      #x and y contain the delta move from the button pressed
      x=as.numeric(x)-initX
      y=as.numeric(y)-initY
      #when a control point between two splines, the first must be distinguished from the second
      first=TRUE
      for(s in spline) {
        sp=list.spline[[s]]
        #update of the second control point of the spline
        if(pos==2) {
          sp$coord[[3]]=sp$coord[[3]]+x
          sp$coord[[4]]=sp$coord[[4]]+y
        }
        #update of the third control point of the spline
        if(pos==3) {
          sp$coord[[5]]=sp$coord[[5]]+x
          sp$coord[[6]]=sp$coord[[6]]+y
        }
        #update of the fourth control point of the spline
        if(pos==4 & first==TRUE) {
          sp$coord[[7]]=sp$coord[[7]]+x
          sp$coord[[8]]=sp$coord[[8]]+y
          first=FALSE
        }
        else
          #update of the first control point of the next spline
          #because this control point is joining 2 splines
          if(pos==4 & first==FALSE) {
            sp$coord[[1]]=sp$coord[[1]]+x
            sp$coord[[2]]=sp$coord[[2]]+y
          }
        #update the spline in the list
        list.spline[[s]]<<-sp
        #update the spline on the canvas
        tkcoords(canvas,sp$spline,sp$coord)
      }
    }
  }
  
  #last position for control point
  last_pos <<- numeric( 2 )
  
  #function to create a control point
  createPoint <- function(nb,x,y,r,spline,pos) {
    p <- tkcreate ( canvas , "oval" , x - r , y - r , x + r , y + r ,
                    width = 1 , outline = "black" , fill = "blue",tags="draw" )
    
    #functions to track the mouse
    tkitembind ( canvas , p , "<1>" , control_point_pressed(nb) )
    tkitembind ( canvas , p,"<B1-Motion>" , control_point_move(nb) )
    tkitembind ( canvas , p, "<ButtonRelease-1>" , control_point_released(spline,pos))
  }
  
  
  
  #number of splines  
  nb.spline=1
  #number of control points
  nb.control_point=1
  
  
  #if the list of nodes is not empty
  if(length(nodes)>0) {
    #create texts for all the nodes
    for (i in 1:length(nodes)) {
      #get current node
      node=nodes[[i]]
      #get coordinate of current node
      coord=getNodeXY(node)
      #get the name of the node
      name=name(node)
      
      #create the text
      p<-tkcreate(canvas, "text", offsetX+coord$x*scalingFactorX, workingHeight+0-coord$y*scalingFactorY, text=name,font=plotFont, fill="brown",tags="draw")
      #function to track the mouse
      tkitembind(canvas, p, "<1>", text_pressed(i))
      tkitembind(canvas, p,"<B1-Motion>", text_move(i))
      tkitembind(canvas, p, "<ButtonRelease-1>",text_released(i))
      #add the name in the list
      list.node=c(list.node,name)
    }
    
    #we need 2 lists: one for the edges from the node and the other one for the edges to the node
    list.from = vector("list",length(nodes))  
    list.to = vector("list",length(nodes))  
    
    #get the list of the edges
    edges = AgEdge(graph1)
    #for all edges
    for (i in 1:length(edges)) {
      edge=edges[[i]]
      #for all splines
      for (j in 1:numSplines(edge)) { 
        coord=getSpline(edge, j)
        #if this is the last spline, we add an arrow
        if(j==numSplines(edge)) {
          arrow='last'
        }
        else {
          arrow='none'
        }
        #compute the coordinates of the spline according to the scaling factor
        lCoord=numeric(8)
        for(k in 1:4) {
          lCoord[2*k-1]=offsetX+slot(cPoints(coord)[[k]],'x')*scalingFactorX
          lCoord[2*k]=workingHeight+0-slot(cPoints(coord)[[k]],'y')*scalingFactorY
        }
        #get the value of this rule
        val=rules[paste(tail(edge),"->",head(edge)),5]
        col="black"
        #compute the color of the edge according to the value of the rule
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
        #draw the spline
        sp=tkcreate(canvas, "line", lCoord,width=2,arrow=arrow,smooth='bezier',splinesteps=6,tags="draw",fill=col)
        #compute the number of the node from and to
        from=which(list.node==tail(edge))
        to=which(list.node==head(edge))
        #this list contains information on all the splines
        list.spline <<- c(list.spline,list(list(spline=sp,coord=lCoord,from=from,to=to)))
        
        
        #if this is the first spline of the edge
        if(j==1) {            
          #add the current spline to the list from for this node
          list.from[[from]]=c(list.from[[from]],nb.spline)
          #create control points
          #control point 2
          createPoint(nb=nb.control_point,x=lCoord[3],y=lCoord[4],r=r,spline=c(nb.spline),pos=2)
          nb.control_point=nb.control_point+1
          #control point 3
          createPoint(nb=nb.control_point,x=lCoord[5],y=lCoord[6],r=r,spline=c(nb.spline),pos=3)
          nb.control_point=nb.control_point+1
          #if there is another spline
          if(numSplines(edge)!=1) {
            #control point 4 and 1 for the next spline
            #warning the field spline is a vector with the current and the next spline
            createPoint(nb=nb.control_point,x=lCoord[7],y=lCoord[8],r=r,spline=c(nb.spline,nb.spline+1),pos=4)
            nb.control_point=nb.control_point+1
          }
        }
        #if this is the last spline of the edge
        if(j==numSplines(edge)){ 
          #add the current spline to the list to for this node
          list.to[[to]]=c(list.to[[to]],nb.spline)
          #if this spline is not the first one
          if(j!=1) {
            #control point 2
            createPoint(nb=nb.control_point,x=lCoord[3],y=lCoord[4],r=r,spline=c(nb.spline),pos=2)
            nb.control_point=nb.control_point+1
            #control point 3
            createPoint(nb=nb.control_point,x=lCoord[5],y=lCoord[6],r=r,spline=c(nb.spline),pos=3)
            nb.control_point=nb.control_point+1
          }
        }
        #if the spline is neither the first nor the last one
        if(j!=1 && j!=numSplines(edge)) {
          #control point 2
          createPoint(nb=nb.control_point,x=lCoord[3],y=lCoord[4],r=r,spline=c(nb.spline),pos=2)
          nb.control_point=nb.control_point+1
          #control point 3
          createPoint(nb=nb.control_point,x=lCoord[5],y=lCoord[6],r=r,spline=c(nb.spline),pos=3)
          nb.control_point=nb.control_point+1
          #control point 4 and 1 (see spline 1)
          createPoint(nb=nb.control_point,x=lCoord[7],y=lCoord[8],r=r,spline=c(nb.spline,nb.spline+1),pos=4)
          nb.control_point=nb.control_point+1
          
        }
        nb.spline=nb.spline+1
      }
    }
  }
  
  
  #write the current postscript image in the current directory
  tkpostscript(canvas, file="graph.ps",height=workingHeight,width=workingWidth)
  
  
  
  
}
