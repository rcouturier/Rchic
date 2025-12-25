#' @title Displays the Implicative Graph.
#'
#' @description Reads the ASI rules, selects the rules according to the toolbar and calls rgraphviz before displaying the rules.
#'
#' @details 
#' This function allows you to compute the implicative graph. As its name suggests, this graph illustrates implication rules,
#' or usually called association rules. Nevertheless the way to compute these association rules is completely different from
#' the classical association rules (computed with the conditional probability).
#' In the implication graph, only the strongest implication rules are displayed. In pratice, the term implication is not totally
#' true because it is quasi-implication. In mathematic, if we have an implication there is no counter-example. With Statistical
#' Implicative Analysis, quasi implications can have some counter-examples. Also for commodity reason, the term implication will
#' be used instead of quasi implication.
#' 
#' On the right of the figure, one can select 
#' the threshold allowing us to select only the strongest implications. It is possible to select different thresholds (up to 4).
#' The color of each threshold can be changed. Consequently a user can quickly see the implications in this graph. For example,
#' in the first figure we can see that two implications have a implication index greater than 0.99 (this number is multiplied 
#' by 100 in rchic for comodity reason). These implications are: craftiness implies malignant and powerful implies strong.
#' 
#' \if{html}{\figure{implicative1.png}}
#' \if{latex}{\figure{implicative1.png}{options: width=15cm}}
#' 
#' If one change the threshold parameters, the graph is automatically displayed again as in the next figure.
#' \if{html}{\figure{implicative2.png}}
#' \if{latex}{\figure{implicative2.png}{options: width=15cm}}
#' 
#' In this figure, the number of implications is greater. According to the color of the implication, a user can quickly see
#' what the the interesting implications.
#' 
#' @param   fileName      name of the file containing the data
#' @param list.variables   list containing all the variables used in the computation
#' @param computing.mode   controls the computing mode: 1=classic implication, 2=classic implication+ confidence, 3=implifiance
#' @param complete.graph   displays a complete graph (with a=>b and b=>a rules)
#' @author Raphael Couturier 
#' @export
implicativeGraph <-function(fileName,list.variables,computing.mode=1,complete.graph=0) {
  confidence<<-0
  visibleWidth<<-1200
  visibleHeight<<-800
  workingWidth<<-1200
  workingHeight<<-800
  
  #create the window
  top <- tktoplevel()
  #put the name of the window
  tkwm.title( top , paste("Implicative graph",fileName) )
  
  tt <- ttkframe ( top )#, padding = 0)
  toolbar <- ttkframe ( top )#, padding = 0 )
  tkgrid(toolbar,row = 0 , column = 0, sticky = "news")
  tkgrid(tt,row = 0 , column = 1 , sticky = "news")
  tkgrid.columnconfigure (top , 1 , weight = 1 )
  tkgrid.rowconfigure (top , 0 , weight = 1 )
  #list that'll contain the values of confidence
  conf<<-list()
  fromconf<<- list()
  toconf<<- list()
  confconf1<<- list()
  #list that'll contain the the coordinates where drawing confidence
  coordx1<<- list()
  coordx2<<- list()
  var2<<-list()
  
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
  myedit<<-tclVar(0)
  myaffiche<<-tclVar(0)
  list.variables<<-list.variables
  computing.mode<<-computing.mode
  complete.graph<<-complete.graph
  #currently we consider that all items are selected
  list.selected.item=rep_len(T,length(list.variables))
  list.tcl<<-lapply(list.selected.item,function(i) tclVar(i))
  toolbarGraph(toolbar,callPlotImplicativeGraph,updatePlotImplicativeGraph)
  tkconfigure(xscr, command = function(...) tkxview(canvas, ...))
  tkconfigure(yscr, command = function(...) tkyview(canvas, ...))
  #tkconfigure(canvas, xscrollcommand = function(...) tkset(xscr, ...))
  #tkconfigure(canvas, yscrollcommand = function(...) tkset(yscr, ...))
  tkpack(xscr, side = "bottom", fill = "x")
  tkpack(yscr, side = "right", fill = "y")
  tkpack(canvas, side = "left", fill="both", expand=1)
  plotFont <<- "Helvetica 10"
  toolbarItem(list.variables,list.tcl,callPlotImplicativeGraph)
  callPlotImplicativeGraph()
}

callPlotImplicativeGraph <- function() {
  # Need to check these parameters here
  thres=100
  for(i in 1:4) {
    check <- as.numeric(tclvalue(mycbvalue[[i]]))
    #print(check)
    val=as.numeric(tclvalue(myvalue[[i]]))
    #print(val)
    if(check)
      thres=val
  }
  edit=as.numeric(tclvalue(myedit))
  # affiche=as.numeric(tclvalue(myaffiche))
  list.selected.item=lapply(list.tcl,function(i) tclvalue(i))
  #print(list.selected.item)
  computeImplicativeGraph(thres,sapply(myvalue,tclvalue),sapply(mycbvalue,tclvalue),mycolor,list.selected.item, confconf1)
  plotImplicativeGraph(thres,sapply(myvalue,tclvalue),sapply(mycbvalue,tclvalue),mycolor,list.selected.item,edit=edit,first=TRUE)
}

updatePlotImplicativeGraph <- function() {
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
  edit=as.numeric(tclvalue(myedit))
  # affiche=as.numeric(tclvalue(myaffiche))
  list.selected.item=lapply(list.tcl,function(i) tclvalue(i))
  plotImplicativeGraph(thres,sapply(myvalue,tclvalue),sapply(mycbvalue,tclvalue),mycolor,list.selected.item,edit=edit,first=FALSE)
}

computeImplicativeGraph <- function(thres=99,value,cbvalue,color,list.selected.item,confconf1) {
  rules<-read.table(file='transaction.out',header=TRUE,row.names=1,sep=',',stringsAsFactors = FALSE)
  row=row.names(rules)
  rules=as.data.frame(lapply(rules,as.numeric))
  row.names(rules)=row
  n=dim(rules)[1]
  listNodes=strsplit(row.names(rules),split=' -> ')
  
  write.csv(rules, "raph.csv", row.names = FALSE)
  
  #according to computing.mode, the location of the index is not the same
  if(computing.mode==1 | computing.mode==2)
    index.imp=5    #it corresponds to the classical index
  if(computing.mode==4)
    index.imp=6    #it corresponds to the entropic
  if(computing.mode==3)
    index.imp=7    #it corresponds to the implifiance
  
  
  #determine the list of visible nodes
  lNodes=character(0)
  for(i in 1:n) {
    from=listNodes[[i]][1]
    to=listNodes[[i]][2]
    if( rules[i,index.imp]>thres  & as.numeric(list.selected.item[[which(list.variables==from)]]) 
         & as.numeric(list.selected.item[[which(list.variables==to)]]) & 
        ((computing.mode==2 & rules[i,4]>=confidence) | computing.mode!=2) & 
        ((complete.graph==0 & rules[i,1]<=rules[i,2]) | complete.graph!=0)) {
                lNodes=c(lNodes,from,to)
    }
  }
  lNodes=unique(lNodes)
  # listNode will be used to search for the names of the corresponding node number 1 2 3
  listNode <<- list()
  for(i in 1:length(lNodes)) {
    listNode[i]<<-lNodes[i]
  }
  
  print("complete graph")
  print(complete.graph==0)
  
  #create the graph with the nodes
  #g1 <- new("graphNEL", nodes = lNodes,edgemode = "directed")
  g1 <- graphNEL( nodes = lNodes,edgemode = "directed")
  #add the edge of the graph
  compte<-1
  for(i in 1:n) {
    rule=strsplit(row.names(rules)[i],split=' -> ')
    from=rule[[1]][1]
    to=rule[[1]][2]
    if( rules[i,index.imp]>thres&
        as.numeric(list.selected.item[[which(list.variables==from)]]) &
        as.numeric(list.selected.item[[which(list.variables==to)]]) &
        ((computing.mode==2 & rules[i,4]>=confidence) | computing.mode!=2) &
        ((complete.graph==0 & rules[i,1]<=rules[i,2]) | complete.graph!=0) )  {
          g1 <- addEdge(from,to,g1)
          # retrieve the confidence value
          conf1=rules[i,4]
          # rounding confidence to two digits after the decimal point
          conf2=round(conf1,2)
          #we keep the from and to value and confidence in tables (global variable) to display later confidence values without recalculating all the graph
          fromconf[compte]<<-from
          toconf[compte]<<-to
          confconf1[compte]<<-conf2
          compte=compte+1
    }
    
  }
  #no need to plot thegraph
  #plot(g1,recipEdges="distinct")
  #call rgraphviz to draw a nice graph
  graph1 <- agopen(g1,"foo",recipEdges="distinct")
  toFile(graph1, layoutType="dot", filename="g1_dot.svg", fileType="svg")
  
  offsetX<<-100 #-40
  scalingFactorX<<-1.9/1.6#*10
  scalingFactorY<<-0.5/2.#*10
  size.x=slot(slot(boundBox(graph1),'upRight'),'x')
  size.y=slot(slot(boundBox(graph1),'upRight'),'y')
  workingHeight<<-size.y*scalingFactorY+10
  workingWidth<<-size.x*scalingFactorX+offsetX*2.
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
  list.spline <- list()
  list.node <- list()
  list.name <- list()
  #size of the controle nodes
  r=5
  #number of splines
  nb.spline=1
  #number of control points
  nb.control_point=1
  #we need 2 lists: one for the edges from the node and the other one for the edges to the node
  list.from = vector("list",length(nodes))
  list.to = vector("list",length(nodes))
  list.edge=list()
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
      coord$x=offsetX+coord$x*scalingFactorX
      coord$y=workingHeight+0-coord$y*scalingFactorY
      list.node=c(list.node,list(list(name,coord)))
      list.name=c(list.name,name)
    }
    #get the list of the edges
    edges = AgEdge(graph1)
    #for all edges
    for (i in 1:length(edges)) {
      edge=edges[[i]]
      list.edge=c(list.edge,edge)
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
        val=rules[paste(tail(edge),"->",head(edge)),index.imp]
        col=color[[1]]
        #compute the color of the edge according to the value of the rule
        if(cbvalue[[1]]==1 && value[[1]]<val){ 
          col=color[[1]]
        }
        else {
          if(cbvalue[[2]]==1 && value[[2]]<val)
            col=color[[2]]
          else {
            if(cbvalue[[3]]==1 && value[[3]]<val)
              col=color[[3]]
            else {
              if(cbvalue[[4]]==1 && value[[4]]<val)
                col=color[[4]]
            }
          }
        }
        #draw the spline
        
        #compute the number of the node from and to
        from=which(list.name==tail(edge))
        to=which(list.name==head(edge))
        #this list contains information on all the splines
        list.spline = c(list.spline,list(list(spline=nb.spline,coord=lCoord,from=from,to=to,arrow=arrow,col=col,num.spline=j,endspline=numSplines(edge))))
        nb.spline1=nb.spline+1
      }
    }
  }
  #write the current postscript image in the current directory
  tkpostscript(canvas, file="graph.ps",height=workingHeight,width=workingWidth)
  list.edge<<-list.edge
  list.node<<-list.node
  list.spline<<-list.spline
}

plotImplicativeGraph <- function(thres=99,value,cbvalue,color,list.selected.item,edit=FALSE,first=TRUE) {
  tkdelete(canvas, "text")
  tkdelete(canvas, "text1")
  
  if(first) {
    tkdelete(canvas, "control")
    tkdelete(canvas, "draw")
  }
  if(edit==FALSE) {
    tkdelete(canvas, "control")
  }
  #coordinates to compute the delta of a move from the last mouse move
  lastX=0
  lastY=0
  #coordinates to compute the delta of a move from the initial button pressed
  initX=0
  initY=0
  #size of the controle nodes
  r=5
  if(first) {
    list.spline.object <<- list()
    list.control <<- list()
    #last position for control point
    last_pos <<- numeric( 2 )
  }
  #mouse pressed on text item
  text_pressed <- function(i) {
    if(edit==TRUE) {
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
  }
  #mouse move on the text
  text_move <- function(i)
  {
    if(edit==TRUE) {
      force(i)
      function(x,y){
        x <- as.numeric(x)
        y <- as.numeric(y)
        tkmove(canvas, "selected", x-lastX,y-lastY)
        lastX <<- x
        lastY <<- y
      }
    }
  }
  #mouse release on the text
  text_released <- function(i){
    if(edit==TRUE) {
      force(i)
      function(x,y) {
        #x and y contain the delta move from the button pressed
        x=as.numeric(x)-initX
        y=as.numeric(y)-initY
        node=list.node[[i]]
        node[[2]]$x=node[[2]]$x+as.numeric(x)
        node[[2]]$y=node[[2]]$y+as.numeric(y)
        list.node[[i]]<<-node
        #for all edges from this node
        for(e in list.from[[i]]) {
          sp=list.spline.object[[e]]
          sp$coord[[1]]=sp$coord[[1]]+as.numeric(x)
          sp$coord[[2]]=sp$coord[[2]]+as.numeric(y)
          #update the spline in the list
          list.spline.object[[e]]<<-sp
          #update the spline on the canvas
          tkcoords(canvas,sp$spline,sp$coord)
        }
        #for all edges to this node
        for(e in list.to[[i]]) {
          sp=list.spline.object[[e]]
          sp$coord[[7]]=sp$coord[[7]]+as.numeric(x)
          sp$coord[[8]]=sp$coord[[8]]+as.numeric(y)#-15
          #update the spline in the list
          list.spline.object[[e]]<<-sp
          #update the spline on the canvas
          tkcoords(canvas,sp$spline,sp$coord)
        }
      }
    }
  }
  #mouse pressed on control point
  control_point_pressed <- function (i) {
    if(edit==TRUE) {
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
  }
  #mouse move on control point
  control_point_move <- function ( i ) {
    if(edit==TRUE) {
      force(i)
      function(x,y) {
        pos <- as.numeric ( c ( x , y ) )
        tkmove ( canvas , "selected" , pos [ 1 ] - last_pos [ 1 ] ,
                 pos [ 2 ] - last_pos [ 2 ] )
        last_pos <<- pos
      }
    }
  }
  
  
  # displays the values of confidence when moving the control points 
  displayConfidence<- function(){
    # delete the old value of confidence
    tkdelete(canvas, "text1")
    if(boolDisplayConfidence==T)
    {
      
      for (i in 1:(num-1)) {
        
        Xm=coordx1[[i]]
        Ym=coordx2[[i]]
        var=var2[[i]]
        
        # displays the new values
        p1 <<- tkcreate(canvas, "text", Xm, Ym, text=var, fill="black",tags="text1")      
        
      }
    }
  }
  
  #mouse released ono control point
  control_point_released <- function ( spline,pos) {
    if(edit==TRUE) {
      force(spline)
      function(x,y){
        #x and y contain the delta move from the button pressed
        x=as.numeric(x)-initX
        y=as.numeric(y)-initY
        #when a control point between two splines, the first must be distinguished from the second
        first=TRUE
        for(s in spline) {
          sp=list.spline.object[[s]]
          endspline=sp$endspline
          num1=sp$num
          confidence=sp$conf
          num.spline=sp$num.spline.edge
          
          #update of the second control point of the spline
          if(pos==2) {
            sp$coord[[3]]=sp$coord[[3]]+x
            sp$coord[[4]]=sp$coord[[4]]+y
            
            # update confidence coordinates when the control points move
            
            if((endspline==1)||((endspline==3)&&(num.spline==2)))
            {
              Xm=(sp$coord[[3]]+sp$coord[[5]])/2
              Ym=(sp$coord[[4]]+sp$coord[[6]])/2
              coordx1[num1]<<-Xm
              coordx2[num1]<<-Ym
            }
            
            if((endspline>4)&&(num.spline==3)){
              Xm=(sp$coord[[3]]+sp$coord[[5]])/2
              Ym=(sp$coord[[4]]+sp$coord[[6]])/2
              coordx1[num1]<<-Xm
              coordx2[num1]<<-Ym
            }
            
          }
          #update of the third control point of the spline
          if(pos==3) {
            sp$coord[[5]]=sp$coord[[5]]+x
            sp$coord[[6]]=sp$coord[[6]]+y
            
            
            if((endspline==1)||((endspline==3)&&(num.spline==2)))
            {
              Xm=(sp$coord[[3]]+sp$coord[[5]])/2
              Ym=(sp$coord[[4]]+sp$coord[[6]])/2
              coordx1[num1]<<-Xm
              coordx2[num1]<<-Ym
            }
            
            if((endspline==2)&&(num.spline==1))
            {
              Xm=(sp$coord[[5]]+sp$coord[[7]])/2
              Ym=(sp$coord[[6]]+sp$coord[[8]])/2
              coordx1[num1]<<-Xm
              coordx2[num1]<<-Ym
            }
            
            if((endspline==4)&&(num.spline==2))
            {
              Xm=(sp$coord[[5]]+sp$coord[[7]])/2
              Ym=(sp$coord[[6]]+sp$coord[[8]])/2
              coordx1[num1]<<-Xm
              coordx2[num1]<<-Ym
            }
            if((endspline>4)&&(num.spline==3)){
              Xm=(sp$coord[[3]]+sp$coord[[5]])/2
              Ym=(sp$coord[[4]]+sp$coord[[6]])/2
              coordx1[num1]<<-Xm
              coordx2[num1]<<-Ym
            }
          }
          #update of the fourth control point of the spline
          if(pos==4 & first==TRUE) {
            sp$coord[[7]]=sp$coord[[7]]+x
            sp$coord[[8]]=sp$coord[[8]]+y
            first=FALSE
            if((endspline==2)&&(num.spline==1))
            {
              Xm=(sp$coord[[5]]+sp$coord[[7]])/2
              Ym=(sp$coord[[6]]+sp$coord[[8]])/2
              coordx1[num1]<<-Xm
              coordx2[num1]<<-Ym
            }
            if((endspline==4)&&(num.spline==2))
            {
              Xm=(sp$coord[[5]]+sp$coord[[7]])/2
              Ym=(sp$coord[[6]]+sp$coord[[8]])/2
              coordx1[num1]<<-Xm
              coordx2[num1]<<-Ym
            }
          }
          else
            #update of the first control point of the next spline
            #because this control point is joining 2 splines
            if(pos==4 & first==FALSE) {
              sp$coord[[1]]=sp$coord[[1]]+x
              sp$coord[[2]]=sp$coord[[2]]+y
              
              
              
            }
          
          
          #update the spline in the list
          list.spline.object[[s]]<<-sp
          #update the spline on the canvas
          tkcoords(canvas,sp$spline,sp$coord)
        }
        displayConfidence()
      }
    }
  }
  #function to create a control point
  createPoint <- function(nb,x,y,r,spline,pos) {
    if(edit) {
      sp=list.spline.object[[spline[1]]]
      x=sp$coord[[2*pos-1]]
      y=sp$coord[[2*pos]]
      p <- tkcreate ( canvas , "oval" , x - r , y - r , x + r , y + r ,
                      width = 1 , outline = "black" , fill = "blue",tags="control" )
      #functions to track the mouse
      tkitembind ( canvas , p , "<1>" , control_point_pressed(nb) )
      tkitembind ( canvas , p,"<B1-Motion>" , control_point_move(nb) )
      tkitembind ( canvas , p, "<ButtonRelease-1>" , control_point_released(spline,pos))
    }
    if(first) {
      list.control<<-c(list.control,list(list(nb,x,y,spline,pos)))
    }
    
  }
  if(first) {
    
    #number of control points
    nb.control_point=1
    #we need 2 lists: one for the edges from the node and the other one for the edges to the node
    list.from <<- vector("list",length(list.node))
    list.to <<- vector("list",length(list.node))
    #list.edge=list()
    #if the list of nodes is not empty
    if(length(list.node)>0) {
      #create texts for all the nodes
      for (i in 1:length(list.node)) {
        #get current node
        node=list.node[[i]]
        #get coordinate of current node
        coord=node[[2]]
        #get the name of the node
        name=node[[1]]
        #create the text
        p<-tkcreate(canvas, "text", coord$x, coord$y, text=name,font=plotFont, fill="black",tags="text")
        #function to track the mouse
        tkitembind(canvas, p, "<1>", text_pressed(i))
        tkitembind(canvas, p,"<B1-Motion>", text_move(i))
        tkitembind(canvas, p, "<ButtonRelease-1>",text_released(i))
        #add the name in the list
        #list.node=c(list.node,name)
      }
      #
      # #get the list of the edges
      # edges = AgEdge(graph1)
      # #for all edges
      num <<-1
      for (i in 1:length(list.spline)) {
        
        sp=list.spline[[i]]
        sp=list.spline[[i]]
        from=sp[['from']]
        to=sp[['to']]
        # Retrieve the corresponding names of nodes
        from1 <-listNode[from]
        to1 <-listNode[to]
        num.spline.edge=sp[['num.spline']]
        endspline=sp[['endspline']]
        coord=sp[['coord']]
        coord1=sp[['coord']]
        
        sp <- tkcreate(canvas, "line", sp[['coord']],width=2,arrow=sp[['arrow']],smooth='bezier',splinesteps=6,tags="draw",fill=sp[['col']])
        
        
        #calculates the coordinates where we will display the confidence
        for (k in 1:length(confconf1)) {
          # Search the confidence value that corresponds to each pair (from to) in the order
          chai1=fromconf[k]
          chai2=toconf[k]
          if(grepl(chai1, from1) & grepl(chai2,to1))
          {
            
            
            var2[num] <<- confconf1[[k]]
            confidence=var2[num]
            
          }
        }
        list.spline.object <<- c(list.spline.object,list(list(spline=sp,coord=coord,from=from,to=to,endspline=endspline,num=num,conf=confidence,num.spline.edge=num.spline.edge)))
        
        #if this is the first spline of the edge
        if(num.spline.edge==1) {
          #add the current spline to the list from for this node
          list.from[[from]]<<-c(list.from[[from]],i)
          #create control points
          #control point 2
          createPoint(nb=nb.control_point,x=coord[3],y=coord[4],r=r,spline=c(i),pos=2)
          nb.control_point=nb.control_point+1
          #control point 3
          createPoint(nb=nb.control_point,x=coord[5],y=coord[6],r=r,spline=c(i),pos=3)
          nb.control_point=nb.control_point+1
          #if there is another spline
          if(endspline!=1) {
            #control point 4 and 1 for the next spline
            #warning the field spline is a vector with the current and the next spline
            createPoint(nb=nb.control_point,x=coord[7],y=coord[8],r=r,spline=c(i,i+1),pos=4)
            nb.control_point=nb.control_point+1
          }
        }
        #if this is the last spline of the edge
        if(num.spline.edge==endspline){
          #add the current spline to the list to for this node
          list.to[[to]]<<-c(list.to[[to]],i)
          #if this spline is not the first one
          if(num.spline.edge!=1) {
            #control point 2
            createPoint(nb=nb.control_point,x=coord[3],y=coord[4],r=r,spline=c(i),pos=2)
            nb.control_point=nb.control_point+1
            #control point 3
            createPoint(nb=nb.control_point,x=coord[5],y=coord[6],r=r,spline=c(i),pos=3)
            nb.control_point=nb.control_point+1
          }
        }
        #if the spline is neither the first nor the last one
        if(num.spline.edge!=1 && num.spline.edge!=endspline) {
          #control point 2
          createPoint(nb=nb.control_point,x=coord[3],y=coord[4],r=r,spline=c(i),pos=2)
          nb.control_point=nb.control_point+1
          #control point 3
          createPoint(nb=nb.control_point,x=coord[5],y=coord[6],r=r,spline=c(i),pos=3)
          nb.control_point=nb.control_point+1
          #control point 4 and 1 (see spline 1)
          createPoint(nb=nb.control_point,x=coord[7],y=coord[8],r=r,spline=c(i,i+1),pos=4)
          nb.control_point=nb.control_point+1
        }
        
        
        sp=list.spline[[i]]
        sp=list.spline[[i]]
        
        # one spline: then we have 3 segments, the display will be in the middle of the 2nd segment if((num.spline.edge==1) && (endspline==1)) {
        if((num.spline.edge==1) && (endspline==1)) {
          
          
          Xm= (sp$coord[[5]]+sp$coord[[3]])/2
          Ym=(sp$coord[[6]]+sp$coord[[4]])/2
          coordx1[num]<<-Xm
          coordx2[num]<<-Ym
        }else {# two splines: then we have 6 segments, the display will be in the middle of the 3rd segment if((num.spline.edge==1) && (endspline==1)) {
          if((num.spline.edge==1) && (endspline==2)) {
            Xm= (sp$coord[[5]]+sp$coord[[7]])/2
            Ym=(sp$coord[[6]]+sp$coord[[8]])/2
            coordx1[num]<<-Xm
            coordx2[num]<<-Ym
          }else # Tree splines: then we have 9 segments, the display will be in the middle of the 5th segment
          {
            if((num.spline.edge==2) && (endspline==3)) {
              Xm= (sp$coord[[5]]+sp$coord[[3]])/2
              Ym=(sp$coord[[6]]+sp$coord[[4]])/2
              coordx1[num]<<-Xm
              coordx2[num]<<-Ym
            }
            else
            {
              if((num.spline.edge==2) && (endspline==4)) {
                
                Xm= (sp$coord[[7]]+sp$coord[[5]])/2
                Ym=(sp$coord[[8]]+sp$coord[[6]])/2
                coordx1[num]<<-Xm
                coordx2[num]<<-Ym
              }
              else 
              {
                if((num.spline.edge==3) && (endspline >4)) {                      
                  Xm= (sp$coord[[5]]+sp$coord[[3]])/2
                  Ym=(sp$coord[[6]]+sp$coord[[4]])/2
                  coordx1[num]<<-Xm
                  coordx2[num]<<-Ym
                }
              }
            }
            
            
          }
        }
        
        if(num.spline.edge==endspline)
        {
          num<<-num+1
        }
      }
      
    }  
  }
  
  else {
    for (i in 1:length(list.control)) {
      element=list.control[[i]]
      createPoint(nb=element[[1]],x=element[[2]],y=element[[3]],r=r,spline=element[[4]],pos=element[[5]])
    }
    for (i in 1:length(list.node)) {
      #get current node
      node=list.node[[i]]
      #get coordinate of current node
      coord=node[[2]]
      #get the name of the node
      name=node[[1]]
      #create the text
      p<-tkcreate(canvas, "text", coord$x, coord$y, text=name,font=plotFont, fill="brown",tags="text")
      #function to track the mouse
      tkitembind(canvas, p, "<1>", text_pressed(i))
      tkitembind(canvas, p,"<B1-Motion>", text_move(i))
      tkitembind(canvas, p, "<ButtonRelease-1>",text_released(i))
      #add the name in the list
      #list.node=c(list.node,name)
    }
  }
  #write the current postscript image in the current directory
  tkpostscript(canvas, file="graph.ps",height=workingHeight,width=workingWidth)
}
