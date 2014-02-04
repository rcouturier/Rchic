readRulesAndDisplayImplicativeGraph <-function() {
  
  #example of graph drawing for which the threshold can change
  
  
  library(Rgraphviz)
  require(tcltk) || stop("tcltk support is absent")
  require(graphics); require(stats)
  
  rules=read.table(file='transaction.out',header=TRUE,row.names=1,sep=',')
  visibleWidth=1200
  visibleHeight=800
  
  workingWidth=1200
  workingHeight=800
  
  tt <- tktoplevel()
  xscr <- tkscrollbar(tt, orient="horizontal",
                      command=function(...)tkyview(canvas,...))
  
  yscr <- tkscrollbar(tt, orient="vertical",
                     command=function(...)tkyview(canvas,...))
  
  
  
  
  
  
  canvas <- tkcanvas(tt, relief="raised", width=visibleWidth, height=visibleHeight,
                     xscrollcommand=function(...)tkset(xscr,...), 
                     yscrollcommand=function(...)tkset(yscr,...), 
                     scrollregion=c(0,0,workingWidth,workingHeight))
  tkconfigure(xscr, command = function(...) tkxview(canvas, ...))
  tkconfigure(yscr, command = function(...) tkyview(canvas, ...))
  #tkconfigure(canvas, xscrollcommand = function(...) tkset(xscr, ...))
  #tkconfigure(canvas, yscrollcommand = function(...) tkset(yscr, ...))
  
  tkpack(xscr, side = "bottom", fill = "x")
  tkpack(yscr, side = "right", fill = "y")
  tkpack(canvas, side = "left", fill="both", expand=1)
  
  plotFont <- "Helvetica 8"
  
  
  
  
  myreplot <- function(...) {
    
    
    
    threshold=as.numeric(tclObj(thresholdDialog))
    
    
        
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
        g1 <- addEdge(from,to,g1,1)
      }
    }
    
    #no need to plot thegraph
  #  plot(g1)
    
    
    
    graph1 <- agopen(g1,"foo")
    
    
    workingHeight=slot(slot(boundBox(graph1),'upRight'),'y')+100
    workingWidth=slot(slot(boundBox(graph1),'upRight'),'x')+100
    
    tkconfigure(canvas, scrollregion=c(0,0,workingWidth,workingHeight))
    
    tkdelete(canvas, "draw")
    
    
    nodes = AgNode(graph1)
    
    
    offsetX=40
    
    scalingFactor=0.8
    
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
        tkcreate(canvas, "line", lCoord,width=2,arrow=arrow,smooth='bezier',splinesteps=10,tags="draw")
      }
    }
  }
  
  have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
  if(have_ttk) {
    tkbutton <- ttkbutton
    tkframe <- ttkframe
  }
  
  thresholdDialog  <- tclVar(99.99)
  
  
  
  grDevices::devAskNewPage(FALSE) # override setting in demo()
  tclServiceMode(FALSE)
  base <- tktoplevel()  
  tkpack(tklabel(base, text="Threshold"))
  for ( i in c(99.99,99.9,99,95,90,80) ) {
    tmp <- tkradiobutton(base, command=myreplot,
                         text=i,value=i,variable=thresholdDialog)
    tkpack(tmp, anchor="w")
    
  }
  
  
  tclServiceMode(TRUE)
  
  
  myreplot()
  
  
}