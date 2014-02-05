readRulesComputeAndDisplayHierarchy <-function() {
  
  rules=read.table(file='transaction.out',header=TRUE,row.names=1,sep=',')
  n=dim(rules)[1]
  
  
  tempListVariables=strsplit(row.names(rules),split=' -> ')
  list.variables=character(0)
  
  for(i in 1:n) {
    
    
    from=tempListVariables[[i]][1]
    to=tempListVariables[[i]][2]
    list.variables=c(list.variables,from,to)
    
  }
  list.variables=sort(unique(list.variables))
  max.length.variables=max(str_length(list.variables))
  
  #data frame containing all the cohesion
  #in fact the computation of the cohesion is made just after...
  cohesion_df=data.frame()
  
  #list of the occurrences of the variables
  list.occurrences.variables=vector()
  
  for(i in 1:n) {
    rule=strsplit(row.names(rules)[i],split=' -> ')
    from=rule[[1]][1]
    to=rule[[1]][2]
    val=rules[i,5]
    cohesion_df[from,to]=val
    
    list.occurrences.variables[from]=rules[i,1]
  }
  
  cohesion_df[is.na(cohesion_df)]=0 
  
  #sort col names
  cohesion_df=cohesion_df[order(names(cohesion_df))]
  #sort row names
  cohesion_df=cohesion_df[order(row.names(cohesion_df)),]
  
  
  
  #apply the cohesion formula
  #first divide values by 100
  cohesion_df=cohesion_df/100 
  
  #first remove elements < 0.5
  cohesion_df[cohesion_df<0.5]=0 
  
  cohesion_df=sqrt(1-(-cohesion_df*log2(cohesion_df)-(1-cohesion_df)*log2(1-cohesion_df))^2)
  
  #replace NAN by 0  => NOT NICE...
  cohesion_df[is.na(cohesion_df)]=0
  
  
  
  
  
  #convert to matrix
  cohesion_matrix=data.matrix(cohesion_df)  
  
  
  
  #currently we consider that all items are selected
  list.selected.item=rep_len(T,length(list.variables))
  
  
  
  
  #call the similarity computation written in C
  res=callHierarchyComputation(cohesion_matrix,list.selected.item,list.occurrences.variables)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  list.simi.indexes.variable=res[[1]][[1]]
  list.simi.variables=res[[1]][[2]]
  
  #name of variables to create the classes
  variable.left=res[[2]]    #variable.left=tabo
  variable.right=res[[3]]   #variable.right=tabz
  
  
  nb.levels=res[[4]]
  
  list.significant.nodes=res[[5]]
  
  list.final.nodes=res[[6]]
  
  print(list.final.nodes)
  
  #remove the () in the classes and convert the indexes from char to integer
  list.simi.indexes.variable=str_replace_all(list.simi.indexes.variable,"([())])","")
  list.simi.indexes.variable=strsplit(list.simi.indexes.variable,' ')
  list.simi.indexes.variable=as.integer(list.simi.indexes.variable[[1]])
  
  #remove the () and create a list of the variable in the order that they need to be displayed
  list.simi.variables=str_replace_all(list.simi.variables,"([())])","")
  list.simi.variables=strsplit(list.simi.variables,' ')
  list.simi.variables=list.simi.variables[[1]]
  
  
  
  
  offsetX=10
  offsetY=30
  
  dx=20
  dy=10
  
  visibleWidth=1200
  visibleHeight=800
  
  workingWidth=length(list.simi.variables)*dx+50
  workingHeight=offsetY+10*(max.length.variables)+nb.levels*dy+50
  
  offset.variable.x=0
  offset.variable.y=0
  for(i in 1:length(list.simi.indexes.variable)){
    offset.variable.x[list.simi.indexes.variable[i]]=offsetX+dx*i
    offset.variable.y[i]=offsetY+10*max.length.variables+10
  }
  
  
  
  
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
    
    
    
    
    tkconfigure(canvas, scrollregion=c(0,0,workingWidth,workingHeight))
    
    tkdelete(canvas, "draw")
    
    level=0
    
    for (i in 1:length(list.simi.variables)) {
      #lengtt of current variable
      length.variable=str_length(list.simi.variables[i])
      #offset compared to the lenghtest variable
      offset.length.variable=max.length.variables-length.variable
      for (j in 1:str_length(list.simi.variables[i])) {
        tkcreate(canvas, "text", offsetX+i*dx, offsetY+10*(offset.length.variable+j), text=substr(list.simi.variables[i],j,j),font=plotFont, fill="brown",tags="draw")
      }
    }
    
    offsetY=offsetY+10*max.length.variables+10
    
    line.coord=numeric(4)
    for (j in 1:nb.levels) {
      #for (j in 1:12) {  
      
      y2=dy*j+offsetY;
      line.coord[1]=offset.variable.x[variable.left[j]]   #tabz = offset.variable.x
      line.coord[2]=offset.variable.y[variable.left[j]]   #tabh = offset.variable.y
      line.coord[3]=offset.variable.x[variable.left[j]]
      line.coord[4]=y2
      #draw the left horizontal line
      tkcreate(canvas, "line", line.coord,width=2,tags="draw")
      
      line.coord[1]=offset.variable.x[variable.left[j]]
      line.coord[2]=y2
      line.coord[3]=offset.variable.x[variable.right[j]]
      line.coord[4]=y2
      
      
      #draw the vertical line
      if(list.significant.nodes[j]) {   
        color="red"
      }
      else {
        color="black"
      }
      tkcreate(canvas, "line", line.coord,width=2,tags="draw",fill=color)
      
      #draw arrow
      line.coord[1]=offset.variable.x[variable.right[j]]-5
      line.coord[2]=y2-5
      line.coord[3]=offset.variable.x[variable.right[j]]
      line.coord[4]=y2
      tkcreate(canvas, "line", line.coord,width=2,tags="draw",fill=color)
      line.coord[1]=offset.variable.x[variable.right[j]]-5
      line.coord[2]=y2+5
      line.coord[3]=offset.variable.x[variable.right[j]]
      line.coord[4]=y2
      tkcreate(canvas, "line", line.coord,width=2,tags="draw",fill=color)
      
      
      
      
      line.coord[1]=offset.variable.x[variable.right[j]]
      line.coord[2]=y2
      line.coord[3]=offset.variable.x[variable.right[j]]
      line.coord[4]=offset.variable.y[variable.right[j]]
      
      #draw the right line
      tkcreate(canvas, "line", line.coord,width=2,tags="draw")
      
      
      offset.variable.x[variable.left[j]]=(offset.variable.x[variable.left[j]]+offset.variable.x[variable.right[j]])/2
      
      offset.variable.x[variable.right[j]]=offset.variable.x[variable.left[j]]
      
      offset.variable.y[variable.left[j]]=y2
      
      
      
      offset.variable.y[variable.right[j]]=y2
      
      
      
      level[variable.left[j]]=-1;
      level[variable.right[j]]=-1;
      #     i=2;
      #     for (u=0;u<nb_col;u++)
      #     {
      #       if(Item[u] && level[u]==j-1)
      #       {
      #         dc->MoveTo((int)taby[u][0],tabh[u]);
      #         dc->LineTo((int)taby[u][0],dy*f+deby);  
      #       }
      #     }
      
    }
    
    for (u in 1:length(list.simi.variables))
    {
      if(list.final.nodes[u])
      {
        line.coord[1]=offset.variable.x[u]
        line.coord[2]=offset.variable.y[u]
        line.coord[3]=offset.variable.x[u]
        line.coord[4]=dy*nb.levels+offsetY
        tkcreate(canvas, "line", line.coord,width=2,tags="draw")
      }
    }
    
    
  }
  
  have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
  if(have_ttk) {
    tkframe <- ttkframe
  }
  
  
  tclServiceMode(TRUE)
  
  
  myreplot()
}