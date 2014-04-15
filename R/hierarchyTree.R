#' @title Computes and Displays the Hierarchy Tree.
#'
#' @description Reads the ASI rules, computes the hierarchy tree and displays it.
#' 
#' @param   list.variables  list of variables to compute the hierarchy tree from.
#'
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export

hierarchyTree <-function(list.variables, verbose=FALSE) {
  
  rules = read.table(file='transaction.out',header=TRUE,row.names=1,sep=',',stringsAsFactors=F)
  n = dim(rules)[1]
  
  print(list.variables)
  
  max.length.variables=max(str_length(list.variables))
  
  #data frame containing all the cohesion
  #in fact the computation of the cohesion is made just after...
  cohesion_matrix=matrix(nrow=length(list.variables),ncol=length(list.variables))
  colnames(cohesion_matrix)=list.variables
  rownames(cohesion_matrix)=list.variables
  
  #list of the occurrences of the variables
  list.occurrences.variables=vector()
  
  for(i in 1:n) {
    rule=strsplit(row.names(rules)[i],split=' -> ')
    from=rule[[1]][1]
    to=rule[[1]][2]
    val=as.numeric(rules[i,5])
    cohesion_matrix[from,to]=val
    
    list.occurrences.variables[from]=as.numeric(rules[i,1])
  }
  
  cohesion_matrix[is.na(cohesion_matrix)]=0 
  
 
  
  
  
  #apply the cohesion formula
  #first divide values by 100
  cohesion_matrix=cohesion_matrix/100 
  
  #first remove elements < 0.5
  cohesion_matrix[cohesion_matrix<0.5]=0 
  
  cohesion_matrix=sqrt(1-(-cohesion_matrix*log2(cohesion_matrix)-(1-cohesion_matrix)*log2(1-cohesion_matrix))^2)
  
  #replace NAN by 0  => NOT NICE...
  cohesion_matrix[is.na(cohesion_matrix)]=0
  
  
  
  
  
  ##convert to matrix
  #cohesion_matrix=data.matrix(cohesion_df)  
  
  
  
  #currently we consider that all items are selected
  list.selected.item=rep_len(T,length(list.variables))
  
  
  
  
  #call the hierarchy computation written in C
  res=callHierarchyComputation(cohesion_matrix,list.selected.item,list.occurrences.variables,verbose)
  
  
  
  
  
  list.hier.indexes.variable=res[[1]][[1]]
  list.hier.variables=res[[1]][[2]]
  
  #name of variables to create the classes
  variable.left=res[[2]]    #variable.left=tabo
  variable.right=res[[3]]   #variable.right=tabz
  
  
  nb.levels=res[[4]]
  
  list.significant.nodes=res[[5]]
  
  list.final.nodes=res[[6]]
  
  #print(list.final.nodes)
  
  #remove the () in the classes and convert the indexes from char to integer
  list.hier.indexes.variable=str_replace_all(list.hier.indexes.variable,"([())])","")
  list.hier.indexes.variable=strsplit(list.hier.indexes.variable,' ')
  list.hier.indexes.variable=as.integer(list.hier.indexes.variable[[1]])
  
  #remove the () and create a list of the variable in the order that they need to be displayed
  list.hier.variables=str_replace_all(list.hier.variables,"([())])","")
  list.hier.variables=strsplit(list.hier.variables,' ')
  list.hier.variables=list.hier.variables[[1]]
  
  
  
  
  offsetX=10
  offsetY=30
  
  dx=20
  dy=10
  
  visibleWidth=1200
  visibleHeight=800
  
  workingWidth=length(list.hier.variables)*dx+50
  workingHeight=offsetY+10*(max.length.variables)+nb.levels*dy+50
  
  offset.variable.x=0
  offset.variable.y=0
  for(i in 1:length(list.hier.indexes.variable)){
    offset.variable.x[list.hier.indexes.variable[i]]=offsetX+dx*i
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
    
    for (i in 1:length(list.hier.variables)) {
      #lengtt of current variable
      length.variable=str_length(list.hier.variables[i])
      #offset compared to the lenghtest variable
      offset.length.variable=max.length.variables-length.variable
      for (j in 1:str_length(list.hier.variables[i])) {
        tkcreate(canvas, "text", offsetX+i*dx, offsetY+10*(offset.length.variable+j), text=substr(list.hier.variables[i],j,j),font=plotFont, fill="brown",tags="draw")
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
      
    }
    
    for (u in 1:length(list.hier.variables))
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