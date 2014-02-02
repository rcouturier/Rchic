library(rchic)
library(stringr)
require(tcltk) || stop("tcltk support is absent")
source('R/callAsirules.R')


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

similarity_df=data.frame()

for(i in 1:n) {
  rule=strsplit(row.names(rules)[i],split=' -> ')
  from=rule[[1]][1]
  to=rule[[1]][2]
  val=rules[i,7]
  similarity_df[from,to]=val
}

similarity_df[is.na(similarity_df)]=0 

#sort col names
similarity_df=similarity_df[order(names(similarity_df))]
#sort row names
similarity_df=similarity_df[order(row.names(similarity_df)),]


#convert to matrix
similarity_matrix=data.matrix(similarity_df/100)  #we need to use values between 0 and 1





res=callSimilarityComputation(similarity_matrix)

list.simi.indexes.variable=res[[1]][[1]]
list.simi.variables=res[[1]][[2]]
tabo=res[[2]]
tabz=res[[3]]

nb.levels=res[[4]]+1

list.simi.indexes.variable=str_replace_all(list.simi.indexes.variable,"[[:punct:]]","")
list.simi.indexes.variable=strsplit(list.simi.indexes.variable,' ')
list.simi.indexes.variable=as.integer(list.simi.indexes.variable[[1]])


list.simi.variables=str_replace_all(list.simi.variables,"[[:punct:]]","")
list.simi.variables=strsplit(list.simi.variables,' ')
list.simi.variables=list.simi.variables[[1]]




offsetX=10
offsetY=30
space.item=20
dx=20
dy=10

visibleWidth=1200
visibleHeight=800

workingWidth=nb.levels*space.item+50
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
  
  for (i in 1:nb.levels) {
    #lengtt of current variable
    length.variable=str_length(list.simi.variables[i])
    #offset compared to the lenghtest variable
    offset.length.variable=max.length.variables-length.variable
    for (j in 1:str_length(list.simi.variables[i])) {
      tkcreate(canvas, "text", offsetX+i*space.item, offsetY+10*(offset.length.variable+j), text=substr(list.simi.variables[i],j,j),font=plotFont, fill="brown",tags="draw")
    }
  }
  
  offsetY=offsetY+10*max.length.variables+10
  
  line.coord=numeric(4)
  for (j in 1:nb.levels) {
  #for (j in 1:12) {  
  
    y2=dy*j+offsetY;
    line.coord[1]=offset.variable.x[tabo[j]]   #tabh = offset.variable.x
    line.coord[2]=offset.variable.y[tabo[j]]   #tabh = offset.variable.y
    line.coord[3]=offset.variable.x[tabo[j]]
    line.coord[4]=y2
    
    tkcreate(canvas, "line", line.coord,width=2,tags="draw")
    
    line.coord[1]=offset.variable.x[tabo[j]]
    line.coord[2]=y2
    line.coord[3]=offset.variable.x[tabz[j]]
    line.coord[4]=y2
    
    tkcreate(canvas, "line", line.coord,width=2,tags="draw")
    
    
    line.coord[1]=offset.variable.x[tabz[j]]
    line.coord[2]=y2
    line.coord[3]=offset.variable.x[tabz[j]]
    line.coord[4]=offset.variable.y[tabz[j]]
    
    tkcreate(canvas, "line", line.coord,width=2,tags="draw")
    
    
#     dc->LineTo((int)taby[tabz[j-1]][0],tabh[tabz[j-1]]);
    
    offset.variable.x[tabo[j]]=(offset.variable.x[tabo[j]]+offset.variable.x[tabz[j]])/2
#     taby[tabo[j-1]][0]=(taby[tabo[j-1]][0]+taby[tabz[j-1]][0])/2;
    offset.variable.x[tabz[j]]=offset.variable.x[tabo[j]]
#     taby[tabz[j-1]][0]=taby[tabo[j-1]][0];
    offset.variable.y[tabo[j]]=y2
    
#     tabh[tabo[j-1]]=y2;
    
    offset.variable.y[tabz[j]]=y2
#     tabh[tabz[j-1]]=y2;
    
    
     level[tabo[j]]=-1;
     level[tabz[j]]=-1;
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
  
  
  
  
}

have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
if(have_ttk) {
  tkframe <- ttkframe
}


tclServiceMode(TRUE)


myreplot()






# 
# for (j=1;j<=f;j++)
# {
#   y1=dy*(j-1)+deby;
#   y2=dy*j+deby;
#   dc->MoveTo((int)taby[tabo[j-1]][0],tabh[tabo[j-1]]);
#   dc->LineTo((int)taby[tabo[j-1]][0],y2);
#   if(SigniNode && Node[j-1]>0)
#   {
#     dc->MoveTo((int)taby[tabo[j-1]][0]+2,y2);
#     newPen=new CPen(PS_SOLID /*| PS_INSIDEFRAME*/,5,RGB(255,0,0));
#     oldPen2=dc->SelectObject(newPen);
#     dc->LineTo((int)taby[tabz[j-1]][0]-2,y2);
#     dc->MoveTo((int)taby[tabz[j-1]][0],y2);
#   }
#   else
#     dc->LineTo((int)taby[tabz[j-1]][0],y2);
#   if(SigniNode && Node[j-1]>0)
#   {
#     dc->SelectObject(oldPen2);
#     delete newPen;
#   }
#   dc->LineTo((int)taby[tabz[j-1]][0],tabh[tabz[j-1]]);
#   taby[tabo[j-1]][0]=(taby[tabo[j-1]][0]+taby[tabz[j-1]][0])/2;
#   taby[tabz[j-1]][0]=taby[tabo[j-1]][0];
#   tabh[tabo[j-1]]=y2;
#   tabh[tabz[j-1]]=y2;
#   level[tabo[j-1]]=-1;
#   level[tabz[j-1]]=-1;
#   i=2;
#   for (u=0;u<nb_col;u++)
#   {
#     if(Item[u] && level[u]==j-1)
#     {
#       dc->MoveTo((int)taby[u][0],tabh[u]);
#       dc->LineTo((int)taby[u][0],dy*f+deby);	
#     }
#   }
# }
# dc->SelectObject(OldFont);
# 
# 
# 
# 
