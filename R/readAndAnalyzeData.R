#' @title Read and analyze data (preprocessing of data)
#'
#' @description Partitions data that require that
#' 
#' @param   dataCSV   dataframe of the initial data
#' @param   ask_value  if true, user can choose the number of partition 
#' (otherwise it is fixed to 3 by default)
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export



readAndAnalyzeData <- function (dataCSV, ask_value=FALSE) {




#function to partition variables
#partitionned variables finished with "space"p
split_variables <- function(variable) {
  name=names(variable)
  res=grepl("\\.p$", name)
  #if variable need partitionning
  if(res==TRUE) {
    #remove "space"p
    name=gsub("\\.p$","",name) 
    
    if(ask_value==TRUE) {
      print(paste("number of partition for variables ",name))
      nb.partitions=scan(n=1)
    }
    else {
      nb.partitions=3
    }
    print(name)
    #call kmeans to partition the variable
    #cl=kmeans(variable,nb.partitions)
    cl=callDynamicCloud(as.numeric(c(t(variable))),as.integer(nb.partitions))
    
    #nb of elements
    len=length(cl)
    
    #function used to split the variable after the partitionning
    f1 <- function(i,len) {
      t=rep(0,len)
      #t[which(cl$cluster==i)]=1  
      t[which(cl==i)]=1  
      f1=t
    }
    
    #transform the list of clustering in a list of partitionned variables
    split=lapply(1:nb.partitions,function(i) f1(i,len) )
    #create the corresponding dataframe 
    df=data.frame(matrix(unlist(split), nrow=nrow(variable), byrow=F))
    #add the name of variables
    names(df)=lapply(1:nb.partitions,function(i) paste(name,as.character(i)))
    
    split_variables=df
  }
  else
  {
    #if variable don't need partitionning, we simply return it
    split_variables=variable
  }
    
}

#read of the file
dataCSV = read.csv(fileName,sep=";",as.is=T)
#determine the type of df
res=lapply(dataCSV,is.numeric)
#remove 1st element of the list
res=res[-1]
if(any(res==FALSE)) {
  dataCSV = read.csv2(fileName,sep=";",as.is=T)
  print("READING THE FILE WITH EUROPEAN FORMAT")
}

#partition all the variables one by one
result=lapply(1:ncol(dataCSV),function(i) split_variables(dataCSV[i]))

#create a new dataframe with the partitionned variables
data2=data.frame(result)
#print(data2)
analyseData = data2
}