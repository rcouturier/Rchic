############################################################
#############    COMPUTE THE HIERARCHY TREE    ############
############################################################

library(stringr)
require(tcltk) || stop("tcltk support is absent")

#select file
fileName <- tclvalue(tkgetOpenFile())

if (!nchar(fileName)) {
  tkmessageBox(message = "No file was selected!")
  setwd(initDirectory)
} else{
  
  variablep<-0
  
  #loading of all functions
  
  #read of the file
  dataCSV<-read.csv(fileName,sep=";")
  
  data1=dataCSV[,2:dim(dataCSV)[2]]
  
  list.variables1=names(data1)# on récupére les noms des variables
  
  # tester si les variables on a la fin " p"
  
  grep("\\.p$", list.variables1, value=TRUE) 
  
  
  liste=list.variables1[grepl("\\.p$", list.variables1)]
  
  # suprimer .p
  liste<-gsub("\\.p","",liste) 
  list.variables1<-gsub("\\.p","",list.variables1) 
  
  
  result<-list()
  
  liste1<- list()
  
  # si les variables contiennent " p" a la fin alors transformer le fichier avant de continuer
  
  if(setequal(list.variables1,liste)==TRUE )
  {
    variablep<-1
    
    #########################le chois du nombre d'occurence de chaque variable##########################
    
    top <- tktoplevel()
    tktitle(top)<-"Nombre de partitions"
    myvalue<<-list(tclVar(3))
    
    label.text <- tclVar(liste[1]) 
    label <- tklabel(top, text = tclvalue(label.text)) 
    tkconfigure(label, textvariable = label.text) 
    tkgrid(label) 
    
    listederoulante<-tkwidget(top,"spinbox", from=1, to=9, increment=1,width=3, textvariable=myvalue[[1]])
    
    nbvariables<-length(liste)
    
    f<-length(liste)+1
    s<-1
    l<-s-1
    s4<- 0
    
    var1<-0
    m <- 1
    result <- list()
    
    n=dim(dataCSV)[1]
    
    
    OnOK <- function()
    {
      
      val=as.numeric(tclvalue(myvalue[[1]]))
      
      km1=data1[][s]
      cl <<- kmeans(km1, val, iter.max=10) 
      
      
      if(s==1)
      {
        m<<- 1
        var1<<-val
      }
      print("Appel a la fonction on ok")
      if(s>1)
      { 
        m<<- var1+1     
        var1<<-var1+val
      }
      
      for(k1 in m:var1) { 
        s3 <<- k1-s4
        liste1[k1]<<-paste0(liste[s],s3)
        result[[k1]]<<-cl$cluster
        
        for(k2 in 1:n) { 
          
          if(result[[k1]][k2]== s3)
          {
            result[[k1]][k2]<<- 1
          }
          else
          {
            result[[k1]][k2]<<- 0
          }
        }  
        
      }
      s4<<-s4+val
      
      s <<-s+1
      l <<-l+1
      
      tclvalue(label.text) <- liste[s]
      
      
      if(s==f)
      {
        
        tkdestroy(top)
        
        # creer la matrice dont laquelle on vas mettre les données 0 ou 1 
        
        data2<<- matrix(0,n,var1,dimnames=list(c(),c(liste1) ) )
        
        
        for(k3 in 1:var1) { 
          for(k4 in 1:n) { 
            data2[k4,k3]<<- result[[k3]][k4]
            
          }
        }
        
        result <-list() 
        
        list.variables<<-gsub("\\.p","",liste1) 
        
        unlink('file.csv')
        
        write.table(data2,"file.csv",sep=";",dec=",")
        
        data3<<-read.csv(file="file.csv",sep=";")
        
        
        data2transac(data3) 
        callAsirules()#formation de transaction.out apartir de transaction.tab on fait appel a l'algorithme apriori pour calculer les régle d'associations les occurence etc.

        verbose=FALSE
        hierarchyTree(list.variables,verbose=verbose)
        
      }
      
    }
    
    bouton1 <- tkbutton(top,text="OK",command=OnOK)
    
    tkpack(label,listederoulante,bouton1)
    
    
    
    
    
  }
  # Si le fichier ne contient pas des variables de partitionnement
  
  else
  {
    variablep<-0
    
    dataCSV=dataCSV[,2:dim(dataCSV)[2]]
    
    data2transac(dataCSV)
    callAsirules()
    list.variables=names(dataCSV)
    list.variables=list.variables[-1]
    #list of variables is needed to keep the same order in the variable when the cohesion matrix is built
    verbose=FALSE
    hierarchyTree(list.variables,verbose=verbose)
  }
}

  
  
  