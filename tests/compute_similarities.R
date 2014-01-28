library(rchic)
source('R/callAsirules.R')


rules=read.table(file='transaction.out',header=TRUE,row.names=1,sep=',')
n=dim(rules)[1]


listVariables=strsplit(row.names(rules),split=' -> ')
lVariables=character(0)
for(i in 1:n) {
  
  
    from=listVariables[[i]][1]
    to=listVariables[[i]][2]
    lVariables=c(lVariables,from,to)
  
}
lVariables=unique(lVariables)

similarity_df=data.frame()

for(i in 1:n) {
  rule=strsplit(row.names(rules)[i],split=' -> ')
  from=rule[[1]][1]
  to=rule[[1]][2]
  val=rules[i,7]
  similarity_df[from,to]=val
}

similarity_df[is.na(similarity_df)]=0 

#convert to matrix
similarity_matrix=data.matrix(similarity_df/100)  #we need to use values between 0 and 1

callSimilarityComputation(similarity_matrix)
