data2transac <- function(data) {
  
  n = dim(data)[1]
  
  #we suppress the first column
  data=data[,2:dim(data)[2]]
  unlink('transaction.tab')
  
  
  for( i in 1:n ) {
    nonzero = (data[i,] != 0)
    write(file='transaction.tab', x=paste( names(data)[nonzero], data[i,][nonzero], sep=' ', collapse=' '), sep='\n', append=TRUE)
  }
}