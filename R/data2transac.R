#' @title Saves the data to a transaction file.
#'
#' @description Saves the data to a transaction file.
#' 
#' @param 	data 	Matrix of data.
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export

data2transac <- function(data) {
  
  n = dim(data)[1]
  if(variablep==0)
  {   
    data=data[,2:dim(data)[2]]
  }
  #we suppress the first column
 
  unlink('transaction.tab')
  
  
  for( i in 1:n ) {
    nonzero = (data[i,] != 0)
    write(file='transaction.tab', x=paste( names(data)[nonzero], data[i,][nonzero], sep=' ', collapse=' '), sep='\n', append=TRUE)
  }
}