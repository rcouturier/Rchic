#' @title Saves the data to a transaction file.
#'
#' @description Saves the data to a transaction file.
#' 
#' @param 	data 	Matrix of data.
#' 
#' @author Raphael Couturier
#' @export

data2transac <- function(data) {
  
  n = dim(data)[1]
  #   if(variablep==0)
  #   {   
  #     data=data[,2:dim(data)[2]]
  #   }
  #we suppress the first column
  
  unlink('transaction.tab')

  callWriteTransactions(data)
  
  
  
  
}