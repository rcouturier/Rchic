#' @title Contains many routines for GUI
#'
#' 
#' @author Raphael Couturier 
#' @export




getMyOption <- function() {
  optionFile="option.csv"
  if (file.exists(optionFile)){
    my.option = read.table(file=optionFile,head=FALSE,sep=",",stringsAsFactors=F)
    print(my.option)
  }
  else {
    fields=c("computation","complete.graph","verbose")
    values=c("Classic","0","0")
    my.option=data.frame(fields,values,stringsAsFactors=FALSE)
  }
}

