#' @title Contains many routines for GUI
#'
#' 
#' @author Rapha\"{e}l Couturier \email{raphael.couturier@@univ-fcomte.fr}
#' @export




getOption <- function() {
  optionFile="option.csv"
  if (file.exists(optionFile)){
    my.option = read.table(file=optionFile,head=FALSE,sep=",",stringsAsFactors=F)
    print(my.option)
  }
  else {
    fields=c("computation","other")
    values=c("Classic","other2")
    my.option=data.frame(fields,values,stringsAsFactors=FALSE)
  }
}

