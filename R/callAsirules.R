callAsirules <- function() {
  #library(rchic)
  
#  library.dynam("rchic",package=c("rchic"))
 # dyn.load( file.path(".", paste("rchic", .Platform$dynlib.ext, sep="")) )
  
  
  b<-c("rchic","-l","-s0","-m1","-n2","-c2",'transaction.tab',"transaction.out")
  a<-length(b)
  .C("asirules",as.integer(a),as.character(b))
}