#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

int asirules(int*, char**);

// [[Rcpp::export]]
void call_apriori(int nb,StringVector  s) {
  
  
  //std::cout<<nb<<std::endl;
  //std::cout<<s.size()<<std::endl;
  char **argv=new char*[nb];
  for(int i=0;i<nb;i++) {
    argv[i]=new char[s[i].size()+1];
    strcpy(argv[i],s[i]);
    //std::cout<<argv[i]<<std::endl;
  }
  asirules(&nb, argv);
}


