
#include <Rcpp.h>
#include <bgc.h>

using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_hello_world() {

    int ret = bgc(NULL, NULL, 0);

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}
