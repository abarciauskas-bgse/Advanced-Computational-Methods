#include <Rcpp.h>
#include <math.h>
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

// [[Rcpp::export]]
// Credit: http://adv-r.had.co.nz/Rcpp.html
NumericVector rowSumsC(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(nrow);
  
  for (int i = 0; i < nrow; i++) {
    double total = 0;
    for (int j = 0; j < ncol; j++) {
      total += x(i, j);
    }
    out[i] = total;
  }
  return out;
}

// [[Rcpp::export]]
NumericVector calcDist(NumericMatrix x, NumericMatrix y, int p) {
  NumericMatrix xx(x.nrow(), x.ncol());
  for (int i=0; i<x.ncol(); ++i) {
    xx(_,i) = pow(x(_,i)-y(_,i), p);
  }
  float power = 1/(float)p;
  return pow(rowSumsC(xx),power);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//
// 
// /*** R
// timesTwo(42)
// */
