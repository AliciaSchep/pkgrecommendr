// Code adapted from reco package, by Dmitriy Selivanov

#include <RcppArmadillo.h>

#ifdef _OPENMP
#include <omp.h>
#endif

#define GRAIN_SIZE 1
using namespace Rcpp;
using namespace RcppArmadillo;
using namespace arma;

// [[Rcpp::export]]
void als_implicit(const arma::sp_mat& mat, arma::mat& X, arma::mat& XtX, arma::mat& Y, int n_threads) {
  int nc = mat.n_cols;
  #ifdef _OPENMP
  #pragma omp parallel for num_threads(n_threads) schedule(dynamic, GRAIN_SIZE)
  #endif
  for(int i = 0; i < nc; i++) {
    int p1 = mat.col_ptrs[i];
    int p2 = mat.col_ptrs[i + 1];
    // catch situation when some columns in matrix are empty, so p1 becomes equal to p2 or greater than number of columns
    if(p1 < p2) {
      arma::uvec idx = uvec(&mat.row_indices[p1], p2 - p1);
      arma::mat X_nnz = X.cols(idx);

      arma::vec confidence = vec(&mat.values[p1], p2 - p1);
      arma::mat inv = XtX + X_nnz.each_row() % (confidence.t() - 1) * X_nnz.t();
      arma::mat rhs = X_nnz * confidence;

      Y.col(i) = solve(inv, rhs, solve_opts::fast );
    }
  }
}

// [[Rcpp::export]]
double als_loss(const arma::sp_mat& mat, arma::mat& X, arma::mat& Y, double lambda, int n_threads) {
  int nc = mat.n_cols;
  double loss = 0;
  #ifdef _OPENMP
  #pragma omp parallel for num_threads(n_threads) schedule(dynamic, GRAIN_SIZE) reduction(+:loss)
  #endif
  for(int i = 0; i < nc; i++) {
    int p1 = mat.col_ptrs[i];
    int p2 = mat.col_ptrs[i + 1];
    if(p1 < p2) {
      arma::uvec idx = uvec(&mat.row_indices[p1], p2 - p1);
      arma::vec true_val = vec(&mat.values[p1], p2 - p1);
      arma::mat user_i = X.cols(idx);
      // implicit feedback: true_val = confidence
      loss += accu((square( 1 - (Y.col(i).t() * user_i) ) * true_val));
    }
  }
  if(lambda > 0)
    loss += lambda * (accu(square(X)) + accu(square(Y)));
  return loss / accu(mat);
}
