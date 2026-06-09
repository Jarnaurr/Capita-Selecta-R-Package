#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List bootstrap_cpp(NumericVector X, int B, std::string dist,
                   double param1 = NA_REAL, double param2 = NA_REAL) {
  int n = X.size();
  NumericVector bootstrap_means(B);
  NumericVector bootstrap_sds(B);

  if (dist == "NonParam") {
    for (int i = 0; i < B; i++) {
      NumericVector bs_sample = Rcpp::sample(X, n, true);
      bootstrap_means[i] = mean(bs_sample);
      bootstrap_sds[i]   = sd(bs_sample);
    }
  } else if (dist == "Norm") {
    for (int i = 0; i < B; i++) {
      NumericVector bs_sample = rnorm(n, param1, param2);
      bootstrap_means[i] = mean(bs_sample);
      bootstrap_sds[i]   = sd(bs_sample);
    }
  } else if (dist == "Exp") {
    for (int i = 0; i < B; i++) {
      NumericVector bs_sample = rexp(n, param1);
      bootstrap_means[i] = mean(bs_sample);
      bootstrap_sds[i]   = sd(bs_sample);
    }
  } else {
    stop("dist must be 'NonParam', 'Norm', or 'Exp'");
  }

  return List::create(
    Named("bootstrap_means") = bootstrap_means,
    Named("bootstrap_sds")   = bootstrap_sds
  );
}
