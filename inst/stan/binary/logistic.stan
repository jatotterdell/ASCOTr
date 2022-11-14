// Simple logistic regression model for Bernoulli outcome

data {
  int N; // number of observations
  int K; // number of design parameters
  matrix[N, K] X; // design matrix
  array[N] int y; // outcome response
  vector[K] beta_sd; // prior standard deviation for beta parameters
}

parameters {
  vector[K] beta_raw;
}

transformed parameters {
  vector[K] beta = beta_sd .* beta_raw;
}

model {
  // lp
  vector[N] eta = X * beta;
  // prior
  target += std_normal_lpdf(beta_raw);
  // likelihood
  target += bernoulli_logit_lpmf(y | eta);
}
