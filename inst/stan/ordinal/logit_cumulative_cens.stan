// cumulative logistic model for ordinal outcome

functions {

  real known_y_contribution(int k, int K, real eta, vector c) {
    real ll;
    if(k == 1) {
      // ll = log1m_exp(-log1p_exp(-(eta - c[1])));
      ll = log1m_inv_logit(eta - c[1]);
    } else if(k == K) {
      // ll = -log1p_exp(-(eta - c[K-1]));
      ll = log_inv_logit(eta - c[K-1]);
    } else {
      // ll = log_diff_exp(-log1p_exp(-(eta - c[k-1])), -log1p_exp(-(eta - c[k])));
      ll = log(inv_logit(eta - c[k-1]) - inv_logit(eta - c[k]));
    }
    return ll;
  }

  real censored_y_contribution(int a, int b, int K, real eta, vector c) {
    real ll;
    if(a == 1 && b == K) {
      ll = 0.; // no information
    } else if (a == b) {
      ll = known_y_contribution(a, K, eta, c);
    } else if (a == 1) {
      ll = log1m_exp(-log1p_exp(-(eta - c[b])));
    } else if (b == K) {
      ll = -log1p_exp(-(eta - c[a-1]));
    } else {
      ll = log_diff_exp(-log1p_exp(-(eta - c[a-1])), -log1p_exp(-(eta - c[b])));
    }
    return ll;
  }

  real cens_ordered_logistic_lpmf(array[,] int k, vector eta, vector c) {
    int N = num_elements(eta);
    int K = num_elements(c) + 1;
    vector[N] ll;
    // for stability, work on log-scale
    for (n in 1:N) {
      ll[n] = censored_y_contribution(k[n, 1], k[n, 2], K, eta[n], c);
    }
    return sum(ll);
  }
}

data {
  int N; // number of observations
  int K; // number of design parameters
  int J; // number of outcome levels
  matrix[N, K] X; // design matrix (no intercept)
  array[N, 2] int y; // outcome level
  vector[K] beta_sd; // prior standard deviation for beta parameters
  vector<lower=0> [J] p_par; // dirichlet prior hyper-parameters
}

parameters {
  vector[K] beta_raw;
  simplex[J] p;
}

transformed parameters {
  vector[K] beta = beta_sd .* beta_raw; // covariate coefficients
  ordered[J-1] alpha = logit(cumulative_sum(p[1:(J-1)])); // logit-scale intercepts
}

model {
  // lp
  vector[N] eta = X * beta;
  // prior
  // - beta ~ Normal(0, beta_sd)
  // - p(eta = 0) ~ Dirichlet(p_par)
  target += std_normal_lpdf(beta_raw) +
            dirichlet_lpdf(p | p_par);
  // likelihood
  target += cens_ordered_logistic_lpmf(y | eta, alpha);
}
