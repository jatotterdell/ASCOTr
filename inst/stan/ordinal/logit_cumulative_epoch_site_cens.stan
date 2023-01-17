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
  int N;          // number of participants
  int K;          // number of design parameters
  int J;          // number of outcome levels
  int M_region;   // number of regions
  int M_site;     // total number of sites across all regions
  int M_epoch;    // number of epochs
  array[N, 2] int y; // outcome level
  matrix[N, K] X; // design matrix, including treatment design, covariates, and eligibilities
  vector[K] beta_sd; // prior for design coefficient parameters
  vector<lower=0> [J] p_par; // dirichlet prior hyper-parameters
  array[M_site] int<lower=1> region_by_site; // region indicator for each site
  array[N] int<lower=1> site;                // site indicator
  array[N] int<lower=1> epoch;               // epoch indicator
}

parameters {
  simplex[J] p0;
  vector[K] beta_raw;                 // design coefficients
  vector[M_site] epsilon_site;        // site coefficients
  vector[M_epoch-1] epsilon_epoch;    // epoch coefficients, first term = 0.
  vector<lower=0>[M_region] tau_site; // region-specific site sd
  real<lower=0> tau_epoch;            // cohort sd
}

transformed parameters {
  ordered[J-1] alpha = logit(cumulative_sum(p0[1:(J-1)]));
  vector[K] beta = beta_sd .* beta_raw;
  vector[M_site] gamma_site;
  vector[M_epoch] gamma_epoch;
  // use country specific site variation
  for(m in 1:M_site) {
    gamma_site[m] = tau_site[region_by_site[m]] * epsilon_site[m];
  }
  // define random-walk(1) prior
  gamma_epoch[1] = 0.0;
  gamma_epoch[2:M_epoch] = tau_epoch * cumulative_sum(epsilon_epoch);
}

model {
  // lp
  vector[N] eta =
    X * beta + gamma_site[site] + gamma_epoch[epoch];
  // prior
  target += dirichlet_lpdf(p0 | p_par)
          + std_normal_lpdf(beta_raw)
          + std_normal_lpdf(epsilon_epoch)
          + std_normal_lpdf(epsilon_site)
          + student_t_lpdf(to_vector(tau_site) | 3, 0, 1) - M_region*student_t_lccdf(0 | 3, 0, 1)
          + student_t_lpdf(tau_epoch | 3, 0, 1) - student_t_lccdf(0 | 3, 0, 1);
  // likehoood
  target += cens_ordered_logistic_lpmf(y | eta, alpha);
}

generated quantities {
  array[N] int y_ppc;
  vector[N] eta =
    X * beta + gamma_site[site] + gamma_epoch[epoch];
  for(n in 1:N) {
    if (y[n, 1] == y[n, 2]) {
      y_ppc[n] = ordered_logistic_rng(eta[n], alpha);
    } else {
      y_ppc[n] = 99;
    }
  }
}
