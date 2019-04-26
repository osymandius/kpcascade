
data {
  int<lower=0> N;
  vector[N] prev;
  vector[N] se;
  // real prior_mu;
  // real prior_tau;
  // real low;
  // real high;
}

parameters {
  // real<lower=low, upper=high> theta;
  real theta;
}

model {
  prev ~ normal(theta, se);
  // theta ~ normal(prior_mu, prior_tau);
}

