data {
  int<lower=0> N; 
  vector[N] price; 
  vector[N] surface; 
}

parameters {
  real beta_0;
  real beta_1; 
  real<lower=0> sigma;
}

model {
  // priors
  beta_0 ~ normal(0, 10);
  beta_1 ~ normal(0, 10);
  sigma ~ normal(0, 10);

  // likelihood
  for (i in 1:N) {
    price[i] ~ normal((beta_0) + (beta_1 * surface[i]), sigma);
}
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(price[i] | beta_0 + beta_1 * surface[i], sigma);
  }
}
