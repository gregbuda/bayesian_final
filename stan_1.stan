data {
  int<lower=0> N;
  vector[N] price;
  vector[N] no_rooms;
  vector[N] surface;
  vector[N] bathroom;
  //int<lower=1> num_districts;
  //int<lower=1, upper=num_districts> district_index[N];
}

parameters {
  real beta_0;
  real beta_1;
  real beta_2;
  real beta_3;
  //vector[num_districts] b;
  real<lower=0> sigma;
  //real<lower=0> tau;
}

model {
  // priors
  beta_0 ~ normal(0, 10);
  beta_1 ~ normal(0, 10);
  beta_2 ~ normal(0, 10);
  beta_3 ~ normal(0, 10);
  //tau ~ normal(0, 10);
  sigma ~ normal(0, 10);

  // likelihood
  for (i in 1:N) {
    // price[i] ~ normal((beta_0 + b[district_index[i]]) + (beta_1 * no_rooms[i]) + (beta_2 * surface[i])  + (beta_3 * bathroom[i]), sigma);
    //b[district_index[i]] ~ normal(0, tau);
    price[i] ~ normal((beta_0) + (beta_1 * no_rooms[i]) + (beta_2 * surface[i])  + (beta_3 * bathroom[i]), sigma);
}
}


generated quantities {
  real log_lik[N];
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(price[i] | beta_0 + beta_1 * no_rooms[i] + beta_2 * surface[i] + beta_3 * bathroom[i], sigma);
  }
}
