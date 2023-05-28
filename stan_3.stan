data {
  int<lower=0> N; 
  vector[N] price;
  vector[N] surface; 
  int<lower=1> num_districts;
  int<lower=1, upper=num_districts> district_index[N];
}

parameters {
  real beta_0;
  real beta_1; 
  vector[num_districts] b_raw; 
  real<lower=0> tau; 
  vector<lower=0>[num_districts] sigma_district;
}

transformed parameters {
  vector[num_districts] b;
  b = tau * b_raw;
}

model {
  beta_0 ~ normal(0, 10);
  beta_1 ~ normal(0, 10);
  b_raw ~ normal(0, 1);
  tau ~ normal(0, 10);
  sigma_district ~ normal(0, 10);

  for (i in 1:N) {
    price[i] ~ normal((beta_0 + b[district_index[i]]) + (beta_1 * surface[i]) , sigma_district[district_index[i]]);
  }
}