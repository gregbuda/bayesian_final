data {
  int<lower=0> N;
  vector[N] price; 
  vector[N] surface;
  int<lower=1> num_districts;
  int<lower=1, upper=num_districts> district_index[N];
  int ascensor[N];
  int calef[N];
  int aire[N];
  int park[N];
  int piscina[N];
  int mueble[N];
}

parameters {
  real beta_0; 
  real beta_1; 
  real beta_2; 
  real beta_3; 
  real beta_4; 
  real beta_5; 
  real beta_6;
  real beta_7; 

  vector[num_districts] b_raw; // uncentered random effects
  vector<lower=0>[num_districts] sigma_district; 
  real<lower=0> tau;
}

transformed parameters {
  vector[num_districts] b;
  b = tau * b_raw;
}

model {
  // priors
  beta_0 ~ normal(0, 10);
  beta_1 ~ normal(0, 10);
  beta_2 ~ normal(0, 10);
  beta_3 ~ normal(0, 10);
  beta_4 ~ normal(0, 10);
  beta_5 ~ normal(0, 10);
  beta_6 ~ normal(0, 10);
  beta_7 ~ normal(0, 10);
  b_raw ~ normal(0, 1);
  tau ~ normal(0, 10);
  sigma_district ~ normal(0, 10);

  // likelihood
  for (i in 1:N) {
    price[i] ~ normal((beta_0 + b[district_index[i]]) + beta_1 * surface[i] + beta_2 * ascensor[i] + beta_3 * calef[i] + beta_4 * aire[i] + beta_5 * park[i] + beta_6 * piscina[i] + beta_7 * mueble[i], sigma_district[district_index[i]]);
  }
}