data {
  int<lower=0> N;
  int<lower=0> J; //number of groups
  vector[N] y;
  vector[N] x;
  int<lower=1, upper=J> j[N]; //group indicator
}
parameters {
  real beta1;
  real<lower=0> sigma;
  vector[J] gamma;
}
transformed parameters {
  vector[N] mu;
  for(i in 1:N){
    mu[i] = beta1 * x[i] + gamma[j[i]];
  }
}
model {
  beta1 ~ normal(0, 10);
  sigma ~ inv_gamma(1, 1);
  gamma ~ normal(0, 10);
  
  y ~ normal(mu, sigma);
}

