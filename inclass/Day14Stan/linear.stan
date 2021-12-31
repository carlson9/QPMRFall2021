data {
  int<lower=0> N;
  vector[N] y;
  vector[N] x;
}
parameters {
  real beta0;
  real beta1;
  real<lower=0> sigma;
}
transformed parameters {
  vector[N] mu;
  mu = beta0 + beta1 * x;
}
model {
  beta0 ~ normal(0, 10);
  beta1 ~ normal(0, 10);
  sigma ~ inv_gamma(1, 1);
  
  y ~ normal(mu, sigma);
}
generated quantities{
  real y_sim[N];
  for(n in 1:N){
    y_sim[n] = normal_rng(mu[n], sigma);
  }
}

