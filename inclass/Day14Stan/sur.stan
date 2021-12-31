data {
  int<lower=0> N;
  vector[2] y[N];
  vector[N] x1;
  vector[N] x2;
} parameters {
  real beta1;
  real beta2;
  real beta01;
  real beta02;
  cholesky_factor_corr[2] L_Omega;
  vector<lower=0>[2] L_sigma;
} model {
  matrix[2,2] L_Sigma;
  row_vector[2] mu[N];
  
  beta1 ~ normal(0,5);
  beta2 ~ normal(0,5);
  beta01 ~ normal(0,5);
  beta02 ~ normal(0,5);

  L_Sigma = diag_post_multiply(L_Omega, L_sigma);

  L_Omega ~ lkj_corr_cholesky(4);
  L_sigma ~ cauchy(0, 2.5);
  
  for(n in 1:N)
    mu[n] = [beta01 + x1[n] * beta1,
      beta02 + x2[n] * beta2];
  
  y ~ multi_normal_cholesky(mu, L_Sigma);
}

