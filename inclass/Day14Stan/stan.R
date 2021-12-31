library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
setwd('~/QPMRFall2021/inclass/day14Stan/')

set.seed(123)
x = rnorm(500)
y = 1.5*x - 3 + rnorm(500)

to_stan = list(
  N = length(y),
  x = x,
  y = y
)

model = stan_model('linear.stan')
mod1 = rstan::sampling(model,
            data = to_stan,
            chains = 2,
            iter = 1000,
            seed = 1453)

#or, alternatively:
#mod1 = stan('linear.stan',
#            data = to_stan,
#            chains = 2,
#            iter = 1000,
#            seed = 1453)

summary(mod1)$summary
summary(summary(mod1)$summary[, 'Rhat'])
plot(mod1)
traceplot(mod1)

set.seed(123)
N = 500
J = 5
x = rnorm(N)
group.ef = rep(rnorm(J), each = N/J)
y = 1.5*x - 3 + group.ef + rnorm(N)
j = rep(1:J, each = N/J)

to_stan = list(
  N = N,
  J = J,
  x = x,
  y = y,
  j = j
)

model2 = stan_model('hier.stan')
mod2 = stan(model2,
            data = to_stan,
            chains = 2,
            iter = 1000,
            seed = 1453)

summary(mod2)$summary
summary(summary(mod2)$summary[, 'Rhat'])
plot(mod2)
traceplot(mod2)

#TODO: create a Stan file that runs a pooled logit (hint, bernoulli), and run it on voted in cses4_cut.csv (codebook in folder, use any feature engineering you want)
#TODO: alter the logit to allow a random intercept for region, and at least one varying slope for race

#seemingly unrelated regression (SUR) - powerful tool, and is a preliminary for more complex DGPs
set.seed(123)
x1 = rnorm(1000)
x2 = rnorm(1000)
mu_1 = 1.5 * x1 + 3 + rnorm(1000)
mu_2 = -5 * x2 + 2 + rnorm(1000)
library(MASS)
y = matrix(ncol = 2)
for(i in 1:1000) y = rbind(y, as.vector(mvrnorm(1, c(mu_1[1], mu_2[1]), Sigma = matrix(c(1,.25,.25,1), nrow = 2))))
y = y[-1, ]

to_stan = list(
  N = 1000,
  x1 = x1,
  x2 = x2,
  y = y
) 

model3 = stan_model('sur.stan')
mod3 = stan(model3,
            data = to_stan,
            chains = 2,
            iter = 1000,
            seed = 1453)
summary(summary(mod2)$summary['Rhat'])

plot(density(unlist(extract(mod2, pars = 'L_Omega[2,1]'))))

#TODO: plot other things of interest

