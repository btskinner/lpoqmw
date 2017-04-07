// vi_normal.stan
data {
  int<lower=1> N;
  int<lower=1> J;
  int<lower=1> K;
  matrix[N,K] x;
  vector[N] y;
  int school[N];
}
parameters {
  real alpha_mu;
  vector[J] alpha;
  vector[K] beta;
  real<lower=0> sigma;
  real<lower=0> alpha_sigma;
}
model {
  // hyper priors
  alpha_mu ~ normal(0,1);
  alpha_sigma ~ cauchy(0,2.5);

  // random intercept prior
  alpha ~ normal(alpha_mu, alpha_sigma);

  // regression parameter and error prior
  beta ~ normal(0,10);
  sigma ~ cauchy(0,10);

  // likelihood
  y ~ normal(alpha[school] + x * beta, sigma);
}
