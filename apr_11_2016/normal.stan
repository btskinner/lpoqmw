// normal.stan
data {
  int<lower=0> N;
  int<lower=0> K;
  matrix[N,K] x;
  vector[N] y;
}
parameters {
  real alpha;
  vector[K] beta;
  real<lower=0> sigma;
}
model {
  // regression parameter and error prior
  beta ~ normal(0,10);
  sigma ~ cauchy(0,10);

  // likelihood
  y ~ normal(alpha + x * beta, sigma);
}
