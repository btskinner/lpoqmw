################################################################################
##
## A (very) short primer on Bayesian vs. Frequentist regression
## LPO Quantitative Methods Colloquium
## 11 April 2017
## Benjamin Skinner
##
################################################################################

## clear
rm(list = ls())

## libraries
libs <- c('tidyverse','MASS','lme4','plotly','rstan','rstanarm','shinystan')
lapply(libs, require, character.only = TRUE)
options(width = 100)

## -----------------------------------------------------------------------------
## Analytic Bayesian solution
## -----------------------------------------------------------------------------

## data (k = success; n = total trials)
x <- c(rep(1, 250), rep(0, 150))
k <- sum(x == 1)
n <- length(x)

## prior hyperparameters (a = success; b = failure)
a <- 100
b <- 100

# ## prior, likelihood, posterior: random draws
prior <- rbeta(n, a, b)
likelihood <- rbinom(n, n, k/n)
posterior <- rbeta(n, a + k, b + n - k)

## put into data frame
a_bayes <- data.frame(theta = c(prior, likelihood / n, posterior),
                      distribution = c(rep('prior', length(prior)),
                                       rep('likelihood', length(likelihood)),
                                       rep('posterior', length(posterior)))) %>%
    mutate(distribution = as.factor(distribution))

## plot
g <- ggplot(a_bayes, aes(x = theta, fill = distribution)) +
    geom_density(alpha = 0.25, adjust = 2) +
    ylab('') +
    xlab(expression(theta))
g

## -----------------------------------------------------------------------------
## 3D visualization of bivariate density
## -----------------------------------------------------------------------------

## create data that are distributed bivariate normal
mu <- c(0,0)
Sigma <- matrix(c(1,0,0,1),2,2)
beta <- mvrnorm(200, mu, Sigma)
dens <- kde2d(beta[,1], beta[,2], n = nrow(beta))

## interactive plot
axx <- list(title = 'beta_1')
axy <- list(title = 'beta_2')
axz <- list(title = 'Joint density')
p <- plot_ly(x = dens$x, y = dens$y, z = dens$z) %>%
    add_surface() %>%
    layout(scene = list(xaxis = axx, yaxis = axy, zaxis = axz))
p

## -----------------------------------------------------------------------------
## Bayesian vs. Frequentist regression
## -----------------------------------------------------------------------------

## read data
df <- read_csv('tn_star_k.csv')
head(df)

## -------------------------------------
## frequentist linear model
## -------------------------------------

freq_lm <- lm(math_std ~ t_smc + t_rga + female + black + asian
              + hispc + natam + orace + incit + subur + urban
              + frpl + spced,
              data = df)
summary(freq_lm)

## -------------------------------------
## Bayesian linear model
## -------------------------------------

## show Stan model
writeLines(readLines('normal.stan'))

## compile Stan model
norm_mod <- stan_model(file = 'normal.stan')

## set up Stan data in list
y <- df$math_std
x <- df %>%
    select(t_smc, t_rga, female, black, asian,
           hispc, natam, orace, incit, subur, urban,
           frpl, spced) %>%
    as.matrix(.)
N <- nrow(x)
K <- ncol(x)

stan_df <- list('y' = y, 'x' = x, 'N' = N, 'K' = K)

## Bayesian linear model using rstan
bayes_lm_1 <- sampling(norm_mod,
                       data = stan_df,
                       iter = 1000,
                       chains = 3,
                       cores = 3,
                       pars = c('alpha','beta','sigma','lp__'))
print(bayes_lm_1)

## Bayesian linear model using rstanarm
bayes_lm_2 <- stan_glm(math_std ~ t_smc + t_rga + female + black + asian
                       + hispc + natam + orace + incit + subur + urban
                       + frpl + spced,
                       data = df,
                       family = gaussian(link = 'identity'),
                       chains = 3,
                       cores = 3,
                       iter = 1000)
summary(bayes_lm_2, digits = 2)

## check convergence using ShinyStan
## launch_shinystan(bayes_lm_1, quiet = TRUE)

## -------------------------------------
## frequentist varying intercept model
## -------------------------------------

freq_vi <- lmer(math_std ~ -1 + t_smc + t_rga + female + black + asian
                + hispc + natam + orace + incit + subur + urban
                + frpl + spced + (1 | school),
                data = df)
summary(freq_vi)

## -------------------------------------
## Bayesian varying intercept model
## -------------------------------------

## show Stan model
writeLines(readLines('vi_normal.stan'))

## compile Stan model
vi_norm_mod <- stan_model(file = 'vi_normal.stan')

## add information for school-level varying intercepts
school <- df$school
J <- length(unique(school))

stan_df <- list('y' = y, 'x' = x, 'N' = N, 'K' = K, 'J' = J, 'school' = school)

## Bayesian varying intercept model using rstan
bayes_vi_1 <- sampling(vi_norm_mod,
                       data = stan_df,
                       iter = 2000,
                       chains = 3,
                       cores = 3,
                       pars = c('alpha_mu','alpha','beta',
                                'alpha_sigma','sigma','lp__'))
print(bayes_vi_1)

## Bayesian varying intercept model using rstanarm
bayes_vi_2 <- stan_glmer(math_std ~ -1 + t_smc + t_rga + female + black + asian
                         + hispc + natam + orace + incit + subur + urban
                         + frpl + spced + (1 | school),
                         data = df,
                         family = gaussian(link = 'identity'),
                         chains = 3,
                         cores = 3,
                         iter = 2000)
summary(bayes_vi_2, digits = 2)

## check convergence using ShinyStan
## launch_shinystan(bayes_vi_1, quiet = TRUE)

## -----------------------------------------------------------------------------
## END
################################################################################
