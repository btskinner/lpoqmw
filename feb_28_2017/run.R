## run.R

## library
library(MASS)

source('./bootstrapIM.R')
source('./kr_test_normal.R')

## make data
xz <- mvrnorm(n = 200, mu = c(8,5), Sigma = matrix(c(1,0.5,0.5,1),2,2))
x <- xz[,1]
z <- xz[,2]

mu <- 5 * z + x + z^2

y <- rnorm(200, mu)

## make into dataframe
df = data.frame(y = y, x = x, z = z)

## check it
lm(y ~ z + x + I(z^2), df)

## correctly specified
bootstrapIM_normal(as.formula('y ~ z + x + I(z^2)'), df, 100)
bootstrapIM.normal(as.formula('y ~ z + x + I(z^2)'), df, 100)

## incorrectly specified
bootstrapIM_normal(as.formula('y ~ z + x'), df, 100)
bootstrapIM_normal(as.formula('y ~ x + I(x^2)'), df, 100)
