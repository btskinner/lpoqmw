## code modified from http://dx.doi.org/10.7910/DVN/26935

ll_normal_bsIM <- function(beta, y, X, sigma2){
    -1/2 * (sum(log(sigma2) + (y -(X%*%beta))^2/sigma2))
}

calc_gradient <- function(y, X, beta, sigma2) {
    data <- cbind(y, X)

    apply(data, 1, function(obs) {
        numericGradient(ll_normal_bsIM,
                        beta,
                        y = obs[1],
                        X = obs[2:length(obs)],
                        sigma2 = sigma2)
    })
}

make_D <- function(formula, data) {

    ## get linear model fit
    fit <- lm(formula, data)

    ## your Xs
    X <- model.matrix(fit)

    ## outcome
    y <- data[,c(paste(formula[[2]]))]

    ## get N and K
    N <- length(y)
    K <- ncol(X)

    ## get beta and Xbeta (== mu)
    beta <- fit$coefficients
    mu <- X %*% beta

    ## get sigma^2_hat
    sigma2 <- sum(fit$residuals^2)/(N-K)

    ## calculate the gradient for each observation
    grad <- calc_gradient(y, X, beta, sigma2)

    ## get Dhat by building 'sandwich': -P^-1 M -P^-1
    meat <- grad %*% t(grad)                 # == M
    bread <- -solve(vcov(fit))               # == -P^-1
    matri <- nrow(X)^(-1/2)*(meat + bread)   # == full D matrix
    Dhat <- as.vector(diag(matri))           # get the diagonal elements

    return(list('N' = N,
                'K' = K,
                'mu' = mu,
                'sigma2' = sigma2,
                'Dhat' = Dhat))
}


bootstrapIM_normal <- function(formula, data, B, B2 = B){

    require(maxLik)

    init <- make_D(formula, data)

    ## init list for D values
    D <- list()
    Dbar <- rep(0, length(init[['Dhat']]))

    ## loop through bootstraps to create Vb
    for(i in 1:B){

        cat(paste('\rNow on iteration:', i, 'of', B))
        if (i == B) cat('\n')

        ## simulate some values from world in which mu & sigma are true
        y_B <- rnorm(init[['N']], init[['mu']], sqrt(init[['sigma2']]))

        df_1 = data
        data$y = y_B

        ## make bootstrapped Dhat
        boot <- make_D(formula, df_1)

        ## store D in list
        D[[i]] <- boot[['Dhat']]

        ## store Dbar value (will divide at the end)
        Dbar <- D[[i]] + Dbar

        ## init lists for bootstraps for simulation variance
        DBbar <- rep(0, length(boot[['Dhat']]))
        DB <- list()

        ## ...you're going to need to bootstrap VBb
        for(j in 1:B2){

            ## simulate some values from world in which mu_B & sigma_B are true
            y_B2 <- rnorm(boot[['N']], boot[['mu']], sqrt(boot[['sigma2']]))

            df_2 = df_1
            df_2$y = y_B2

            ## make bootstrapped-bootstrapped Dhat
            boot_boot <- make_D(formula, df_2)

            ## store D in list
            DB[[j]] <- boot_boot[['Dhat']]

            ## store DBbar value (will divide at the end)
            DBbar <- DB[[j]] + DBbar
        }

        ## divide now!
        DBbar <- DBbar / B2

        ## init VBb matrix (should be K x K square matrix)
        VBb <- matrix(0, nrow = length(DBbar), ncol = length(DBbar))

        ## build VBb (...surely a better way)
        for(j in 1:B2){
            VBb <- VBb + (DB[[j]] - DBbar) %*% t(DB[[j]] - DBbar)
        }

        ## divide now!
        VBb <- VBb / (B2-1)

        ## invert matrix
        invVBb <- solve(VBb)

        ## calculate Test statistic for later p-value distribution comparison
        T[i] <- t(D[[i]]) %*% invVBb %*% D[[i]]

    }

    ## divide now!
    Dbar <- Dbar / B

    ## init Vb matrix (again, should be K x K square matrix)
    Vb <- matrix(0, nrow = length(Dbar), ncol = length(Dbar))

    ## build Vb
    for(i in 1:B){
        Vb <- Vb + (D[[i]] - Dbar) %*% t(D[[i]] - Dbar)
    }

    ## divide
    Vb <- Vb / (B - 1)

    ## invert
    invVb <- solve(Vb)

    ## finally, our omega test statistic
    omegaB <- t(init[['Dhat']]) %*% invVb %*% init[['Dhat']]
    ## print('omegaB')
    ## print(omegaB)

    ## T ~ chisq(K) so (# times omegaB > T) is approx probability
    ## that omegaB != null of no difference between conventional
    ## and robust errors...I think...
    pb <- (B + 1 - sum(T < as.numeric(omegaB))) / (B + 1)

    message(paste('omega:', omegaB))
    message(paste('pval :', round(pb, 3))

    return(list(stat = omegaB, pval = pb))
}

