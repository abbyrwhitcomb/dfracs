    library(dfracs)

    ## The file m ret 10stocks.txt contains the monthly returns of ten stocks from January 1994 to December 2006. The ten stocks include Apple Computer, Adobe Systems, Automatic Data Processing, Advanced Micro Devices, Dell, Gateway, Hewlett-Packard Company, International Business Machines Corp., Microsoft Corp., and Oracle Corp. The file m sp500ret 3mtcm.txt contains three columns. The second column gives the monthly returns of the S&P 500 index from January 1994 to December 2006. The third column gives the monthly rates of the 3-month Treasury bill in the secondary market, which are obtained from the Federal Reserve Bank of St. Louis and used as the risk-free rate here. Consider portfolios that consist of the ten stocks and allow short selling.

    m_sp500ret <- read.table(system.file("extdata", "m_sp500ret_3mtcm.txt", package = "dfracs"), header = TRUE, skip = 1)
    m_sp500ret$Date <- as.Date(paste0("01-", m_sp500ret$Date), format = "%d-%b-%y")
    m_logret <- read.table(system.file("extdata", "m_logret_10stocks.txt", package = "dfracs"), header = TRUE)

    ## a: Using a single-index model (see Exercise 3.3) for the structured covariance matrix F, calculate the estimate F of F in (4.45).

    returns <- m_logret[, -1]
    market <- m_sp500ret[, 2]
    rf <- m_sp500ret[, 3]

    excess_market <- market - rf
    excess_returns <- sweep(returns, 1, rf)
    n_stocks <- ncol(excess_returns)
    betas <- numeric(n_stocks)
    resid_vars <- numeric(n_stocks)

    for (i in 1:n_stocks) {
      model <- lm(excess_returns[, i] ~ excess_market)
      betas[i] <- coef(model)[2]
      resid_vars[i] <- var(resid(model))
    }

    B <- matrix(betas, ncol = 1)
    sigma_m2 <- var(excess_market)
    D <- diag(resid_vars)

    F_hat <- B %*% t(B) * sigma_m2 + D
    print(F_hat)
    #>           [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]     [,9]    [,10]
    #>  [1,] 2.688024 2.671250 2.664092 2.676353 2.664659 2.667871 2.669894 2.664546 2.665578 2.664138
    #>  [2,] 2.671250 2.662241 2.651326 2.663528 2.651890 2.655087 2.657100 2.651778 2.652805 2.651372
    #>  [3,] 2.664092 2.651326 2.644771 2.656391 2.644784 2.647972 2.649980 2.644672 2.645696 2.644267
    #>  [4,] 2.676353 2.663528 2.656391 2.674575 2.656956 2.660159 2.662176 2.656843 2.657872 2.656437
    #>  [5,] 2.664659 2.651890 2.644784 2.656956 2.648101 2.648536 2.650544 2.645235 2.646259 2.644830
    #>  [6,] 2.667871 2.655087 2.647972 2.660159 2.648536 2.657118 2.653739 2.648423 2.649449 2.648018
    #>  [7,] 2.669894 2.657100 2.649980 2.662176 2.650544 2.653739 2.657816 2.650432 2.651458 2.650026
    #>  [8,] 2.664546 2.651778 2.644672 2.656843 2.645235 2.648423 2.650432 2.646047 2.646147 2.644718
    #>  [9,] 2.665578 2.652805 2.645696 2.657872 2.646259 2.649449 2.651458 2.646147 2.648565 2.645742
    #> [10,] 2.664138 2.651372 2.644267 2.656437 2.644830 2.648018 2.650026 2.644718 2.645742 2.647464

    ## b: The delta in (4.45) suggested by Ledoit and Wolf (2003, 2004) is of the following form. Let fij and sigmaij denote the (i,j)th entry of F and S, respectively, and define... Then delta = min{1,(k, n)+}. Compute the covariance estimate (4.45) with F in (a) and the delta suggested by Ledoit and Wolf, and plot the estimated eﬃcient frontier using this covariance estimate.

    n <- nrow(excess_returns)
    p <- ncol(excess_returns)

    betas <- numeric(p)
    resid_vars <- numeric(p)
    for (i in 1:p) {
      model <- lm(excess_returns[, i] ~ excess_market)
      betas[i] <- coef(model)[2]
      resid_vars[i] <- var(resid(model))
    }

    B <- matrix(betas, ncol = 1)
    sigma_m2 <- var(excess_market)
    F_hat <- B %*% t(B) * sigma_m2 + diag(resid_vars)
    S <- cov(excess_returns)

    S_centered <- scale(excess_returns, center = TRUE, scale = FALSE)
    var_Sij <- matrix(0, p, p)
    for (i in 1:p) {
      for (j in 1:p) {
        var_Sij[i, j] <- var(S_centered[, i] * S_centered[, j]) / n
      }
    }
    delta <- min(1, sum(var_Sij) / sum((F_hat - S)^2))
    Sigma_LW <- delta * F_hat + (1 - delta) * S

    mu <- colMeans(returns)
    targets <- seq(min(mu), max(mu), length.out = 100)
    Sigma_inv <- solve(Sigma_LW)
    A <- cbind(rep(1, p), mu)
    risks <- rets <- numeric(length(targets))

    for (i in seq_along(targets)) {
      b <- c(1, targets[i])
      lambda <- solve(t(A) %*% Sigma_inv %*% A) %*% b
      w <- Sigma_inv %*% A %*% lambda
      rets[i] <- sum(w * mu)
      risks[i] <- sqrt(t(w) %*% Sigma_LW %*% w)
    }
    plot(risks, rets, type = "l",
         xlab = "Risk", 
         ylab = "Expected Return")

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-4-7_files/figure-markdown_strict/unnamed-chunk-2-1.png)


    ## c: Perform PCA on the ten stocks. Using the first two principal components as factors in a two-factor model for F (see Section 3.4.3), estimate F.

    datapca <- scale(returns, center = TRUE, scale = FALSE)
    pca <- prcomp(datapca)

    scores <- pca$x[, 1:2]
    B <- coef(lm(as.matrix(datapca) ~ scores))[-1, ]  
    resid <- datapca - scores %*% B
    D <- diag(diag(cov(resid)))
    F_hat <- t(B) %*% cov(scores) %*% B + D

    print(F_hat)
    #>              AAPL         ADBE          ADP          AMD         DELL          GTW           HP          IBM         MSFT         ORCL
    #> AAPL 0.0045492771 0.0016265949 0.0004279518 0.0034320094 0.0019108729 0.0026496378 0.0007280821 0.0011233328 0.0011306530 0.0015025605
    #> ADBE 0.0016265949 0.0044543592 0.0003703980 0.0018636988 0.0017437041 0.0027860931 0.0004228686 0.0007546065 0.0010254711 0.0010425713
    #> ADP  0.0004279518 0.0003703980 0.0007882798 0.0005387538 0.0004298686 0.0006638147 0.0001193524 0.0002029440 0.0002531975 0.0002775676
    #> AMD  0.0034320094 0.0018636988 0.0005387538 0.0071438193 0.0022416429 0.0024359741 0.0012950483 0.0018115429 0.0013378131 0.0023624627
    #> DELL 0.0019108729 0.0017437041 0.0004298686 0.0022416429 0.0035220840 0.0031983773 0.0005055066 0.0008912443 0.0011867856 0.0012283079
    #> GTW  0.0026496378 0.0027860931 0.0006638147 0.0024359741 0.0031983773 0.0065056598 0.0005885146 0.0011745834 0.0018761061 0.0016577751
    #> HP   0.0007280821 0.0004228686 0.0001193524 0.0012950483 0.0005055066 0.0005885146 0.0023049709 0.0003797376 0.0003010194 0.0004977931
    #> IBM  0.0011233328 0.0007546065 0.0002029440 0.0018115429 0.0008912443 0.0011745834 0.0003797376 0.0014830329 0.0005283867 0.0007554281
    #> MSFT 0.0011306530 0.0010254711 0.0002531975 0.0013378131 0.0011867856 0.0018761061 0.0003010194 0.0005283867 0.0020186643 0.0007275561
    #> ORCL 0.0015025605 0.0010425713 0.0002775676 0.0023624627 0.0012283079 0.0016577751 0.0004977931 0.0007554281 0.0007275561 0.0038527785

    ## d: Using the estimated F in (c) as the shrinkage target in (4.45), compute the new value of delta and the new shrinkage estimate (4.45) of sigma. Plot the corresponding estimated eﬃcient frontier and compare it with that in (b).

    Sigma <- cov(datapca)

    n <- nrow(datapca)
    var_Sij <- outer(1:ncol(Sigma), 1:ncol(Sigma), Vectorize(function(i, j) var(datapca[, i] * datapca[, j]) / n))
    delta <- min(1, sum(var_Sij) / sum((F_hat - Sigma)^2))

    Sigma_shrink <- delta * F_hat + (1 - delta) * Sigma

    mu <- colMeans(returns)
    targets <- seq(min(mu), max(mu), length.out = 100)
    A <- cbind(1, mu)
    Sigma_inv <- solve(Sigma_shrink)

    risks <- numeric(length(targets))
    rets <- numeric(length(targets))
    for (i in seq_along(targets)) {
      lambda <- solve(t(A) %*% Sigma_inv %*% A) %*% c(1, targets[i])
      w <- Sigma_inv %*% A %*% lambda
      rets[i] <- sum(w * mu)
      risks[i] <- sqrt(t(w) %*% Sigma_shrink %*% w)
    }


    plot(risks, rets, type = "l",
         xlab = "Risk", 
         ylab = "Expected Return")

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-4-7_files/figure-markdown_strict/unnamed-chunk-2-2.png)
