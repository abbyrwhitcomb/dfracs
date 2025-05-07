    library(dfracs)

    #### The file m_sp500ret_3mtcm.txt contains three columns. The second column gives the monthly returns of the S&P 500 index from January 1994 to December 2006. The third column gives the monthly rates of the 3-month U. S. Treasury bill in the secondary market, which is obtained from the Federal Reserve Bank of St. Louis and used as the risk-free asset here. Consider the ten monthly returns in the file m ret 10stocks.txt.

    m_sp500ret <- read.table(system.file("extdata", "m_sp500ret_3mtcm.txt", package = "dfracs"), header = TRUE, skip = 1)
    m_sp500ret$Date <- as.Date(paste0("01-", m_sp500ret$Date), format = "%d-%b-%y")
    m_logret <- read.table(system.file("extdata", "m_logret_10stocks.txt", package = "dfracs"), header = TRUE)

    ## a: Fit CAPM to the ten stocks. Give point estimates and 95% confidence intervals of alpha, beta, the Sharpe index, and the Treynor index. (Hint: Use the delta method for the Sharpe and Treynor indices.)

    stocks <- m_logret[, -1]
    market <- m_sp500ret[, 2]  
    rf <- m_sp500ret[, 3]      

    capm_est <- capm(stocks, market, rf)
    capm_est
    #>    stock                alpha              beta           alpha_lwr           alpha_upr          beta_lwr         beta_upr            sharpe          sharpe_se
    #> 1   AAPL   0.0266917647763696  1.00592348029235 0.00106439378447631  0.0523191357682628 0.999810048155551 1.01203691242915 -2.35499164318819 0.0800640769025436
    #> 2   ADBE  0.00885435579627779  1.00110324093169 -0.0163746125035554   0.034083324096111 0.995084848111325 1.00712163375206 -2.36588230421352 0.0800640769025436
    #> 3    ADP -0.00527737245999902 0.998420694838682 -0.0148819035683891 0.00432715864839103 0.996129525402866  1.0007118642745 -2.37599787373448 0.0800640769025436
    #> 4    AMD   0.0108678038412762   1.0030156536573 -0.0207615121047007  0.0424971197872532 0.995470452215051 1.01056085509956 -2.36370895143603 0.0800640769025436
    #> 5   DELL  0.00351492190555578 0.998633075976005 -0.0179878443366384  0.0250176881477499 0.993503571969653 1.00376257998236 -2.36960500372715 0.0800640769025436
    #> 6    GTW -0.00621254802732263 0.999836944329266 -0.0362920768332096  0.0238669807785643 0.992661445987281 1.00701244267125 -2.37440273384508 0.0800640769025436
    #> 7     HP  0.00421661351464743    1.000595186312 -0.0143968740613774  0.0228301010906723 0.996154922300475 1.00503545032352 -2.36949141369488 0.0800640769025436
    #> 8    IBM  -0.0028650871318016 0.998590788892934 -0.0153254517643766 0.00959527750077342 0.995618357830647 1.00156321995522 -2.37434591516117 0.0800640769025436
    #> 9   MSFT 0.000245286627692486 0.998977488395815 -0.0150502385542889  0.0155408118096738 0.995328727263573 1.00262624952806 -2.37222423323337 0.0800640769025436
    #> 10  ORCL -0.00219889790067599 0.998437948385383 -0.0252004725339589  0.0208026767326069 0.992950902304666  1.0039249944661 -2.37293793007896 0.0800640769025436
    #>              treynor          treynor_se
    #> 1  -3.83831821352691  0.0118082820515886
    #> 2  -3.85600820312249  0.0117345166030469
    #> 3  -3.87013852142138 0.00449568355118265
    #> 4   -3.8540176723508  0.0146758215111444
    #> 5  -3.86133306808285  0.0100399669363599
    #> 6  -3.87106636237987  0.0140630362494409
    #> 7  -3.86063869585933 0.00867231681037446
    #> 8   -3.8677219315383 0.00582780577786015
    #> 9  -3.86460726350332 0.00714530363233958
    #> 10 -3.86705513926256  0.0107578004492642

    ## b: Use the bootstrap procedure in Section 3.5 to estimate the standard errors of the point estimates of α, β, and the Sharpe and Treynor indices.

    boot_est <- data.frame(
      stock = colnames(stocks),
      alpha_se = NA,
      beta_se = NA,
      sharpe_se = NA,
      treynor_se = NA
    )

    for (i in 1:ncol(stocks)) {
      boot_est[i, 2:5] <- bootstrap_capm(stocks[, i], market, rf)
    }
    print(boot_est)
    #>    stock    alpha_se     beta_se sharpe_se treynor_se
    #> 1   AAPL 0.010088321 0.003017683 0.1535026  0.1265607
    #> 2   ADBE 0.009018130 0.002757245 0.1549027  0.1288281
    #> 3    ADP 0.004946129 0.001211854 0.1554572  0.1288624
    #> 4    AMD 0.015748061 0.003894693 0.1550343  0.1286208
    #> 5   DELL 0.006809957 0.002170841 0.1546561  0.1276098
    #> 6    GTW 0.015606143 0.003970984 0.1564596  0.1293057
    #> 7     HP 0.007786227 0.002045912 0.1548121  0.1288778
    #> 8    IBM 0.005727872 0.001474049 0.1552048  0.1283024
    #> 9   MSFT 0.005851396 0.001864746 0.1549360  0.1277464
    #> 10  ORCL 0.009504610 0.002477891 0.1553735  0.1286962

    ## c: Test for each stock the null hypothesis α = 0.

    alpha_test <- alpha_capm(stocks, market, rf)
    print(alpha_test)
    #>    stock         alpha    alpha_se      t_stat    p_value
    #> 1   AAPL  0.0266917648 0.012972678  2.05753694 0.04132086
    #> 2   ADBE  0.0088543558 0.012771005  0.69331706 0.48915494
    #> 3    ADP -0.0052773725 0.004861852 -1.08546541 0.27941182
    #> 4    AMD  0.0108678038 0.016010887  0.67877588 0.49829867
    #> 5   DELL  0.0035149219 0.010884787  0.32292060 0.74719340
    #> 6    GTW -0.0062125480 0.015226378 -0.40801219 0.68383142
    #> 7     HP  0.0042166135 0.009422222  0.44751794 0.65512998
    #> 8    IBM -0.0028650871 0.006307487 -0.45423594 0.65029858
    #> 9   MSFT  0.0002452866 0.007742656  0.03167991 0.97476834
    #> 10  ORCL -0.0021988979 0.011643490 -0.18885214 0.85045736

    ## d: Use the regression model (3.24) to test for the ten stocks the null hypothesis α = 0.
    # y_t = alpha + x_t*beta + epsilon_t, 1 ≤ t ≤ n, (3.24)

    alpha_test <- alpha_capm(stocks, market, rf)
    print(alpha_test)
    #>    stock         alpha    alpha_se      t_stat    p_value
    #> 1   AAPL  0.0266917648 0.012972678  2.05753694 0.04132086
    #> 2   ADBE  0.0088543558 0.012771005  0.69331706 0.48915494
    #> 3    ADP -0.0052773725 0.004861852 -1.08546541 0.27941182
    #> 4    AMD  0.0108678038 0.016010887  0.67877588 0.49829867
    #> 5   DELL  0.0035149219 0.010884787  0.32292060 0.74719340
    #> 6    GTW -0.0062125480 0.015226378 -0.40801219 0.68383142
    #> 7     HP  0.0042166135 0.009422222  0.44751794 0.65512998
    #> 8    IBM -0.0028650871 0.006307487 -0.45423594 0.65029858
    #> 9   MSFT  0.0002452866 0.007742656  0.03167991 0.97476834
    #> 10  ORCL -0.0021988979 0.011643490 -0.18885214 0.85045736

    ## e: Perform a factor analysis on the excess returns of the ten stocks. Show the factor loadings and rotated factor loadings. Explain your choice of the number of factors.

    excess_returns <- sweep(stocks, 1, rf)
    excess_market <- market - rf

    cor_mat <- cor(excess_returns)
    eigenvalues <- eigen(cor_mat)$values
    plot(eigenvalues, type = "b", pch = 16, 
         main = "Scree Plot")

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-3-6_files/figure-markdown_strict/unnamed-chunk-2-1.png)


    fa <- factanal(excess_returns, factors = 1, rotation = "varimax", scores = "none")
    fa
    #> 
    #> Call:
    #> factanal(x = excess_returns, factors = 1, scores = "none", rotation = "varimax")
    #> 
    #> Uniquenesses:
    #>  AAPL  ADBE   ADP   AMD  DELL   GTW    HP   IBM  MSFT  ORCL 
    #> 0.005 0.005 0.005 0.005 0.005 0.005 0.005 0.005 0.005 0.005 
    #> 
    #> Loadings:
    #>      Factor1
    #> AAPL 0.999  
    #> ADBE 0.999  
    #> ADP  1.000  
    #> AMD  0.999  
    #> DELL 0.999  
    #> GTW  0.999  
    #> HP   0.999  
    #> IBM  1.000  
    #> MSFT 0.999  
    #> ORCL 0.999  
    #> 
    #>                Factor1
    #> SS loadings      9.986
    #> Proportion Var   0.999
    #> 
    #> Test of the hypothesis that 1 factor is sufficient.
    #> The chi square statistic is 1430.06 on 35 degrees of freedom.
    #> The p-value is 4.37e-278

    ## f: Consider the model ret = β11{t<t0}reM + β21{t≥t0}reM + epsilont,in which ret = rt− rf and reM = rM− rf are the excess returns of thes tock and the S&P 500 index. The model suggests that the β in the CAPM might not be a constant (i.e., beta1 = beta2). Taking February 2001 as the month t0, test for each stock the null hypothesis that β1 = β2.

    t0 <- which(m_sp500ret$Date == as.Date("2001-02-01"))
    n <- nrow(stocks)
    before_after <- as.numeric(1:n >= t0)  

    beta <- data.frame(
      stock = colnames(stocks),
      beta1 = NA, beta2 = NA, diff = NA, se_diff = NA, t_stat = NA, p_value = NA
    )

    for (i in 1:ncol(stocks)) {
      excess_stock <- stocks[, i] - rf
      before <- excess_market * (1 - before_after)
      after <- excess_market * before_after
      model <- lm(excess_stock ~ before + after)
      coefs <- coef(model)
      vcov_mat <- vcov(model)
      beta1 <- coefs["before"]
      beta2 <- coefs["after"]
      diff <- beta1 - beta2
      se_diff <- sqrt(vcov_mat["before", "before"] + vcov_mat["after", "after"] - 2 * vcov_mat["before", "after"])
      t_stat <- diff / se_diff
      p_val <- 2 * pt(-abs(t_stat), df = model$df.residual)
      
      beta[i, 2:7] <- c(beta1, beta2, diff, se_diff, t_stat, p_val)
    }

    print(beta)
    #>    stock     beta1     beta2          diff     se_diff      t_stat    p_value
    #> 1   AAPL 1.0053997 1.0033028  2.096948e-03 0.003331323  0.62946403 0.52998412
    #> 2   ADBE 1.0015657 1.0034171 -1.851429e-03 0.003280365 -0.56439738 0.57331019
    #> 3    ADP 0.9984377 0.9985055 -6.788652e-05 0.001250104 -0.05430468 0.95676326
    #> 4    AMD 1.0038803 1.0073422 -3.461901e-03 0.004107316 -0.84286196 0.40062135
    #> 5   DELL 1.0000916 1.0059309 -5.839322e-03 0.002758678 -2.11671028 0.03590230
    #> 6    GTW 1.0015877 1.0085970 -7.009312e-03 0.003873896 -1.80937042 0.07235646
    #> 7     HP 1.0008576 1.0019082 -1.050574e-03 0.002421224 -0.43390212 0.66497074
    #> 8    IBM 0.9987818 0.9995463 -7.645734e-04 0.001620651 -0.47176945 0.63776345
    #> 9   MSFT 0.9992167 1.0001743 -9.576123e-04 0.001989345 -0.48137070 0.63094081
    #> 10  ORCL 0.9994638 1.0035712 -4.107312e-03 0.002975391 -1.38042779 0.16946842

    ## g: Estimate t0 in (f) by the least squares criterion that minimizes the residual sum of squares over (β1,β2,t0).

    t0s_est(stocks, market, rf, m_logret$Date)
    #>    stock t0_index    t0_date
    #> 1   AAPL       49 0001-02-19
    #> 2   ADBE        2 0002-01-19
    #> 3    ADP        2 0002-01-19
    #> 4    AMD        4 0004-04-19
    #> 5   DELL       62 0002-01-19
    #> 6    GTW       81 0009-01-20
    #> 7     HP      146 0002-01-20
    #> 8    IBM       67 0007-01-19
    #> 9   MSFT       73 0001-03-20
    #> 10  ORCL       81 0009-01-20
