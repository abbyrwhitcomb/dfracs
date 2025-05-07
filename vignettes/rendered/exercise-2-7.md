    library(dfracs)

    #### The file w_logret_3automanu.txt contains the weekly log returns of three auto manufacturers, General Motors Corp., Ford Motor Corp., and Toyota Motor Corp., from the week of January 3, 1994 to the week of June 25, 2007.

    w_logret <- read.table(system.file("extdata", "w_logret_3automanu.txt", package = "dfracs"), header = TRUE)

    ## a: Write down the sample covariance matrix V^ of these returns.

    cov_mat <- cov(w_logret[, -1])
    print(cov_mat)
    #>              Toyota         Ford           GM
    #> Toyota 2.590840e-04 7.589026e-05 6.252166e-05
    #> Ford   7.589026e-05 3.781422e-04 2.370206e-04
    #> GM     6.252166e-05 2.370206e-04 3.959958e-04

    ## b: For each pair of these three auto manufacturers, use the result in Exercise 2.6(b) to construct a 95% confidence interval for the correlation coeﬃcient of their returns.

    cor_mat <- cor(w_logret[, -1])
    print(cor_mat)
    #>           Toyota      Ford        GM
    #> Toyota 1.0000000 0.2424589 0.1951933
    #> Ford   0.2424589 1.0000000 0.6125100
    #> GM     0.1951933 0.6125100 1.0000000

    p_GM_Ford <- cor_mat["GM", "Ford"]
    p_GM_Toyota <- cor_mat["GM", "Toyota"]
    p_Ford_Toyota <- cor_mat["Ford", "Toyota"]

    n = nrow(w_logret)
    cv <- qnorm(0.975)

    se_GM_Ford <- (1 - p_GM_Ford^2) / sqrt(n) #result of 2.6: sqrt(n)(p^ - p) → N(0, (1 - p^2)^2).
    se_GM_Toyota <- (1 - p_GM_Toyota^2) / sqrt(n)
    se_Ford_Toyota <- (1 - p_Ford_Toyota^2) / sqrt(n)

    print(c(p_GM_Ford - cv * se_GM_Ford, p_GM_Ford + cv * se_GM_Ford))
    #> [1] 0.5665175 0.6585026
    print(c(p_GM_Toyota - cv * se_GM_Toyota, p_GM_Toyota + cv * se_GM_Toyota))
    #> [1] 0.1243898 0.2659968
    print(c(p_Ford_Toyota - cv * se_Ford_Toyota, p_Ford_Toyota + cv * se_Ford_Toyota))
    #> [1] 0.1731781 0.3117398

    ## c: What assumptions have you made for (b)? Perform some graphical checks, such as data plots and Q-Q plots, on these assumptions.
    # As stated in 2.6, we let (Ui,Vi) be i.i.d. bivariate normal random vectors to prove sqrt(n)(p^ - p) → N(0, (1 - p^2)^2). We need to assume the pairs of log returns follow a bivariate normal distribution. 

    pairs(w_logret[, -1], main = "Returns")

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-2-7_files/figure-markdown_strict/unnamed-chunk-2-1.png)


    company_names <- c("GM", "Ford", "Toyota")
    multiqqplot(w_logret, company_names)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-2-7_files/figure-markdown_strict/unnamed-chunk-2-2.png)![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-2-7_files/figure-markdown_strict/unnamed-chunk-2-3.png)![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-2-7_files/figure-markdown_strict/unnamed-chunk-2-4.png)
