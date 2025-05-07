    library(dfracs)

    #### The file w logret 3stocks.txt contains the weekly log returns of three stocks (Citigroup Inc., General Motors, and Pfizer Inc.) from the week of January 4, 1982 to the week of May 21, 2007.

    w_logret <- read.table(system.file("extdata", "w_logret_3stocks.txt", package = "dfracs"), header = TRUE)
    w_logret$Date <- as.Date(w_logret$Date, format = "%m/%d/%Y")

    ## a: Compute the sample mean, variance, skewness, excess kurtosis, and Ljung-Box statistic up to lag 10 for each return series.

    r <- w_logret$Citi
    print(mean(r)) 
    #> [1] 0.00138092
    print(var(r)) 
    #> [1] 0.0003718589
    skewness <- (1/length(r)) * sum((r - mean(r))^3 / (sd(r))^3)
    print(skewness) 
    #> [1] -0.367065
    kurtosis <- (1/length(r)) * sum((r - mean(r))^4 / (sd(r))^4)
    print(kurtosis)
    #> [1] 8.577163
    jb <- length(r) * ((skewness^2)/6 + ((kurtosis - 3)^2/24))
    print(jb)
    #> [1] 1745.677
    Box.test(r, lag = 10, type = "Ljung-Box")
    #> 
    #>  Box-Ljung test
    #> 
    #> data:  r
    #> X-squared = 20.388, df = 10, p-value = 0.02579

    r <- w_logret$GM
    print(mean(r)) 
    #> [1] 0.0005938061
    print(var(r)) 
    #> [1] 0.0003277874
    skewness <- (1/length(r)) * sum((r - mean(r))^3 / (sd(r))^3)
    print(skewness) 
    #> [1] -0.1507214
    kurtosis <- (1/length(r)) * sum((r - mean(r))^4 / (sd(r))^4)
    print(kurtosis)
    #> [1] 5.317057
    jb <- length(r) * ((skewness^2)/6 + ((kurtosis - 3)^2/24))
    print(jb)
    #> [1] 301.1891
    Box.test(r, lag = 10, type = "Ljung-Box")
    #> 
    #>  Box-Ljung test
    #> 
    #> data:  r
    #> X-squared = 13.312, df = 10, p-value = 0.2067

    r <- w_logret$PFE
    print(mean(r)) 
    #> [1] 0.001258388
    print(var(r)) 
    #> [1] 0.0002774457
    skewness <- (1/length(r)) * sum((r - mean(r))^3 / (sd(r))^3)
    print(skewness) 
    #> [1] -0.2608525
    kurtosis <- (1/length(r)) * sum((r - mean(r))^4 / (sd(r))^4)
    print(kurtosis)
    #> [1] 4.532443
    jb <- length(r) * ((skewness^2)/6 + ((kurtosis - 3)^2/24))
    print(jb)
    #> [1] 144.5675
    Box.test(r, lag = 10, type = "Ljung-Box")
    #> 
    #>  Box-Ljung test
    #> 
    #> data:  r
    #> X-squared = 14.773, df = 10, p-value = 0.1406

    ## b: Plot the histograms of these returns and the squared returns.
    hist(w_logret$Citi, 
         main = "Histogram for Weekly Log Returns of Citi", 
         xlab = "Log Returns of Citi", 
         ylab = "Frequency",
         breaks = 20)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-6-3_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    hist(w_logret$Citi^2, 
         main = "Histogram for Squared Weekly Log Returns of Citi", 
         xlab = "Squared Log Returns of Citi", 
         ylab = "Frequency",
         breaks = 20)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-6-3_files/figure-markdown_strict/unnamed-chunk-2-2.png)

    hist(w_logret$GM, 
         main = "Histogram for Weekly Log Returns of GM", 
         xlab = "Log Returns of GM", 
         ylab = "Frequency",
         breaks = 20)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-6-3_files/figure-markdown_strict/unnamed-chunk-2-3.png)

    hist(w_logret$GM^2, 
         main = "Histogram for Squared Weekly Log Returns of GM", 
         xlab = "Squared Log Returns of GM", 
         ylab = "Frequency",
         breaks = 20)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-6-3_files/figure-markdown_strict/unnamed-chunk-2-4.png)

    hist(w_logret$PFE, 
         main = "Histogram for Weekly Log Returns of PFE", 
         xlab = "Log Returns of PFE", 
         ylab = "Frequency",
         breaks = 20)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-6-3_files/figure-markdown_strict/unnamed-chunk-2-5.png)

    hist(w_logret$PFE^2, 
         main = "Histogram for Squared Weekly Log Returns of PFE", 
         xlab = "Squared Log Returns of PFE", 
         ylab = "Frequency",
         breaks = 20)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-6-3_files/figure-markdown_strict/unnamed-chunk-2-6.png)


    ## c: For each stock, perform the Jarque-Bera test of the null hypothesis of normally distributed log returns.

    #Done in part A for easier calculation.

    ## d: For each stock, plot the ACFs of the return series and the squared return series and compare them.
    acf(w_logret$Citi, 
        main = "ACF of Citi", 
        lwd = 4)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-6-3_files/figure-markdown_strict/unnamed-chunk-2-7.png)

    acf(w_logret$Citi^2, 
        main = "Squared Series ACF of Citi", 
        lwd = 4)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-6-3_files/figure-markdown_strict/unnamed-chunk-2-8.png)

    acf(w_logret$GM, 
        main = "ACF of GM", 
        lwd = 4)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-6-3_files/figure-markdown_strict/unnamed-chunk-2-9.png)

    acf(w_logret$GM^2, 
        main = "Squared Series ACF of GM", 
        lwd = 4)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-6-3_files/figure-markdown_strict/unnamed-chunk-2-10.png)

    acf(w_logret$PFE, 
        main = "ACF of PFE", 
        lwd = 4)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-6-3_files/figure-markdown_strict/unnamed-chunk-2-11.png)

    acf(w_logret$PFE^2, 
        main = "Squared Series ACF of PFE", 
        lwd = 4)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-6-3_files/figure-markdown_strict/unnamed-chunk-2-12.png)
\`\`\`
