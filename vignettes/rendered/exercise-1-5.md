    library(dfracs)

    ### The log return of a stock at week t is defined as rt = log(P_t/P_(t-1)), where Pt is the stock price at week t. Consider the weekly log returns of Citigroup Inc., Pfizer Inc., and General Motors from the week of January 4, 1982 to the week of May 21, 2007 in the file w_logret_3stocks.txt.

    w_logret <- read.table(system.file("extdata", "w_logret_3stocks.txt", package = "dfracs"), header = TRUE)
    w_logret$Date <- as.Date(w_logret$Date, format = "%m/%d/%Y")

    ##a: Plot the weekly log returns of each stock over time. A widely held assumption in mathematical models of stock prices is that the corresponding log returns are independent and identically distributed (i.i.d.); see Section 3.1. Do your plots show departures from this assumption? 

    company_names <- c("Citi", "PFE", "GM")
    multiplotlogreturns(w_logret, company_names, "Date", "Weekly")

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-1-5_files/figure-markdown_strict/unnamed-chunk-2-1.png)![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-1-5_files/figure-markdown_strict/unnamed-chunk-2-2.png)![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-1-5_files/figure-markdown_strict/unnamed-chunk-2-3.png)


    ##b: For each stock, show the Q-Q plot of the weekly log returns and thereby check the assumption that they are normally distributed.

    multiqqplot(w_logret, company_names)

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-1-5_files/figure-markdown_strict/unnamed-chunk-2-4.png)![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-1-5_files/figure-markdown_strict/unnamed-chunk-2-5.png)![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-1-5_files/figure-markdown_strict/unnamed-chunk-2-6.png)


    ##c: For every pair of stocks, estimate the correlation coeﬃcient of the weekly log returns from these data, and give a bootstrap estimate of its standard error.

    cor_matrix <- cor(w_logret[, -1])
    print(cor_matrix)
    #>           Citi       PFE        GM
    #> Citi 1.0000000 0.3154711 0.3241647
    #> PFE  0.3154711 1.0000000 0.2220058
    #> GM   0.3241647 0.2220058 1.0000000

    k <- 500 
    set.seed(1052)
    bootstrap_matrix(w_logret[, company_names], k = 500)
    #>              Citi          PFE           GM
    #> Citi 6.247244e-17 3.207612e-02 3.106277e-02
    #> PFE  3.207612e-02 1.110223e-16 2.879271e-02
    #> GM   3.106277e-02 2.879271e-02 9.403759e-17
