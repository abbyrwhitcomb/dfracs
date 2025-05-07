    library(dfracs)

    #### Consider the weekly log returns of Yahoo! stock from the week of April 12, 1996 to the week of June 25, 2007 in the file w logret_yahoo.txt.

    w_logret <- read.table(system.file("extdata", "w_logret_yahoo.txt", package = "dfracs"), header = TRUE)
    w_logret$Date <- as.Date(w_logret$Date, format = "%m/%d/%Y")

    ## a: Are there seasonal eﬀects in the series?

    plot(logret ~ Date, data = w_logret,
            main = "Yahoo Weekly Log Returns by Month",
            xlab = "Time", ylab = "Log Return")

![](/Users/abbywhitcomb/Desktop/dfracs/vignettes/rendered/exercise-5-7_files/figure-markdown_strict/unnamed-chunk-2-1.png)


    ## b: Are there serial correlations in the series? Use the Ljung-Box statistic Q(10) to perform the test.

    Box.test(w_logret$logret, lag = 10, type = "Ljung-Box")
    #> 
    #>  Box-Ljung test
    #> 
    #> data:  w_logret$logret
    #> X-squared = 13.899, df = 10, p-value = 0.1777

    ## c: Fit an ARMA(1,1) model to the data from April 12, 1996 to April 30, 2007, and perform diagnostic checks on the fitted model.

    dates <- subset(w_logret, Date >= as.Date("1996-04-12") & Date <= as.Date("2007-04-30"))
    tmsr <- ts(dates$logret, frequency = 52)

    arma_model <- arima(tmsr, order = c(1, 0, 1))
    print(summary(arma_model))
    #>           Length Class  Mode     
    #> coef        3    -none- numeric  
    #> sigma2      1    -none- numeric  
    #> var.coef    9    -none- numeric  
    #> mask        3    -none- logical  
    #> loglik      1    -none- numeric  
    #> aic         1    -none- numeric  
    #> arma        7    -none- numeric  
    #> residuals 577    ts     numeric  
    #> call        3    -none- call     
    #> series      1    -none- character
    #> code        1    -none- numeric  
    #> n.cond      1    -none- numeric  
    #> nobs        1    -none- numeric  
    #> model      10    -none- list

    Box.test(residuals(arma_model), lag = 10, type = "Ljung-Box")
    #> 
    #>  Box-Ljung test
    #> 
    #> data:  residuals(arma_model)
    #> X-squared = 12.105, df = 10, p-value = 0.2781

    ## d: Compute k-weeks-ahead forecasts (k = 1,2,...,8) based on the fitted model, using April 30, 2007 as the forecast origin. Give the standard errors of your forecasts and compare them with the forecast errors, which are the diﬀerences between the predicted and actual log returns.

    real_dates <- subset(w_logret, Date <= as.Date("2007-04-30"))
    forcast_dates <- subset(w_logret, Date > as.Date("2007-04-30") & Date <= as.Date("2007-06-25"))

    real_ts <- ts(real_dates$logret, frequency = 52)
    model <- arima(real_ts, order = c(1, 0, 1))

    forecast_result <- predict(model, n.ahead = 8)
    predicted <- forecast_result$pred
    actual <- forcast_dates$logret
    errors <- actual - predicted

    data.frame(
      Week = 1:8,
      Forecast = predicted,
      Actual = actual,
      Forecast_Error = errors
    )
    #>   Week    Forecast       Actual Forecast_Error
    #> 1    1 0.001298935 -0.004357506  -0.0056564406
    #> 2    2 0.001317463 -0.017424746  -0.0187422092
    #> 3    3 0.001335524  0.003028565   0.0016930406
    #> 4    4 0.001353130 -0.021498757  -0.0228518869
    #> 5    5 0.001370291 -0.001270332  -0.0026406232
    #> 6    6 0.001387020  0.001111744  -0.0002752755
    #> 7    7 0.001403326 -0.003983650  -0.0053869758
    #> 8    8 0.001419221 -0.000480503  -0.0018997237
