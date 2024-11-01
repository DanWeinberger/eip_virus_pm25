set.seed(123)

#Historical case data from previous 5 years
historical.case <- c(2,5,3,1,7)
historical.mean <- mean(historical.case)

#simulate data from forecats, pretending we have 2 distributions representing 2 members of ensemble
forecast.output1 <-  rnorm(200, mean= log(historical.mean*5.0), sd=1)
forecast.output2 <-  rnorm(800, mean= log(historical.mean*5.0), sd=0.5)

forecast.output.ensemble.mean <- exp(c(forecast.output1,forecast.output2))
forecast.output.ensemble.count <- rpois(n=length(forecast.output.ensemble.mean),
                                        lambda=forecast.output.ensemble.mean)

#calculate the empirical CDF, assuming the ensemble estimimates are integers
integers <-seq(from=0, to=max(forecast.output.ensemble.count), by=1)

forecast.cdf <- NA
forecast.pdf <- NA
for(i in 1:length(integers)){
  forecast.cdf[i] <- mean(forecast.output.ensemble.count<=integers[i])
  forecast.pdf[i] <- mean(forecast.output.ensemble.count==integers[i])
  }
rev.forecast.cdf <- 1- forecast.cdf


#calculate PDF assuming historical data drawn from a Poisson distribution
pdf.pois.historical <- dpois(x=seq(from=0,to=max(forecast.output.ensemble.count), by=1),  lambda= historical.mean)
cdf.pois.historical <- cumsum(pdf.pois.historical)
rev.cdf.pois.historical <- 1-cdf.pois.historical
 
 plot(pdf.pois.historical)
 plot(cdf.pois.historical)
 
 plot(rev.cdf.pois.historical, type='l', col='red')
 points(rev.forecast.cdf, type='l',col='blue')
 
 diff.pdf <- NA
 for(i in 1:length(integers)){
   diff.pdf[i] <- forecast.pdf[i] - pdf.pois.historical[i]
   diff.pdf[i] <- if_else(diff.pdf[i]<0,0,diff.pdf[i])
 }
 
 plot(pdf.pois.historical, type='l', col='red')
 points(forecast.pdf, type='l',col='blue')
 points(diff.pdf, type='l',col='orange')
 
 prob_epidemic <- sum(diff.pdf)
 prob_epidemic
