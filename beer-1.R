

library(forecast)
library(TSA)
library(tseries)


#################### Function Definitions ###############################

# How to find the Time Period using the perodagram
findTimePeriod <- function(data){
  p= periodogram(data)
  dd = data.frame(freq=p$freq,spec=p$spec)
  order = dd[order(-dd$spec),]
  top3=head(order,3)
  print("Frequency",quote= FALSE)
  print(top3)
  
  # Converting frequency to time period
  time = 1/top3$freq
  print("Time Period",quote= FALSE)
  print(time)
}

sumOfSquareAcf<- function(x) {
  sum(acf(x, na.action = na.omit)$acf^2)
}
compare_ssacf<-function(add,mult) {
  additiveSumOfSquare = sumOfSquareAcf(add)
  multiplicativeSumOfSquare = sumOfSquareAcf(mult)
  print(additiveSumOfSquare)
  print(multiplicativeSumOfSquare)
  ifelse(additiveSumOfSquare < multiplicativeSumOfSquare, "Additive", "Multiplicative") 
}

forecastModel <- function(data,method,timePeriod){
  library(ggplot2)
  fit = hw(data,seasonal = method,h=timePeriod)
  print(fit$model)

  fc = forecast(fit,timePeriod)
  plot(fc)
  lines(fitted(fit),col="red")
  fitted(fit)

  fit
}

############# Common Functions for ARIMA Model #############
modelfit <- function(data,p,d,q,P,D,Q,M){
  fit <- arima(log(data),c(p,d,q),seasonal=list(order=c(P,D,Q),period=M))
  # Check All the points are within the limits
  acf(residuals(fit))
  # Check All the points are within the limits
  pacf(residuals(fit))
  
  
  # Test if the residuals are uniformly independently distributed
  # Null Hypothesis - the residuals are uniformly independently distributed
  # Dont reject the null hypothesis as p-value >0.05
  LjungOut <- Box.test(residuals(fit),lag=M,type="Ljung")
  
  cat(sprintf("\"%s\" \"%s\" \"%s\" \"%s\" \n","Model                 ", "Aic   ", "LogLikelihood ", "Ljung P Value"))
  cat(sprintf("\"%s\" \"%.2f\" \"%.2f\" \"%s\" \"%.2f\"  \n", paste("ARIMA(",p,d,q,P,D,Q,M,")"),round(fit$aic,digits=2), logLik(fit), "       ", LjungOut$p.value))
  fit
}

predictData1 <- function(fitModel,duration,data) {
    pred <- predict(fitModel,n.ahead=duration)
    print(pred$pred)
    print(pred$se)
    print(names(pred))
    print(data)
    ts.plot(data,pred$pred,lty=c(1,3),ylabel="Passenger",main="Forecasted Series")
  
  #pred <- predict(fit,n.ahead=duration)
  #ts.plot(data,pred$pred,lty=c(1,3),ylabel="Beer Sales",main="Forecasted Series")
  # futurVal <- forecast(fit,h=duration, level=c(99.5))
  # futurVal
  # plot(futurVal)
}

predictdata <- function(fit,duration,data) {
  pred <- predict(fit,n.ahead=duration)
  ts.plot(data,2.718^pred$pred,log="y",lty=c(1,3),ylabel="Beer Data",main="Forecasted Series")
  print(2.718^pred$pred)
  pred
}
  

# predictdata <- function(fit,duration,data) {
#   #pred <- predict(fit,n.ahead=duration)
#   #ts.plot(data,pred$pred,lty=c(1,3),ylabel="Beer Sales",main="Forecasted Series")
#   futurVal <- forecast(fit,h=duration, level=c(99.5))
#   futurVal
#   plot(futurVal)
# }

predictDataForLog <- function(fit,duration,data) {
  pred <- predict(fit,n.ahead=duration)
  ts.plot(data,2.718^pred$pred,log="y",lty=c(1,3),ylabel="Beer Data",main="Forecasted Series")
}

#################### Holt Winters Method ######################################

# Read the Beer data
setwd("C:/Sai/Great Lakes/Time Series/Assignment")
beer = read.csv("beer.csv",1)

# Check the data for Stationary
attach(beer)
ts_beer=ts(beer)
kpss.test(ts_beer)
# kpss.test shows that the time series is not stationary.


# Find the time period using the periodagram and set the frequency
findTimePeriod(ts_beer)
ts_beer=ts(beer,frequency=4)

# Plot the time series data
plot.ts(OzBeer,col="green",main="Beer Sales")
abline(reg=lm(OzBeer~time(OzBeer)),col="red")
plot(ts_beer,col="blue",main="Beer sales every quarter")

# Check if trend exists using aggregate function
plot(aggregate(ts_beer,FUN = mean),col ="blue",main = "Plot of Monthly Sum of Sales",
     xlab = "Years", ylab = "Sum of Sales")

boxplot(ts_beer~cycle(ts_beer), main = "Boxplot distribution of sales quarterly wise",
        xlab = "Quarterly", ylab = "Beer Sales")

#Test for seasonality
seasonplot(ts_beer, 4, col=rainbow(18), year.labels=TRUE, main="Seasonal plot:
Beer Sales")

# Check how many times to differance for removing seasonality and Trend
nsdiffs(ts_beer)
ndiffs(ts_beer)

# Try fitting with multiplicative Model
fit = forecastModel(ts_beer,"multiplicative",8)
residuals_m = fit$residuals
checkresiduals(residuals(fit))

# Try fitting with additive Model
fit = forecastModel(ts_beer,"additive",8)
residuals_a = fit$residuals
checkresiduals(residuals(fit))

plot.ts(ts_beer,col="green",main="Beer Sales - Multiplicative Method")
fit = hw(ts_beer,seasonal="multiplicative")
lines(fitted(fit),col="red")


# Comapre the Sum of Square of residuals of ACF of both the Models to select the right model 
compare_ssacf(residuals_a,residuals_m)

################### ARIMA for beer data #####################
# ACF and PACF shows that the beer data is not stationary
acf(ts_beer)
pacf(ts_beer)


diff_log_ts = diff(log(ts_beer))
kpss.test(diff_log_ts)
nsdiffs(diff_log_ts)
ndiffs(diff_log_ts)
plot(diff_log_ts,main="Time series of Beer data", col="Blue")
abline(reg=lm(diff_log_ts~time(diff_log_ts)),col="red")
acf(diff_log_ts) # Possible values of q (from ACF Graph) = 2,4,6,8,10
pacf(diff_log_ts) # Possible values of p (from PACF Graph) = 2,3,4

# ARIMA (2,2,1)(2,2,1)4
fit = modelfit(ts_beer,2,1,2,2,1,2,4)
fit
checkresiduals(fit)
plot(ts_beer,main="Time series of Beer data", col="Blue")
predictdata(fit,2*4,ts_beer)
# "Model                 " "Aic   " "LogLikelihood " "Ljung P Value" 
# "ARIMA( 2 1 2 2 1 2 4 )" "-255.02" "135.51" "       " "0.95"  

# ARIMA (2,1,4)(2,1,4)4
fit = modelfit(ts_beer,2,1,4,2,1,4,4)
checkresiduals(fit)
plot(ts_beer,main="Time series of Beer data", col="Blue")
predictdata(fit,2*4,ts_beer)
# "Model                 " "Aic   " "LogLikelihood " "Ljung P Value" 
# "ARIMA( 2 1 4 2 1 4 4 )" "-253.17" "138.58" "       " "0.93" 

# ARIMA (2,1,6)(2,1,6)4
fit = modelfit(ts_beer,2,1,6,2,1,6,4)
checkresiduals(fit)
plot(ts_beer,main="Time series of Beer data", col="Blue")
predictdata(fit,2*4,ts_beer)
# "Model                 " "Aic   " "LogLikelihood " "Ljung P Value" 
# "ARIMA( 2 1 6 2 1 6 4 )" "-249.80" "140.90" "       " "0.92" 

# ARIMA (2,1,8)(2,1,8)4
fit = modelfit(ts_beer,2,1,8,2,1,8,4)
checkresiduals(fit)
plot(ts_beer,main="Time series of Beer data", col="Blue")
predictdata(fit,2*4,ts_beer)
# "Model                 " "Aic   " "LogLikelihood " "Ljung P Value" 
# "ARIMA( 2 1 6 2 1 6 4 )" "-249.80" "140.90" "       " "0.92" 

# ARIMA (3,1,2)(3,1,2)4
fit = modelfit(ts_beer,3,1,2,3,1,2,4)
checkresiduals(fit)
plot(ts_beer,main="Time series of Beer data", col="Blue")
predictdata(fit,2*4,ts_beer)
# "Model                 " "Aic   " "LogLikelihood " "Ljung P Value" 
# "ARIMA( 3 1 2 3 1 2 4 )" "-256.84" "138.42" "       " "0.99"

# ARIMA (3,4,1)(3,4,1)4
fit = modelfit(ts_beer,3,1,4,3,1,4,4)
checkresiduals(fit)
plot(ts_beer,main="Time series of Beer data", col="Blue")
predictdata(fit,2*4,ts_beer)
# "Model                 " "Aic   " "LogLikelihood " "Ljung P Value" 
# "ARIMA( 3 4 1 3 4 1 4 )" "88.86" "-36.43" "       " "0.00"  

# ARIMA (3,4,1)(3,4,1)4
fit = modelfit(ts_beer,4,1,2,4,1,2,4)
checkresiduals(fit)
plot(ts_beer,main="Time series of Beer data", col="Blue")
predictdata(fit,2*4,ts_beer)
# "Model                 " "Aic   " "LogLikelihood " "Ljung P Value" 
# "ARIMA( 3 4 1 3 4 1 4 )" "88.86" "-36.43" "       " "0.00" 

# ARIMA (4,2,1)(4,2,1)4
fit = modelfit(ts_beer,4,2,1,3,4,1,4)
checkresiduals(fit)
plot(ts_beer,main="Time series of Beer data", col="Blue")
predictdata(fit,2*4,ts_beer)
# "Model                 " "Aic   " "LogLikelihood " "Ljung P Value" 
# "ARIMA( 3 4 1 3 4 1 4 )" "88.86" "-36.43" "       " "0.00"  

# ARIMA (3,4,1)(3,4,1)4
fit = modelfit(ts_beer,3,4,1,3,4,1,4)
checkresiduals(fit)
plot(ts_beer,main="Time series of Beer data", col="Blue")
predictdata(fit,2*4,ts_beer)
# "Model                 " "Aic   " "LogLikelihood " "Ljung P Value" 
# "ARIMA( 3 4 1 3 4 1 4 )" "88.86" "-36.43" "       " "0.00"  




fit = modelfit(ts_beer_after_seasonal_detrend,4,2,1,4,2,1,4)
checkresiduals(fit)
plot(ts_beer,main="Time series of Beer data", col="Blue")
predictdata(fit,2*4,ts_beer)



# kpss.test also indicates that the beer data is not stationary
kpss.test((ts_beer))

# Find out the number of times to differance for removing Seasonality
nsdiffs((ts_beer))
# Find out the number of times to differance to removing the trend
ndiffs((ts_beer))


# Plot the beer data and trend line
plot(ts_beer,main="Time series of Beer data", col="Blue")
abline(reg=lm(ts_beer~time(ts_beer)),col="red")


# Differance to remove seasonality and check if that is stationary
ts_beer_after_seasonal = diff(ts_beer,lag=frequency(ts_beer),differences = 1)
# Check if the ts after differancing if seasonality has been removed. 
nsdiffs((ts_beer_after_seasonal))
# Check if the ts after differancing if trend has been removed. 
ndiffs((ts_beer_after_seasonal))
# kpss.test also indicates that the differanced beer data is not stationary
kpss.test((ts_beer_after_seasonal))

# Plot the beer data and trend line
plot(ts_beer_after_seasonal,main="Time series of Beer data", col="Blue")
abline(reg=lm(ts_beer_after_seasonal~time(ts_beer_after_seasonal)),col="red")

# Differance to remove Trend and check if that is stationary
ts_beer_after_seasonal_detrend = diff(ts_beer_after_seasonal,1)
# Check if the ts after differancing if seasonality has been removed. 
nsdiffs((ts_beer_after_seasonal_detrend))
# Check if the ts after differancing if trend has been removed. 
ndiffs((ts_beer_after_seasonal_detrend))
# kpss.test indicates that the differanced beer data is now stationary
kpss.test(ts_beer_after_seasonal_detrend)
# Plot the beer data and trend line
plot(ts_beer_after_seasonal_detrend,main="Time series of Beer data", col="Blue")
abline(reg=lm(ts_beer_after_seasonal_detrend~time(ts_beer_after_seasonal_detrend)),col="red")


acf((ts_beer_after_seasonal_detrend))
pacf((ts_beer_after_seasonal_detrend))
# p value is 2 q value is 0 and d value is 2 and m is 

fit = modelfit(ts_beer_after_seasonal_detrend,1,1,1,1,1,1,4)
checkresiduals(fit)
plot(ts_beer,main="Time series of Beer data", col="Blue")
predictdata(fit,2*4,ts_beer)

fit = modelfit(ts_beer_after_seasonal_detrend,3,1,0,3,1,0,4)
checkresiduals(fit)
plot(ts_beer,main="Time series of Beer data", col="Blue")
predictData1(fit,2*4,ts_beer)

fit = modelfit(ts_beer_after_seasonal_detrend,3,2,0,3,2,0,4)
checkresiduals(fit)
plot(ts_beer,main="Time series of Beer data", col="Blue")
predictdata(fit,2*4,ts_beer)

fit1 <- auto.arima((ts_beer), seasonal=TRUE, trace = TRUE)
fit1
#Best model: ARIMA(2,1,1)(0,1,1)[4]
print(summary(fit1))
checkresiduals(fit1)
