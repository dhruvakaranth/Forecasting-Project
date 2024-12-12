library(quantmod)
library(forecast)
library(caret)

#Loading Data
getSymbols("RS", src = "yahoo" , from = Sys.Date()-365*9 , to = Sys.Date())

# Converting Data into Monthly Intervals
MADA <- to.monthly(RS)

#Creating Time Series
ts <- ts(Cl(MADA),frequency =12)

# Plotting Data
plot(ts, xlab = "Years", lwd = 5, col = "grey")


# Splitting data for Training and Testing purposes
ts.train <- window(ts, start = 1, end = 9.4)
ts.test <- window(ts, start = 9.5, end = 10 )

# Computing Moving Average
lines(ma(ts.train, order = 3),col = "blue")

# Computing exponential Smoothing Average
ets<- ets(ts.train,model = "MMM")

# Forecasting training data
fcast <- forecast(ets)

# Comparing training forecast and actualdata
plot(fcast, lwd =4 )

lines(ts.test, col = "green", lwd =2)

accuracy(fcast,ts.test)