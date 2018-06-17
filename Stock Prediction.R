# Code written by Harsh Bhotika

#Get Stock Prices

Stock_Function <- function(ticker)
{
library(quantmod)
#ticker<-"GSPC"
stock_name <- getSymbols(ticker,src = 'yahoo', from='2013-01-01', to='2017-12-31')
View(stock_name)
class(stock_name)
stock <- get(ticker)
View(stock)
class(stock)
names(stock)
names(stock) <- c("stock.Open","stock.High","stock.Low","stock.Close","stock.Volume","stock.Adjusted")
View(stock)
class(stock)

#Format the Date column to get a Date column
stock <- as.data.frame(stock)
View(stock)
class(stock)

stock$Date <- row.names(stock)
View(stock)
cln <- ncol(stock) # 7
stock <- stock[, c(cln, 1:(cln-1))]
row.names(stock) <- NULL
head(stock)
View(stock)
class(stock)
class(stock$Date)
class(stock$stock.Adjusted)
stock$Date <- as.Date(stock$Date)


#Aggregate data by Month
library(lubridate)
bymonth <- aggregate(cbind(stock.Adjusted)~month(Date),data = stock, FUN = mean)
class(bymonth$`month(Date)`)
colnames(bymonth)[colnames(bymonth)=="month(Date)"] <- "Month"
View(bymonth)

#Name months
bymonth$`Month`[1:12] <- c("January","February","March","April","May","June","July","August",
                                 "September","October","November","December")
View(bymonth)

#Boxplot by Month
boxplot(stock$stock.Adjusted ~ month(stock$Date), data = stock, col="Yellow", staplewex = 1, names=bymonth$Month, ylab="Stock Price",
        xlab="Month", main="Monthly Stock Prices")
bp <- boxplot(stock$stock.Adjusted ~ month(stock$Date), data = stock, col="Yellow", staplewex = 1, names=bymonth$Month,
              ylab=" AdjustedStock Price", xlab="Month", main= stock_name, col.main="blue")
bp$stats
bp$stats<- round(bp$stats, digits = 2)
text(x = col(bp$stats) - .5, y = bp$stats, labels = bp$stats)

#Boxplot by Year
boxplot(stock$stock.Adjusted ~ year(stock$Date), data = stock, col="Green", staplewex = 1)
bp <- boxplot(stock$stock.Adjusted ~ year(stock$Date), data = stock, col="Green", staplewex = 1, ylab="Adjusted Stock Price",
              xlab="Year", main= stock_name, col.main="blue")
bp$stats
bp$stats<- round(bp$stats, digits = 2)
text(x = col(bp$stats) - .5, y = bp$stats, labels = bp$stats)

# Time Series and ARIMA

library(MASS)
library(tseries)
library(forecast)
library(xts)

plot(stock$Date,stock$stock.Adjusted, type = "h", ylab="Adjusted Stock Price",
     xlab="Year", main= stock_name, col.main="blue")

plot(stock$Date,stock$stock.Adjusted, type = "l", ylab="Adjusted Stock Price",
     xlab="Year", main= "Regression Line Plot", col.main="blue")
abline(reg = lm(stock$stock.Adjusted ~ stock$Date), col="blue")

plot(stock$Date,log(stock$stock.Adjusted), type = "l", ylab="Log of Adjusted Stock Price",
     xlab="Year", main= "", col.main="blue")
abline(reg = lm(log(stock$stock.Adjusted) ~ stock$Date), col="blue")

diff_stock <- c(NA,diff(log(stock$stock.Adjusted)))
View(diff_stock)
plot(stock$Date,diff_stock,type = "l", ylab="Diff Log Stock Price",
     xlab="Year", main= "", col.main="blue")
plot(diff(log(stock$stock.Adjusted)),type = "l")

agg <- aggregate(stock.Adjusted~year(Date), data = stock, FUN = mean)
View(agg)
plot(agg, type="l", ylab="Stock Price",
     xlab="Year", main= "Stock Trend Line", col.main="blue")

#Test to convert Data Frame to TS
#stock_TS <- xts(stock[,-1], order.by=as.Date(stock[,1], "%Y/%m/%d"))
#View(stock_TS)

#Time Series and ARIMA continue
start(stock$stock.Adjusted)
end(stock$stock.Adjusted)
frequency(stock$stock.Adjusted)
summary(stock$stock.Adjusted)

acf(stock$stock.Adjusted)
acf(log(stock$stock.Adjusted))
acf(diff(log(stock$stock.Adjusted)))

pacf(stock$stock.Adjusted)
pacf(log(stock$stock.Adjusted))
pacf(diff(log(stock$stock.Adjusted)))

# Dickey Fuller test to screen for stationarity
adf.test(log(stock$stock.Adjusted))
adf.test(diff(log(stock$stock.Adjusted)))

# auto arima to automate p,d,q
pricearima <- ts(log(stock$stock.Adjusted), start = c(1,02),frequency = 1)
View(pricearima)
fitlnstock <- auto.arima(pricearima)
fitlnstock
View(fitlnstock)
plot(pricearima, type="l", main="Comparison of Fitted Prices")
lines(fitted(fitlnstock), col="red")

forecasted <- forecast(fitlnstock, h=100)
forecasted
View(forecasted)
plot(forecasted, xlab="Days")

forecastedvalues <- as.numeric(forecasted$mean)
finalvalues <- exp(forecastedvalues)
finalvalues

df <- getSymbols(ticker,src = 'yahoo', from='2018-01-01', to='2018-05-25')
stock_pred <- get(ticker)
View(stock_pred)
names(stock_pred) <- c("stock.Open","stock.High","stock.Low","stock.Close","stock.Volume","stock.Adjusted")

df<-data.frame(stock_pred$stock.Adjusted,finalvalues)
col_headings<-c("Actual Price","Forecasted Price")
names(df)<-col_headings
attach(df)
View(df)

percentage_error=((df$`Actual Price`-df$`Forecasted Price`)/(df$`Actual Price`))
percentage_error
Prediction_Error <- round((mean(percentage_error)*100),digits = 2)
Prediction_Error

}

Stock_Function('BB')
