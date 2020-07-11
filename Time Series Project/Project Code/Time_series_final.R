#Retail-Giant Sales Forecasting Case Study 
#Forecast the sales & demand for next 6 months for "Global Mart"

#-----------------------------------LOADING LIBRARIES-------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forecast)

#---------------------------------READING DATA FROM CSV----------------------------------------

global_market <- read.csv("Global Superstore.csv")

str(global_market)
View(global_market)

#--------------------------------DATA CLEANING-------------------------------------------------

summary(global_market)
#Postal code has [41296] NA values which needs to be cleaned.
#As postal code value is not available for regions aprt from USA.

#Checking duplicate values
sum(duplicated(global_market))

#As observed we dont have any duplicate values in dataset

#Checking for NA values along all columns
sum(is.na(global_market))

#Creating a list to store column name and corrosponding NA value.
#checking for each column in global_market.
#if value returned by is.na function for a perticular column >0, add the column to list.

list.NAcolumn<-""
for (i in c(1:ncol(global_market)))
{
  len<-length(grep("TRUE",is.na(global_market[,i])))
  if(len > 0){
    list.NAcolumn<-paste(colnames(global_market[i]),":",len,list.NAcolumn)
  }
}

View(list.NAcolumn)

  #As observed there is only one column :"Postal Code" which contains [41296] NA values.

#Deleting Postal.Code as it contain NA values and its unnecessary for forcasting
global_market <- subset(global_market,TRUE,-c(Postal.Code))

#Converting dates column from Factor to date
# 1. Change Order.Date from factor to Date
global_market$Order.Date <- as.Date(global_market$Order.Date,"%d-%m-%Y")
summary(global_market$Order.Date)
#All orders are placed between 2011-01-01 to 2014-12-31

# 2. Change Ship.Date from factor to Date
global_market$Ship.Date<- as.Date(global_market$Ship.Date,"%d-%m-%Y")

#---------------------------------DATA PREPRATION-------------------------------------------

#Segment the whole Dataset into 21 subset based on :
#1.Market area -[eg : APAC , EU etc upto all 7 markets]
#2.Customer segment level - [Corporate,Consumer,Home Office]

global_market$Marketcustomer_segment <- paste(global_market$Market,global_market$Segment,sep = "_")

#Column Marketcustomer_segment contains data of Market & segment combined for each customer.

#Aggregate the 3 attributes Sales, Quantity & Profit, over the Order Date 
#to arrive at monthly values for these attributes

aggregated_data_seg <- global_market %>%  group_by(Marketcustomer_segment) %>% summarise(.,sum(Sales),sum(Profit),sum(Quantity),sd(Profit)*100/mean(Profit))            

View(aggregated_data_seg)

#Aggregating data based on each month for each segment
global_market$YearMonth <- format(as.Date(global_market$Order.Date),"%Y-%m")

aggregated_data_monthly <- global_market %>% group_by(Marketcustomer_segment, YearMonth) %>% summarise(.,sum(Sales),sum(Profit),sum(Quantity),sd(Profit)*100/mean(Profit))

#-----------------------------------PLOTTING THE GRAPHS TO VISUALISE DATA---------------------
global_param <- global_market[,c("Profit","Sales","Market","Segment","Quantity")] %>% group_by(Market,Segment) %>% dplyr::summarise(., sum(Sales),sum(Profit),sd(Profit)*100/mean(Profit))
colnames(global_param) = c("Market","Segment","Sales","Profit","CV")

ggplot(global_param, aes(Segment, Sales, fill=Market)) + geom_bar(position = "dodge",stat = "identity")
ggplot(global_param, aes(Segment, Profit, fill=Market)) + geom_bar(position = "dodge",stat = "identity")
ggplot(global_param, aes(Segment, CV, fill=Market)) + geom_bar(position = "dodge",stat = "identity")

#Based on the graphs below are the findings :
#Highest : 
#1. SALES  : Consumer sector - APAC
#2. Profit : Consumer sector - APAC
#3. CV     : Consumer sector - Canada [Lowest value]

#Second Highest :
#1. SALES  : Consumer sector - EU 
#2. PROFIT : Consumer sector - EU
#3. CV     : Corporate sector - canada


#Based on the graphs plotted we arrive at the conclusion that 2 most profitable sector with
#respect to highest SALES & PROFIT and low CV value are :
# 1. Consumer sector - APAC region 
# 2. Consumer sector - EU region 

#------------------------------------------MODEL BUILDING------------------------------------------------------------------

#Subsetting Aggregated data for profit,sales,quantity based on months for top 2 market segments
top_2Segments <- subset(aggregated_data_monthly, Marketcustomer_segment == "EU_Consumer" | Marketcustomer_segment == "APAC_Consumer")
str(top_2Segments)

#Changing column names
names(top_2Segments) <- c("Market_Segment", "Order_Month", "Sales", "Profit", "Quantity", "CV")

#Getting Top 2 Market Segments
invisible(lapply(split(top_2Segments, top_2Segments$Market_Segment), function(x) {assign(paste0("Top_", x$Market_Segment[1]), x, pos = .GlobalEnv)}))

#Setting data into 2 different dataset acc to APAC & EU region for model building
APAC_Consumer_Aggregate <- Top_APAC_Consumer[,c(2:5)]
EU_Consumer_Aggregate   <- Top_EU_Consumer[,c(2:5)]

#-----------------------------------------MODEL FOR APAC CONSUMER----------------------------

#---------------------------------------SALES FORECAST FOR APAC------------------------------

#As Order.Date has value from 2011-01-01 to 2014-12-31 we choose start and end accordingly
#Frequency i.e. no of observation per unit time is set as 12
title <- c("APAC Consumer Sales: JAN 2011 to DEC 2014")
APAC_Consumer_Sales.TS <- ts(APAC_Consumer_Aggregate$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_Consumer_Sales.TS)

#--------------------------------------SMOOTHNING TIME SERIES----------------------------------
#Smoothing time series using Moving Average method
#Choosing the window size as 4 as it is best fit
w <- 4
APAC_smoothedseries <-stats::filter(APAC_Consumer_Sales.TS,filter=rep(1/w,w,method='convolution',sides=2)) 
View(APAC_smoothedseries)

#NA values are introduced to left & right of series by smoothing
#Smoothing left end of the time series
diff <- APAC_smoothedseries[w+2] - APAC_smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  APAC_smoothedseries[i] <- APAC_smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
Length_APAC <- length(APAC_smoothedseries)
diff <- APAC_smoothedseries[Length_APAC-w] - APAC_smoothedseries[Length_APAC-w-1]
for (i in seq(Length_APAC-w+1, Length_APAC)) {
  APAC_smoothedseries[i] <- APAC_smoothedseries[i-1] + diff
}

#Converting smoothed series to data frame based on order month

APAC_Consumer_Salesdf <- data.frame(cbind(APAC_Consumer_Aggregate$Order_Month,APAC_smoothedseries))
colnames(APAC_Consumer_Salesdf) <- c("Month","Sales")
str(APAC_Consumer_Salesdf)

#Plotting smoothed series using Moving Average method - Filter() used
plot(APAC_Consumer_Sales.TS)
lines(APAC_smoothedseries,col='blue',lwd=2)

#Smoothing time series using HoltWinters method which is also exponantial smoothning

plot(APAC_Consumer_Sales.TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.02, 0.1, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  APAC_HW_smoothedseries <- HoltWinters(APAC_Consumer_Sales.TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(APAC_HW_smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)
APAC_HW_smoothedseries <- HoltWinters(APAC_Consumer_Sales.TS, alpha=.3,beta=FALSE, gamma=FALSE)

plot(APAC_Consumer_Sales.TS)
lines(fitted(APAC_HW_smoothedseries)[,1],col='red',lwd=2)

#Plotting the smoothend timeseries using both Average moving and HoltWinters method
plot(APAC_Consumer_Sales.TS)
lines(APAC_smoothedseries,col='blue',lwd=2)
lines(fitted(APAC_HW_smoothedseries)[,1],col='red',lwd=2)

#Building a model on the smoothed time series using classical decomposition

APAC_Consumer_Salesdf <- data.frame(cbind(APAC_Consumer_Aggregate$Order_Month,APAC_smoothedseries))
colnames(APAC_Consumer_Salesdf) <- c("Month","Sales")

APAC_Consumer_Salesdf$Sales <- as.numeric(as.character((APAC_Consumer_Salesdf$Sales)))

APAC_Consumer_Salesdf$Month <-as.numeric(APAC_Consumer_Salesdf$Month)

str(APAC_Consumer_Salesdf)

#Again creating time series from dataframe created for smoothseries
APAC_Consumer_Sales_TS <- ts(APAC_Consumer_Salesdf$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_Consumer_Sales_TS)

#Creating train & validation sets
ntest <- 6
nTrain <- length(APAC_Consumer_Sales_TS)-ntest
train.ts <- window(APAC_Consumer_Sales_TS,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_Consumer_Sales_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test.ts <- ts(APAC_Consumer_Salesdf$Sales,frequency=12,start=c(2015,1),end=c(2015,6))

train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(APAC_smoothedseries,ylab="Sales",xlab="Time",bty='l',xaxt='n',col="red",title=c("APAC Consumer Sales"))  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")

#Checking MAPE for our linear model
APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_consumer_accuracy #MAPE=13.54141
#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model
autoARIMA_ts <- auto.arima(train.ts)
#Using tsdiag - a generic function to plot time-series diagnostics.
tsdiag(autoARIMA_ts)
autoarima_forecast <- forecast(autoARIMA_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc
#Values for Training and Test sets are as follows :
#MAPE : 
#1. TRAINING - 5.52011
#2. TEST     - 14.48387

#Forecast for next 6 months i.e. 49 to 54(Jan 2015 to June 2015) using Auto ARIMA model
autoarima_ts <- auto.arima(APAC_Consumer_Sales_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
plot(autoarima_forecast)

#------------------------------------APAC CONSUMER QUANTITY FORECAST----------------------------

APAC_Consumer_Quantity.TS <- ts(APAC_Consumer_Aggregate$Quantity,frequency = 12,start = c(2011,1),end = c(2014,12))
plot(APAC_Consumer_Quantity.TS)

#-------------------------------------SMOOTHNING TIME SERIES-------------------------------------

#Using HoltWinter Method
plot(APAC_Consumer_Quantity.TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.02, 0.3, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  APAC_smoothedseries <- HoltWinters(APAC_Consumer_Quantity.TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(APAC_smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)

#Plotting the timeseies
plot(APAC_Consumer_Quantity.TS)
lines(fitted(APAC_smoothedseries)[,1],col='red',lwd=2)

#Using Moving Average Method
w <- 4
APAC_Consumer_Quant_Smoothed <- stats::filter(APAC_Consumer_Quantity.TS,filter=rep(1/w,w,method='convolution',sides=2)) 
#Adding NA values to left & right introduced by smoothing
#Smoothing left end of the time series
diff <- APAC_Consumer_Quant_Smoothed[w+2] - APAC_Consumer_Quant_Smoothed[w+1]
for (i in seq(w,1,-1)) {
  APAC_Consumer_Quant_Smoothed[i] <- APAC_Consumer_Quant_Smoothed[i+1] - diff
}
#Smoothing right end of the time series
Length_APAC_Quant <- length(APAC_Consumer_Quant_Smoothed)
diff <- APAC_Consumer_Quant_Smoothed[Length_APAC_Quant-w] - APAC_Consumer_Quant_Smoothed[Length_APAC_Quant-w-1]
for (i in seq(Length_APAC_Quant-w+1, Length_APAC_Quant)) {
  APAC_Consumer_Quant_Smoothed[i] <- APAC_Consumer_Quant_Smoothed[i-1] + diff
}

#Converting smoothed series to data frame based on order month
#Using moving average smoothed time series here
APAC_Consumer_Quantitydf <- data.frame(cbind(APAC_Consumer_Aggregate$Order_Month,APAC_Consumer_Quant_Smoothed))
colnames(APAC_Consumer_Quantitydf) <- c("Month","Quantity")

#Changing Quantity & month type to numeric
APAC_Consumer_Quantitydf$Quantity <- as.numeric(as.character((APAC_Consumer_Quantitydf$Quantity)))
APAC_Consumer_Quantitydf$Month <- as.numeric(APAC_Consumer_Quantitydf$Month)
str(APAC_Consumer_Quantitydf)

#Plotting Moving Average
plot(APAC_Consumer_Quantity.TS)
lines(APAC_Consumer_Quant_Smoothed,col='blue',lwd=2)

#Plotting smoothed series both Moving Average & HoltWinters
plot(APAC_Consumer_Quantity.TS)
lines(APAC_Consumer_Quant_Smoothed,col='blue',lwd=2)
lines(fitted(APAC_smoothedseries)[,1],col='red',lwd=2)

#Creating time series from dataframe got after smoothning of series
APAC_Consumer_Quantity_TS <- ts(APAC_Consumer_Quantitydf$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))

#Creating train & validation sets for Quantity forecast
ntest <- 6
nTrain <- length(APAC_Consumer_Quantity_TS)-ntest
train.ts <- window(APAC_Consumer_Quantity_TS,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_Consumer_Quantity_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test.ts <- ts(APAC_Consumer_Quantitydf$Quantity,frequency=12,start=c(2015,1),end=c(2015,6))

#Creating Regression model for trend and seasonality

#Using Classical Decomposition
#Regression model based on trend & seasonality
#Using tslm - tslm is used to fit linear models to time series including trend and seasonality components
#tsml function is a wrapper for lm() which allows varibales "trend" & "seasonality".
#sin(2*pi*trend/12)
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(APAC_Consumer_Quant_Smoothed,ylab="Quantity",xlab="Time",bty='l',xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")
#Checking MAPE for our linear model
APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_consumer_accuracy #MAPE=9.580049
#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Using Auto ARIMA model for Quanity forecast

#Model 2: Auto ARIMA model
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc
#Values for Training and Test sets are as follows :
#MAPE : 
#1. TRAINING - 4.8018
#2. TEST     - 10.36941

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Regresssion model
train.lm.model <- tslm(APAC_Consumer_Quantity_TS~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=0)
train.lm.total.forecast
plot(train.lm.total.forecast,col="black")

#-----------------------------------------MODEL FOR EU CONSUMER----------------------------

#---------------------------------------SALES FORECAST FOR EU------------------------------

#As Order.Date has value from 2011-01-01 to 2014-12-31 we choose start and end accordingly
#Frequency i.e. no of observation per unit time is set as 12
title <- c("EU Consumer Sales: JAN 2011 to DEC 2014")
EU_Consumer_Sales.TS <- ts(EU_Consumer_Aggregate$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_Consumer_Sales.TS)

#--------------------------------------SMOOTHNING TIME SERIES----------------------------------
#Smoothing time series using Moving Average method
#Choosing the window size as 4 as it is best fit
w <- 4
EU_smoothedseries <-stats::filter(EU_Consumer_Sales.TS,filter=rep(1/w,w,method='convolution',sides=2)) 
View(EU_smoothedseries)

#NA values are introduced to left & right of series by smoothing
#Smoothing left end of the time series
diff <- EU_smoothedseries[w+2] - EU_smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  EU_smoothedseries[i] <- EU_smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
Length_EU <- length(EU_smoothedseries)
diff <- EU_smoothedseries[Length_EU-w] - EU_smoothedseries[Length_EU-w-1]
for (i in seq(Length_EU-w+1, Length_EU)) {
  EU_smoothedseries[i] <- EU_smoothedseries[i-1] + diff
}

#Converting smoothed series to data frame based on month

EU_Consumer_Salesdf <- data.frame(cbind(EU_Consumer_Aggregate$Order_Month,EU_smoothedseries))
colnames(EU_Consumer_Salesdf) <- c("Month","Sales")
str(EU_Consumer_Salesdf)

#Plotting smoothed series using Moving Average method - Filter() used
plot(EU_Consumer_Sales.TS)
lines(EU_smoothedseries,col='blue',lwd=2)

#Smoothing time series using HoltWinters method which is also exponantial smoothning

plot(EU_Consumer_Sales.TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.02, 0.1, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  EU_HW_smoothedseries <- HoltWinters(EU_Consumer_Sales.TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(EU_HW_smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)
EU_HW_smoothedseries <- HoltWinters(EU_Consumer_Sales.TS, alpha=.3,beta=FALSE, gamma=FALSE)

plot(EU_Consumer_Sales.TS)
lines(fitted(EU_HW_smoothedseries)[,1],col='red',lwd=2)

#Plotting the smoothend timeseries using both Average moving and HoltWinters method
plot(EU_Consumer_Sales.TS)
lines(EU_smoothedseries,col='blue',lwd=2)
lines(fitted(EU_HW_smoothedseries)[,1],col='red',lwd=2)

#Building a model on the smoothed time series using classical decomposition

EU_Consumer_Salesdf <- data.frame(cbind(EU_Consumer_Aggregate$Order_Month,EU_smoothedseries))
colnames(EU_Consumer_Salesdf) <- c("Month","Sales")

EU_Consumer_Salesdf$Sales <- as.numeric(as.character((EU_Consumer_Salesdf$Sales)))

EU_Consumer_Salesdf$Month <-as.numeric(EU_Consumer_Salesdf$Month)

str(EU_Consumer_Salesdf)

#Again creating time series from dataframe created for smoothseries
EU_Consumer_Sales_TS <- ts(EU_Consumer_Salesdf$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_Consumer_Sales_TS)

#Creating train & validation sets
ntest <- 6
nTrain <- length(EU_Consumer_Sales_TS)-ntest
train.ts <- window(EU_Consumer_Sales_TS,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_Consumer_Sales_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test.ts <- ts(EU_Consumer_Salesdf$Sales,frequency=12,start=c(2015,1),end=c(2015,6))

train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(EU_smoothedseries,ylab="Sales",xlab="Time",bty='l',xaxt='n',col="red",title=c("EU Consumer Sales"))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")

#Checking MAPE for our linear model
EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
EU_consumer_accuracy #MAPE=14.68292
#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc
#MAPE=6.867693 for Training set & 21.395597 for Validation set for Auto ARIMA

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto ARIMA model
autoarima_ts <- auto.arima(EU_Consumer_Sales_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
plot(autoarima_forecast)
autoarima_forecast

#---------------------------------------Quantity FORECAST FOR EU------------------------------

#As Order.Date has value from 2011-01-01 to 2014-12-31 we choose start and end accordingly
#Frequency i.e. no of observation per unit time is set as 12
title <- c("EU Consumer Quantity: JAN 2011 to DEC 2014")
EU_Consumer_Quantity.TS <- ts(EU_Consumer_Aggregate$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_Consumer_Quantity.TS)

#--------------------------------------SMOOTHNING TIME SERIES----------------------------------
#Smoothing time series using Moving Average method
#Choosing the window size as 4 as it is best fit
w <- 4
EU_smoothedseries <-stats::filter(EU_Consumer_Quantity.TS,filter=rep(1/w,w,method='convolution',sides=2)) 
View(EU_smoothedseries)

#NA values are introduced to left & right of series by smoothing
#Smoothing left end of the time series
diff <- EU_smoothedseries[w+2] - EU_smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  EU_smoothedseries[i] <- EU_smoothedseries[i+1] - diff
}

#Smoothing right end of the time series
Length_EU <- length(EU_smoothedseries)
diff <- EU_smoothedseries[Length_EU-w] - EU_smoothedseries[Length_EU-w-1]
for (i in seq(Length_EU-w+1, Length_EU)) {
  EU_smoothedseries[i] <- EU_smoothedseries[i-1] + diff
}

#Converting smoothed series to data frame based on month

EU_Consumer_Quantitydf <- data.frame(cbind(EU_Consumer_Aggregate$Order_Month,EU_smoothedseries))
colnames(EU_Consumer_Quantitydf) <- c("Month","Quantity")
str(EU_Consumer_Quantitydf)

#Plotting smoothed series using Moving Average method - Filter() used
plot(EU_Consumer_Quantity.TS)
lines(EU_smoothedseries,col='blue',lwd=2)

#Smoothing time series using HoltWinters method which is also exponantial smoothning

plot(EU_Consumer_Quantity.TS)
cols <- c("red", "blue", "green", "black")
alphas <- c(0.02, 0.1, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  EU_HW_smoothedseries <- HoltWinters(EU_Consumer_Quantity.TS, alpha=alphas[i],beta=FALSE, gamma=FALSE)
  lines(fitted(EU_HW_smoothedseries)[,1], col=cols[i], lwd=2)
}
legend("topleft", labels, col=cols, lwd=2)
EU_HW_smoothedseries <- HoltWinters(EU_Consumer_Quantity.TS, alpha=.3,beta=FALSE, gamma=FALSE)

plot(EU_Consumer_Quantity.TS)
lines(fitted(EU_HW_smoothedseries)[,1],col='red',lwd=2)

#Plotting the smoothend timeseries using both Average moving and HoltWinters method
plot(EU_Consumer_Quantity.TS)
lines(EU_smoothedseries,col='blue',lwd=2)
lines(fitted(EU_HW_smoothedseries)[,1],col='red',lwd=2)

#Building a model on the smoothed time series using classical decomposition

EU_Consumer_Quantitydf <- data.frame(cbind(EU_Consumer_Aggregate$Order_Month,EU_smoothedseries))
colnames(EU_Consumer_Quantitydf) <- c("Month","Quantity")

EU_Consumer_Quantitydf$Quantity <- as.numeric(as.character((EU_Consumer_Quantitydf$Quantity)))

EU_Consumer_Quantitydf$Month <-as.numeric(EU_Consumer_Quantitydf$Month)

str(EU_Consumer_Quantitydf)

#Again creating time series from dataframe created for smoothseries
EU_Consumer_Quantity_TS <- ts(EU_Consumer_Quantitydf$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(EU_Consumer_Quantity_TS)

#Creating train & validation sets
ntest <- 6
nTrain <- length(EU_Consumer_Quantity_TS)-ntest
train.ts <- window(EU_Consumer_Quantity_TS,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(EU_Consumer_Quantity_TS,start=c(2011,nTrain+1), end=c(2011,nTrain+ntest))
test.ts <- ts(EU_Consumer_Quantitydf$Quantity,frequency=12,start=c(2015,1),end=c(2015,6))

train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(EU_smoothedseries,ylab="Sales",xlab="Time",bty='l',xaxt='n',col="red",title=c("EU Consumer Quantity"))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")

#Checking MAPE for our linear model
EU_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
EU_consumer_accuracy #MAPE=4.878414
#ACF and PACF plots 
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

#Model 2: Auto ARIMA model
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
plot(autoarima_forecast)
autoarima_acc
#MAPE=5.928160 for Training set & 7.101892 for Validation set for Auto ARIMA

#Forecast for months 49 to 54(Jan 2015 to June 2015) using Auto ARIMA model
autoarima_ts <- auto.arima(EU_Consumer_Quantity_TS)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
plot(autoarima_forecast)
autoarima_forecast

