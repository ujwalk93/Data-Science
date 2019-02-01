#1. Business Understanding

#“Global Mart” is an online store super giant having worldwide operations. 
#It takes orders and delivers across the globe and deals with all the 
#major product categories - consumer, corporate & home office.  

#The main objective is to forecast the sales and the demand for the next 6 months
#using time series analysis in order to manage revenue and inventory accordingly.

#The store caters to 7 different market segments and in 3 major categories.
#(i.e. 21 segments in total)

#This is done by finding out the 2 most profitable (and consistent) segments 
#from the 21 total segments and forecasting the sales and demand for these segments.

#set the working directory
setwd("D:\\PG_Diploma\\TimeSeries\\CaseStudy")

#Loading the data
superstore <- read.csv("Global Superstore.csv")

#Loading the requiredlibraries
library(zoo)
library(forecast)
library(tseries)
library(ggplot2)

#2. Data understanding and cleaning

#understanding the dimensions and structure of dataset
dim(superstore)
str(superstore)
#Thus, this represent transaction level data, where each row represents a particular order made on the online store.
#From the above commands, it is clear that there are 51290 observations and 24 attributes (columns).

#exploring the data
summary(superstore)

#It is clear that discount, sales, quantity and 
#shipping cost columns do not have negative values

#check for missing values
sapply(superstore, function(x) length(which(x == '')))
#no missing values are present

#Order date and Ship date should be converted from factor to date
superstore$Order.Date<-as.Date(superstore$Order.Date,"%d-%m-%Y")
superstore$Ship.Date<-as.Date(superstore$Ship.Date,"%d-%m-%Y")

#Check for NA values
sapply(superstore, function(x) sum(is.na(x))) 
#Postal.Code is the only varaible with na values
#it can be removed as it will nto be of major help for our analysis
superstore <- superstore[,-12]

#removing Row.ID as it is not required for the analysis
superstore <- superstore[,-1]

#check for duplicates
length(unique(superstore$Row.ID)) #51290 i.e. the total number of rows in superstore
sum(duplicated(superstore)) #0 i.e. no duplicate rows

#Extract month and year from oder Date
superstore$Order.Month<-format(superstore$Order.Date,"%B-%Y")

#3. Data preparation and EDA

#Ordering the required columns for analysis
superstore <- superstore[order(superstore$Order.Date, decreasing = F),]
columns <- as.vector(c("Order.Month","Sales","Quantity","Profit","Market","Segment"))
superstore <- superstore[, columns]

levels(superstore$Market)
levels(superstore$Segment)

market <- as.character(unique(superstore$Market))
segment <- as.character(unique(superstore$Segment))
names <- c(sapply(market, function(x) paste(x,segment,sep = "_")))

#Thus, there are 7 geographical market locations and 3 customer segments
#which are present

#Profit vs Market and Profit vs Segment
ggplot(superstore, aes(x = Market,fill=Profit))+geom_bar(fill="blue")+labs(x="Market",y="Profit")
ggplot(superstore, aes(x = Segment,fill=Profit))+geom_bar(fill="green")+labs(x="Segment",y="Profit")
#It is clear that the APAC market and the consumer segment are the most profitable ones

#Segmenting the whole superstore into the 21 subsets based on 
#the 7 markets and the 3 customer segment levels.

#creating 21 buckets by filtering based on market and segment
for (j in 1 : length(market)){
  for (k in 1 : length(segment)){
    name <- paste(market[j],segment[k], sep = "_")
    #filter the data for 21 buckets
    assign(name, superstore[which(superstore$Market==market[j] & 
                                    superstore$Segment==segment[k]),])
    #aggregate the filtered data based on month and year
    assign(name, aggregate(.~Order.Month + Market + Segment, 
                           superstore[which(superstore$Market==market[j] 
                                            & superstore$Segment==segment[k]),], 
                      sum))
  }
}

df_list <- list(Africa_Consumer, Africa_Corporate, `Africa_Home Office`, 
                APAC_Consumer, APAC_Corporate, `APAC_Home Office`, 
                Canada_Consumer, Canada_Corporate, `Canada_Home Office`,
                EMEA_Consumer, EMEA_Corporate, `EMEA_Home Office`, 
                EU_Consumer, EU_Corporate, `EU_Home Office`, 
                LATAM_Consumer, LATAM_Corporate, `LATAM_Home Office`, 
                US_Consumer, US_Corporate, `US_Home Office`)

#Calculate mean for 21 buckets
avg <- sapply(df_list, function(x) mean(x[[6]]))
avg
#NOTE: Profit is 6th column in each data frame
#NOTE: EU_Consumer(mean = 3930.99), APAC_Consumer(mean = 4642.03) are the 2 buckets with highest profits

#Calculate CV for all 21 buckets
cv <- sapply(df_list, function(x) sd(x[[6]])/mean(x[[6]]))
cv
#NOTE: EU_Consumer(cv = 0.62), APAC_Consumer(cv = 0.63) are the 2 buckets with least cv
#Hence considering these 2 buckets

#Ploting cv and mean of each bucket
buckets <- data.frame(BucketNames = sort(names),Mean=avg,CofficientOfVariation=cv)
ggplot(data=buckets, aes(x = BucketNames, y = Mean, group = 1)) + geom_line() +geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(data=buckets, aes(x = BucketNames, y = CofficientOfVariation, group = 1))  + geom_line() +geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Order the 2 buckects in increasing order of month and year to proceed with time series
APAC_Consumer<-APAC_Consumer[order(as.yearmon(APAC_Consumer$Order.Month,format="%B-%Y")),]
EU_Consumer<-EU_Consumer[order(as.yearmon(EU_Consumer$Order.Month,format="%B-%Y")),]

#Remove MArket and Segment columns
APAC_Consumer <- APAC_Consumer[, c("Order.Month","Sales","Quantity")]
APAC_Consumer$Order.Month <- c(1:nrow(APAC_Consumer))
EU_Consumer <- EU_Consumer[, c("Order.Month","Sales","Quantity")]
EU_Consumer$Order.Month <- c(1:nrow(EU_Consumer))

#4. Model Building and Evaluation

#Time series modeling for APAC_Consumer Sales
#============================================
ylab <- c("Sales")
xlab <- c("Months from Jan 2011 to Dec 2014")
title <- c("APAC Consumer Sales from Jan. 2011 to Dec 2014")

total_timeser_APAC_sales <- ts(APAC_Consumer$Sales)
plot(total_timeser_APAC_sales, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')

indata <- APAC_Consumer[1:42,]
outdata <- APAC_Consumer[43:48,]
timevals_in <- indata$Order.Month
timevals_out <- outdata$Order.Month

timeser <- ts(indata$Sales)
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')

#Modelling using classical decomposition
#---------------------------------------

#Smoothing the series - Moving Average Smoothing
moving_avg_smoothing <- function(w, timeser){
  smoothedseries <- filter(timeser, 
                           filter=rep(1/(2*w+1),(2*w+1)), 
                           method='convolution', sides=2)
  
  #Smoothing left end of the time series
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  #Smoothing right end of the time series
  n <- length(timeser)
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  } 
  return(smoothedseries)
}
smoothedseries <- moving_avg_smoothing(3, timeser)

#Plot the smoothed time series
lines(smoothedseries, col="blue", lwd=2)
#NOTE: Clear treand and seasonality are visible but might be a better fit can be found

#Smoothing the series - Exponential
cols <- c("red", "green", "yellow", "orange")
alphas <- c(0.01, 0.1,0.2,0.5)
#labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries_exp <- HoltWinters(timeser, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries_exp)[,1], col=cols[i], lwd=2)
}
#0.5 is over fitting and 0.01, 0.1 are under fitting 
#Hence proceeding with the previous averaging out values
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#Convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Locally predictable part of time series modelled as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit
#ARIMA(0,0,0) means no residual series
#log likelihood=-446.83  AIC=895.66   AICc=895.76   BIC=897.4

#Check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
#p=0.01 => we can reject the null hypothesis => time series is stationary
kpss.test(resi)
#p=0.1 > 0.5 => fail to reject null hypothesis => time series is stationary

#Evaluate the model using MAPE
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

#Compare our prediction with the actual values, using MAPE
fcast <- global_pred_out
MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec #37.38305

#Plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser_APAC_sales, col = "black", ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(class_dec_pred, col = "red")
#not a very good prediction according to graph which might be a result of over smoothening
#Hence we need to redo the process considering alpha = 0.5 for exponential smoothening

#Modelling with classical decomposition with exponential smoothening technique
#-----------------------------------------------------------------------------
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
smoothedseries <- HoltWinters(timeser, alpha=0.5,
                                  beta=FALSE, gamma=FALSE)
lines(fitted(smoothedseries)[,1], col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#Convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(fitted(smoothedseries)[,1])))
colnames(smootheddf) <- c('Month', 'Sales')

smoothedseries$seasonal #"additive"
#Fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
lmfit <- lm(Sales ~ sin(0.5*Month) + poly(Month,3) + cos(0.5*Month) 
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='green', lwd=2)

#Does not seem to be a very good fit
#Lets consider only trend and proceed with simple linear regression
lmfit <- lm(Sales ~ Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='Red', lwd=2)

#Locally predictable part of time series modelled as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred) #MA(0) as ACF = 0 for h > 0 
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit
#ARIMA(0,0,0) means no residual series
#log likelihood=-453.41  AIC=908.82   AICc=908.92   BIC=910.56
#NOTE These values are even bad than the previous multiplicative model

#Check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
#p=0.0156 => we can reject the null hypothesis => time series is stationary
kpss.test(resi)
#p=0.1 > 0.5 => fail to reject null hypothesis => time series is stationary

#Evaluate the model using MAPE
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

#Compare our prediction with the actual values, using MAPE
fcast <- global_pred_out
MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec #26.087
#much better than the previous value

#Plot the predictions along with original values
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser_APAC_sales, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(class_dec_pred, col = "red")
#Through the MAPE value is much better than previuos the graph doesnt seem to be a best possible fit
#Hence lets redo it by try moving average soomthing with w = 2

#Modelling with classical decomposition with moving average smoothening technique with different window
#------------------------------------------------------------------------------------------------------
smoothedseries <- moving_avg_smoothing(2, timeser)
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty = 'l',xaxt = 'n')
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Fit a multiplicative model with trend and seasonality to the data
lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
              + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Locally predictable seriess
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred) #MA(0) as ACF = 0 for h > 0 
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit #ARIMA(0,0,0)
#log likelihood=-444.67  AIC=891.33   AICc=891.43   BIC=893.07

#Check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
#p=0.01 => we can reject the null hypothesis => time series is stationary
kpss.test(resi)
#p=0.1 > 0.5 => fail to reject null hypothesis => time series is stationary

#Evaluate the model using MAPE
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

#Compare our prediction with the actual values, using MAPE
fcast <- global_pred_out
MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec #23.865
#much better than the previuos value

#Plot the predictions along with original values
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser_APAC_sales, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(class_dec_pred, col = "red")
#looks like the best model achieved till now


#Modelling using ARIMA fit
#--------------------------
autoarima <- auto.arima(timeser)
autoarima #ARIMA(0,1,1)
#log likelihood=-447.11  AIC=898.23   AICc=898.55   BIC=901.66
tsdiag(autoarima)
plot(autoarima$x, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(fitted(autoarima), col="red")

#Check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary") #p = 0.01 => stationary
kpss.test(resi_auto_arima) #p = 0.1 => stationary

#Evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima #27.68952

#Plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser_APAC_sales, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(auto_arima_pred, col = "red")

#NOTE: Classical decompostion with Smoothening using moving average technique and 
#      modelling the global part with multiplicative model with trend and 
#      seasonality gave the best possible results for sales of APAC_Consumer

#Time series modeling for APAC_Consumer Quantity
#===============================================
ylab <- c("Quantity")
xlab <- c("Months from Jan 2011 to Dec 2014")
title <- c("APAC Consumer Quantity from Jan. 2011 to Dec 2014")

total_timeser_APAC_quant <- ts(APAC_Consumer$Quantity)
plot(total_timeser_APAC_quant, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')

timeser <- ts(indata$Quantity)
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')

#Modelling using classical decomposition
#---------------------------------------
#Smoothening using moving average
cols <- c("blue","red", "green", "yellow", "orange")
for(i in 1:3){
  smoothedseries <- moving_avg_smoothing(i, timeser)
  lines(smoothedseries, col=cols[i], lwd=2)
}
#NOTE: window value 1 seems to be the best
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
smoothedseries <- moving_avg_smoothing(1, timeser)
lines(smoothedseries, col="blue", lwd=2)

#Smoothening using exponential method
#Smoothing the series - Exponential
cols <- c("red", "green", "yellow", "orange")
alphas <- c(0.01, 0.1,0.2,0.5)
#labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries_exp <- HoltWinters(timeser, alpha=alphas[i],
                                    beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries_exp)[,1], col=cols[i], lwd=2)
}
#NOTE: moving average with window value 1 seems to be the best still
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
smoothedseries <- moving_avg_smoothing(1, timeser)
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series
#Convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Fit a multiplicative model with trend and seasonality to the data
lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Locally predictable part of time series modelled as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit #ARIMA(0,0,0)
#log likelihood=-253.71  AIC=509.42   AICc=509.52   BIC=511.16

#Check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary") #p-value=0.01 => stationary
kpss.test(resi) #p-value=0.1 => stationary

#Evaluate the model using MAPE
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

#Compare our prediction with the actual values, using MAPE
fcast <- global_pred_out
MAPE_class_dec <- accuracy(fcast,outdata[,3])[5]
MAPE_class_dec #62.10289
#MAPE value too high => not a very good model

#Plot the predictions along with original values
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser_APAC_quant, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(class_dec_pred, col = "red")

#Modelling using ARIMA fit
#--------------------------
autoarima <- auto.arima(timeser)
autoarima #ARIMA(0,1,0)
#log likelihood=-266.07  AIC=534.14   AICc=534.24   BIC=535.85
tsdiag(autoarima)
plot(autoarima$x, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(fitted(autoarima), col="red")

#Check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary") #p = 0.01 => stationary
kpss.test(resi_auto_arima) #p = 0.1 => stationary

#Evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,3])[5]
MAPE_auto_arima #26.24458

#Plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser_APAC_quant, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(auto_arima_pred, col = "red")

#NOTE: Classical decompostion with Smoothening using moving average technique(window 1) and 
#      modelling the global part with multiplicative model with trend and seasonality 
#      gave not so good results compare to ARIMA for quantity of APAC_Consumer.


#Time series modeling for EU_Consumer Sales
#===============================================
ylab <- c("Sales")
xlab <- c("Months from Jan 2011 to Dec 2014")
title <- c("EU Consumer Sales from Jan. 2011 to Dec 2014")

total_timeser_EU_sales <- ts(EU_Consumer$Sales)
plot(total_timeser_EU_sales, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')

indata <- EU_Consumer[1:42,]
outdata <- EU_Consumer[43:48,]
timevals_in <- indata$Order.Month
timevals_out <- outdata$Order.Month

timeser <- ts(indata$Sales)
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')

#Modelling using classical decomposition
#---------------------------------------
#Smoothening using moving average
cols <- c("blue","red", "green", "yellow", "orange")
for(i in 1:3){
  smoothedseries <- moving_avg_smoothing(i, timeser)
  lines(smoothedseries, col=cols[i], lwd=2)
}
#NOTE: window value 1 seems to be the best
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
smoothedseries <- moving_avg_smoothing(1, timeser)
lines(smoothedseries, col="blue", lwd=2)

#Smoothening using exponential method
#Smoothing the series - Exponential
cols <- c("red", "green", "yellow", "orange")
alphas <- c(0.01, 0.1,0.2,0.5)
#labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries_exp <- HoltWinters(timeser, alpha=alphas[i],
                                    beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries_exp)[,1], col=cols[i], lwd=2)
}
#NOTE: moving average with window value 1 seems to be the best still
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
smoothedseries <- moving_avg_smoothing(1, timeser)
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series
#Convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Fit a additive model with trend and seasonality to the data
lmfit <- lm(Sales ~ sin(0.5*Month) + poly(Month,3) + cos(0.5*Month) + poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
lines(timevals_in, global_pred, col='green', lwd=2)

#Fit a multiplicative model with trend and seasonality to the data
lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
lines(timevals_in, global_pred, col='red', lwd=2)
#NOTE: Multiplecative model looks like a better fit

#Locally predictable part of time series modelled as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit #ARIMA(0,0,0)
#log likelihood=-444.8  AIC=891.61   AICc=891.71   BIC=893.35

#Check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary") #p-value=0.01 => stationary
kpss.test(resi) #p-value=0.1 => stationary

#Evaluate the model using MAPE
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

#Compare our prediction with the actual values, using MAPE
fcast <- global_pred_out
MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec #92.95788
#MAPE value too high => not a good model

#Plot the predictions along with original values
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser_EU_sales, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(class_dec_pred, col = "red")
#Not a good model. Hence lets re-build the model with moving avaerage smoothing 
#with window size 2

#Modeling using Classical decompostion with different moving average smoothing with window 2
#-------------------------------------------------------------------------------------------
#NOTE: moving average with window value 1 seems to be the best still
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
smoothedseries <- moving_avg_smoothing(2, timeser)
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series
#Convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Fit a additive model with trend and seasonality to the data as amplitude is not consistently increasing
lmfit <- lm(Sales ~ sin(0.5*Month) + poly(Month,3) + cos(0.5*Month) + poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
lines(timevals_in, global_pred, col='green', lwd=2)

#Locally predictable part of time series modelled as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit #ARIMA(0,0,0)
#log likelihood=-446.34  AIC=894.67   AICc=894.77   BIC=896.41

#Check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary") #p-value=0.01 => stationary
kpss.test(resi) #p-value=0.1 => stationary

#Evaluate the model using MAPE
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

#Compare our prediction with the actual values, using MAPE
fcast <- global_pred_out
MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec #24.045
#MAPE value very low when compared to previous model => a good model

#Plot the predictions along with original values
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser_EU_sales, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(class_dec_pred, col = "red")


#Modelling using ARIMA fit
#--------------------------
autoarima <- auto.arima(timeser)
autoarima #ARIMA(2,1,0)
#log likelihood=-445.84  AIC=897.67   AICc=898.32   BIC=902.81
tsdiag(autoarima)
plot(autoarima$x, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(fitted(autoarima), col="red")

#Check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary") #p = 0.01 => stationary
kpss.test(resi_auto_arima) #p = 0.1 => stationary

#Evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima #28.9226

#Plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser_EU_sales, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(auto_arima_pred, col = "red")

#NOTE: Classical decompostion with Smoothening using moving average technique(window 2) and 
#      modelling the global part with additive model with trend and seasonality 
#      gave good results compare to ARIMA for Sales of EU_Consumer.


#Time series modeling for EU_Consumer Quantity
#===============================================
ylab <- c("Quantity")
xlab <- c("Months from Jan 2011 to Dec 2014")
title <- c("EU Consumer Quantity from Jan. 2011 to Dec 2014")

total_timeser_EU_quant <- ts(EU_Consumer$Quantity)
plot(total_timeser_EU_quant, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')

timeser <- ts(indata$Quantity)
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')

#Modelling using classical decomposition
#---------------------------------------
#Smoothening using moving average
cols <- c("blue","red", "green", "yellow", "orange")
for(i in 1:3){
  smoothedseries <- moving_avg_smoothing(i, timeser)
  lines(smoothedseries, col=cols[i], lwd=2)
}
#NOTE: window value 1 seems to be the best
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
smoothedseries <- moving_avg_smoothing(1, timeser)
lines(smoothedseries, col="blue", lwd=2)

#Smoothening using exponential method
#Smoothing the series - Exponential
cols <- c("red", "green", "yellow", "orange")
alphas <- c(0.01, 0.1,0.2,0.5)
#labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries_exp <- HoltWinters(timeser, alpha=alphas[i],
                                    beta=FALSE, gamma=FALSE)
  lines(fitted(smoothedseries_exp)[,1], col=cols[i], lwd=2)
}
#NOTE: moving average with window value 1 seems to be the best still
plot(timeser, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
smoothedseries <- moving_avg_smoothing(1, timeser)
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series
#Convert the time series to a dataframe
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Fit a additive model with trend and seasonality to the data
lmfit <- lm(Quantity ~ sin(0.5*Month) + poly(Month,3) + cos(0.5*Month) 
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Locally predictable part of time series modelled as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit #ARIMA(0,0,1)
#log likelihood=-251.76  AIC=509.52   AICc=510.16   BIC=514.74

#Check if the residual series is white noise
resi <- local_pred-fitted(armafit)
#adf.test(resi,alternative = "stationary") #p-value=0.1999 => not stationary
kpss.test(resi) #p-value=0.1 => stationary

#Evaluate the model using MAPE
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

#Compare our prediction with the actual values, using MAPE
fcast <- global_pred_out
MAPE_class_dec <- accuracy(fcast,outdata[,3])[5]
MAPE_class_dec #27.78156
#MAPE value is not too high => a good model

#Plot the predictions along with original values
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser_EU_quant, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(class_dec_pred, col = "red")

#Modelling using ARIMA fit
#--------------------------
autoarima <- auto.arima(timeser)
autoarima #ARIMA(2,1,0)
#log likelihood=-261.9  AIC=529.8   AICc=530.44   BIC=534.94
tsdiag(autoarima)
plot(autoarima$x, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(fitted(autoarima), col="red")

#Check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary") #p = 0.045 => stationary
kpss.test(resi_auto_arima) #p = 0.1 => stationary

#Evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,3])[5]
MAPE_auto_arima #30.13319

#Plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser_EU_quant, ylab = ylab, xlab = xlab, title = title, bty='l',xaxt='n')
lines(auto_arima_pred, col = "red")

#NOTE: Classical decompostion with Smoothening using moving average technique(window 1) and 
#      modelling the global part with additive model with trend and seasonality 
#      gave better results compare to ARIMA for quantity of EU_Consumer.

#NOTE: N.ahead=6 parameter in predict() function is used to forecast data for next 6 months.
#One important assumption used is that the time series are modelled only after converting them
#into stationary ones.
#Also, the noise components are ignored or removed during the course of this analysis.
#The MAPE values of all the models are between 24 and 30, which is suitable for forecasting.
#Hence, the models are suitable for the analysis.

