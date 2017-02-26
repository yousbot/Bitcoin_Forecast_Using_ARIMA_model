library(xlsx)
library(openxlsx)

#Reading the raw data from an xlsx.

da <- read.xlsx("BC_Chart_Hist.xlsx", sheet="Feuil1", colNames=TRUE, detectDates=TRUE)

#The Price column is read as a factor, we need to convert it to an integer

da$Price <- as.numeric(as.character(da$Price))

# After conversion, we need to delete the misconverted values 

da$Price[which(is.nan(da$Price))] = NA
da$Price[which(da$Price==Inf)] = NA
da <- da[!(is.na(da$Price) | da$Price==""),]

# The summary of the sequence 

# Time series Forecasting using Exponential Smoothing 
#We don't have to check if our sequence follows a systematic trend

dac <- da # For backup

# Create a time series object
dac <- ts(dac[,2],start=2011, freq=365)
# Performing a HoltWinter procedure to capture seasonality factors
HoltWinters(dac)

plot(dac)

# We save the fit of our model 

dac.hw <- HoltWinters(dac)
# Call predict function for the next week 

predict(dac.hw, n.ahead=7)
plot(dac, xlim=c(2011,2017))
#prediction for the next month
lines(predict(dac.hw, n.ahead=30),col=2)



# Now, we'll procede with a Time Series Forecasting based ARIMA model

# 1 --> Model's Identification

sim.ar <- arima.sim(list(ar=c(0.4,0.4)),n=100)
sim.ma <- arima.sim(list(ma=c(0.6,-0.4)),n=100)
par(mfrow=c(2,2))
acf(sim.ar,main="ACF of AR(2) process")
acf(sim.ma,main="ACF of MA(2) process")
pacf(sim.arr,main="PACF of AR(2) process")
pacf(sim.ar,main="PACF of AR(2) process")
pacf(sim.ma,main="PACF of MA(2) process")


# 2 --> Parameter's estimation 

#by precising the Alpha, Beta and Gama ( Beta is 0 to determine the non validity of the trend smoothing factor )

fit <- arima(dac, order=c(1,0,1))

#Time serie diagnostic
tsdiag(fit)

#Use the block-pierce test to evaluate the autocorrelation in our set of residuals( Values of bitcoin )

Box.test(fit$residuals,lag=1)

# 3 --> ARIMA model's Prediction

LH.pred <- predict(fit, n.ahead=30) #the next 30 days


lines(LH.pred$pred,col="red") # plot style
# predicted means following the standard error (+-)
lines(LH.pred$pred+2*LH.pred$se,col="red",lty=3)
lines(LH.pred$pred-2*LH.pred$se,col="red",lty=3)

LH.pred <- predict(fit, n.ahead=30)
LH.pred 
