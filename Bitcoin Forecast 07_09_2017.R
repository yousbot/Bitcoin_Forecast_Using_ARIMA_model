# 1 ) Get the data 
  # -> Go to bitcoincharts.com and get all the data 
  # copy and paste it to an xlsx file, i've already done it 

# 2 ) Read data

library(openxlsx)
library(zoom)
library(forecast)
da <- read.xlsx("C:/Users/HP/Desktop/Bitcoin Forecast/Full_Bitcoin_Data.xlsx", colNames = TRUE)

# 3 ) Clean data 

da$Timestamp <- as.Date.numeric(da$Timestamp, origin = "13/09/2011")

  # -> Because if you check the data on your xlsx, you'll find out values Inf, or NA, or indefined ! SO we just normalize all the data 

da$Open <- as.numeric(as.character(da$Open))
da$Open[which(is.nan(da$Open))] = NA
da$Open[which(da$Open==Inf)] = NA
da <- da[!(is.na(da$Open) | da$Open==""),]

  # -> Reapeat the same procedure to all the other cols

da$High <- as.numeric(as.character(da$High))
da$High[which(is.nan(da$High))] = NA
da$High[which(da$High==Inf)] = NA
da <- da[!(is.na(da$High) | da$High==""),]

  
da$Low <- as.numeric(as.character(da$Low))
da$Low[which(is.nan(da$Low))] = NA
da$Low[which(da$Low==Inf)] = NA
da <- da[!(is.na(da$Low) | da$Low==""),]


da$Close <- as.numeric(as.character(da$Close))
da$Close[which(is.nan(da$Close))] = NA
da$Close[which(da$Close==Inf)] = NA
da <- da[!(is.na(da$Close) | da$Close==""),]

da$`Volume.(BTC)` <- as.numeric(as.character(da$`Volume.(BTC)`))
da$`Volume.(BTC)`[which(is.nan(da$`Volume.(BTC)`))] = NA
da$`Volume.(BTC)`[which(da$`Volume.(BTC)`==Inf)] = NA
da <- da[!(is.na(da$`Volume.(BTC)`) | da$`Volume.(BTC)`==""),]



da$`Volume.(Currency)` <- as.numeric(as.character(da$`Volume.(Currency)`))
da$`Volume.(Currency)`[which(is.nan(da$`Volume.(Currency)`))] = NA
da$`Volume.(Currency)`[which(da$`Volume.(Currency)`==Inf)] = NA
da <- da[!(is.na(da$`Volume.(Currency)`) | da$`Volume.(Currency)`==""),]


da$Weighted.Price <- as.numeric(as.character(da$Weighted.Price))
da$Weighted.Price[which(is.nan(da$Weighted.Price))] = NA
da$Weighted.Price[which(da$Weighted.Price==Inf)] = NA
da <- da[!(is.na(da$Weighted.Price) | da$Weighted.Price==""),]


# 4 ) Structure your data into a Time Serie object

dac <- da # for BU

dac.ts <- ts(dac[,5],start=c(2011,9), freq=360)
  # Explanation : 
  # dac[,5] ---> means we get only the CLose column, which is the fifth
  # Start=c(2011,9) ---> we're starting from sept 2011
  # freq = 360 ---> From sept to sept, it's just not 7 years !! It need s more explanation

plot(dac.ts)

inout.zoom()

data <- data.frame(closing=dac.ts, lclosing=log(dac.ts))

plot(data$closing)

data.stl = stl(data$closing, s.window = "periodic")

plot(data.stl)

# 5 ) Forecasting using the arima model 

data.forecast = forecast(data.stl, method="arima", h=24, level=95)

plot(data.forecast, ylab="Bitcoin Price", xlab="Years")

head(data.forecast)


# Thank you !! 















