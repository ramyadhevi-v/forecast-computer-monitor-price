# Forecast Computer Monitor Price

setwd("D:/Business Analytics") 

options(digits = 3, scipen = 999)

library(tidyverse)
library(pdfetch)
library(xts)
library(forecast)
library(ggplot2)
library(forecastHybrid)
library(gtrendsR)

remove(list =  ls())

#=====================================================================
# Forecast No. 1 (Use official data)
#
# Download data from FRED on the Consumer Price Index for all Urban Consumers: Personal Computers and peripheral equipment.
#
# Run the following 6-month forecast, (i) an ETS; (ii) an ARIMA; and (iii) ensemble forecast.
# Pick the best forecast based on RMSE.
# Display a ggplot graph of the chosen model forecast.
#=====================================================================

# Download data from FRED
cpi_data = pdfetch_FRED("CUSR0000SEEE01")
head(cpi_data)
tail(cpi_data)

# ETS - expotential times series
fit_ets = ets(cpi_data)
cpi_ets_ff = forecast(fit_ets, h = 6)
autoplot(cpi_ets_ff, main = "ETS Six Month Forecast\nCPI - Urban Consumers - PCs and Peripherals")
accuracy(cpi_ets_ff)
# RMSE is 0.683

# ARIMA - autoregressive integrated moving average
fit_arima = arima(cpi_data)
cpi_arima_ff = forecast(fit_arima, h = 6)
autoplot(cpi_arima_ff, main = "ARIMA Six Month Forecast\nCPI - Urban Consumers - PCs and Peripherals")
accuracy(cpi_arima_ff)
# RMSE is 30.9

# AUTO ARIMA - autoregressive integrated moving average
fit_auto = auto.arima(cpi_data)
cpi_auto_ff = forecast(fit_auto, h = 6)
autoplot(cpi_auto_ff, main = "ARIMA Six Month Forecast\nCPI - Urban Consumers - PCs and Peripherals")
accuracy(cpi_auto_ff)
# RMSE is 0.665

# Ensemble Forecast
fit_hybrid <- hybridModel(cpi_data, weights = "equal", errorMethod = "RMSE")
plot(fit_hybrid, type = "fit")
cpi_hybrid_ff <- forecast(fit_hybrid, h = 6)
cpi_hybrid_ff
autoplot(cpi_hybrid_ff, main = "Ensemble Six Month Forecast\nCPI - Urban Consumers - PCs and Peripherals")
accuracy(cpi_hybrid_ff)
# RMSE is 0.65

# Ensemble Forecast provides the lowest RMSE value, therefore it provides the best forecast

# GGPLOT of Ensemble Forecast
# note: time (monthly) against "Index Dec 2007=100, Seasonally Adjusted"


autoplot(cpi_hybrid_ff) +
  xlab("Time") + ylab("CPI") + theme_bw()+
  theme(legend.title =
          element_blank(), legend.position="bottom",
        legend.text=element_text(size=12))+
  ggtitle("Computers & Peripherals")




#=====================================================================
# Forecast No. 2 (Generate your own data)
#
# Using the R-package "ggtrendsR" download data on "Computer Monitor Prices." The R-package
# ggtrendsR obtains Google Trends data - which is an index of the volume of google searches.
#
# Perhaps we want to find other search terms more apropos my inquiry instead of searching
# "Computer Monitor Prices." Feel free choose any other search term that might give we insights
# into the computer monitor marketplace.
#
# Once we have the search data. Run the following 6-month forecast. (i) an ETS; (ii) an ARIMA;
# and (iii) ensemble forecast. Pick the best forecast based on RMSE. Display a ggplot graph
 # of the chosen model forecast.
#=====================================================================

# search terms, search engines, region

bob <- gtrends(c("Computer Monitor Sale"), time = "today+5-y",
                      gprop = c("web", "news", "images", "froogle", "youtube"), geo = c("US"))
# results
bob

# examine the objects created by gtrends()
names(bob)

# select "interest over time"
time_trend = bob$interest_over_time
time_trend

# graph the search volume data; specifically "hits"
ggplot(data=time_trend, aes(x=date, y=hits,group=keyword,col=keyword))+
  geom_line()+xlab('Time')+ylab('Relative Interest')+ theme_bw()+
  theme(legend.title = element_blank(),legend.position="bottom",
        legend.text=element_text(size=12))+
  ggtitle("Google Search Volume")

# In preparation for forcasting you will have to do something about the
# fact that hits is a character vector
# in the data frame time_trend; and has data "<1". which means less than 100 searches in Google
head(time_trend)
tail(time_trend)
str(time_trend)


time_trend_bob = time_trend %>% filter(keyword == "Computer Monitor Sale")

# Convert the "<1" to "0"; they are still characters
time_trend_bob2 = str_replace(time_trend_bob$hits, "<1", "0")
time_trend_bob2

# Convert to numeric
time_trend_num = as.numeric(time_trend_bob2)


# Fit an ETS model to the data
model2_ets = ets(time_trend_num)
model2_ets_f = forecast(model2_ets, h = 6)
autoplot(model2_ets_f) + ggtitle("Google Search Volume Computer Monitor Sale\nETS Forecast") +
  labs( y = "Search Volume")
accuracy(model2_ets_f)
# RMSE is 12.7

# Fit an ARIMA model to the data
model2_arima = arima(time_trend_num)
model2_arima_f = forecast(model2_arima, h = 6)
autoplot(model2_arima_f) + ggtitle("Google Search Volume Computer Monitor Sale\nARIMA Forecast") +
  labs( y = "Search Volume")
accuracy(model2_arima_f)
# RMSE is 13

# Fit an AUTO ARIMA model to the data
model2_auto = auto.arima(time_trend_num)
model2_auto_f = forecast(model2_auto, h = 6)
autoplot(model2_auto_f) + ggtitle("Google Search Volume Computer Monitor Sale\nAUTO ARIMA Forecast") +
  labs( y = "Search Volume")
accuracy(model2_auto_f)
# RMSE is 12.6

# Ensemble Forecast
model2_hybrid <- hybridModel(time_trend_num, weights = "equal", errorMethod = "RMSE")
plot(model2_hybrid, type = "fit")
model2_hybrid_ff <- forecast(model2_hybrid, h = 6)
model2_hybrid_ff
autoplot(model2_hybrid_ff, main = "Google Search Volume Computer Monitor Sale\nENSEMBLE Forecast")
accuracy(model2_hybrid_ff)
# RMSE is 12

autoplot(model2_hybrid_ff) +
  xlab("Time") + ylab("Computer Monitor Sale") + theme_bw()+
  theme(legend.title =
          element_blank(), legend.position="bottom",
        legend.text=element_text(size=12))+
  ggtitle("Google Search Volume Computer Monitor Sale")

