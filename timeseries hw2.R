setwd("C:/Users/14lmu/Downloads")
install.packages("gdata") 
library(gdata)
library(dplyr)
install.packages("stringr")
library(stringr) 
library(ggplot2)

well_data<-read.csv("G-2866_T.csv")%>%
  rename(date=ï..date)%>%
  mutate(date = as.character(date))%>%
  select(-Well_ft, -Code)%>%
  group_by(time)%>%
  mutate(new_time=as.character(time),
         hour=str_split(new_time,":")[[1]][1],
         minute=str_split(new_time,":")[[1]][2])%>%
  ungroup(time)%>%
  group_by(date)%>%
  mutate(month=str_split(date,"/")[[1]][1],
         day=str_split(date,"/")[[1]][2],
         year=str_split(date,"/")[[1]][3])%>%
  ungroup(date)%>%
  mutate(minute=as.integer(minute),hour=as.integer(hour),day=as.integer(day),
         month=as.integer(month),year=as.integer(year))%>%
  filter(!(year==2018&month==6&day>8)&
           !(year==2018&month==6&day==8&hour>10)&
           !(year==2018&month==6&day==8&hour==9&minute>30))%>%
  arrange(year,month,day,hour)%>%
  group_by(year,month,day, hour, tz_cd)%>%
  summarise(Corrected_mean=mean(Corrected))%>%
  ungroup(year,month,day,hour)

new_well_data<-well_data%>%
  group_by(year,month)%>%
  summarize(month_mean=mean(Corrected_mean),
            numb=n())%>%
  arrange(year,month)

#Create a training and test split, where last 6 months is the test set (all of 2018) 

training <- new_well_data %>% filter(year!=2018)
test <- new_well_data %>% filter(year==2018)

#create a time series object - decomposition 
well <- ts(training$month_mean, start = c(2007, 10), frequency = 12)  
well1 <- ts(test$month_mean, start = 2018, frequency = 12 )

# Need to perform classic decomposition vs. STL decomposition

# Classic Decomp additive
Classic_Add_Decomp <- decompose(well,type="additive")
autoplot(Classic_Add_Decomp) + xlab("Year") + ggtitle("Classic Additive Decomposition of Well Water Level")

# Classic Decomp Multiplicative
Classic_Multi_Decomp <- decompose(well,type="multiplicative")
autoplot(Classic_Multi_Decomp) + xlab("Year") + ggtitle("Classic Multiplicative Decomposition of Well Water Level")

#why use STL? allows changing effects for trend and season, adapted to handle outliers
decomp_stl <- stl(well, s.window = 7)
plot(decomp_stl)
seas=well-decomp_stl$time.series[,1]

#plot of well values with trend/cycle 
plot(well, col = "grey", main = "Well Depth - Trend/Cycle", xlab = "Year", ylab = "Well Depth (in feet)", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2) 

#plot of well values overlaid with the seasonally adjusted water values for the training set 
plot(well, col = "grey", main = "Well Depth - Seasonally Adjusted", xlab = "Year", ylab = "Well Depth (in feet)", lwd = 2)
lines(seas, col = "red", lwd = 2)

#Create ESM models - hypothesis is winters additive, but try all  

#Single Exponential Smoothing Model 
SES.well <- ses(well, initial = "optimal", h = 6)
summary(SES.well)

#plot predicted versus actual for test data 
plot(SES.well$mean,test$month_mean, main = "Predicted vs Actual Well Depth (in feet)", xlab = "Predicted Well Depth", ylab = "Actual Well Depth")

#report the MAPE for test data, along with other values
error1=test$month_mean-SES.well$mean
MAPE1=mean(abs(error1)/abs(test$month_mean))
MAE1=mean(abs(error1))

#Linear Exponential Smoothing Model 
LES.well <- holt(well, initial = "optimal", h = 6)
summary(LES.well)

#plot predicted versus actual for test data 
plot(LES.well$mean,test$month_mean, main = "Predicted vs Actual Well Depth (in feet)", xlab = "Predicted Well Depth", ylab = "Actual Well Depth")

#report the MAPE for test data, along with other values
error2=test$month_mean-LES.well$mean
MAPE2=mean(abs(error2)/abs(test$month_mean))
MAE2=mean(abs(error2))

# Holt-Winters ESM - Additive 
HWES.add <- hw(well, seasonal = "additive", h=6)
summary(HWES.add)

#plot predicted versus actual for test data 
plot(HWES.add$mean,test$month_mean, main = "Predicted vs Actual Well Depth (in feet)", xlab = "Predicted Well Depth", ylab = "Actual Well Depth")

#report the MAPE for test data, along with other values
error3=test$month_mean-HWES.add$mean
MAPE3=mean(abs(error3)/abs(test$month_mean)) 
MAE3=mean(abs(error3))

# Holt-Winters ESM - Multiplicative 
HWES.mult <- hw(well, seasonal = "multiplicative", h=6)
summary(HWES.mult)
test.results <- forecast(HWES.mult)

#plot predicted versus actual for test data 
plot(HWES.mult$mean,test$month_mean, main = "Predicted vs Actual Well Depth (in feet)", xlab = "Predicted Well Depth", ylab = "Actual Well Depth")

#report the MAPE for test data, along with other values
error4=test$month_mean-HWES.mult$mean
MAPE4=mean(abs(error4)/abs(test$month_mean))
MAE4=mean(abs(error4)) 

# Holt Winters multiplicative is the best, so plot next 

plot(test.results, xlab = "Year", ylab = "Water Level (feet)")
lines(well1, main = "Well Water Level Holt-Winters multiplicative ESM Forecast", col = "red")
legend(x = "topleft", legend = c("Predicted", "Test Data"), col = c("blue", "red"), lty = c(1, 1))


#creating plots for the report: 

#Single exponential smoothing plot
plot(SES.well, xlab = "Year", ylab = "Water Level (feet)", main = "Well Water Depth with Single ESM Forecast")
lines(well1, col = "red")
legend(x = "topleft", legend = c("Predicted", "Test Data"), col = c("blue", "red"), lty = c(1, 1))

#linear ESM plot
plot(LES.well, xlab = "Year", ylab = "Water Level (feet)", main = "Well Water Depth with Linear ESM Forecast")
lines(well1, col = "red")
legend(x = "topleft", legend = c("Predicted", "Test Data"), col = c("blue", "red"), lty = c(1, 1))

#additive holt winters plot
plot(HWES.add, xlab = "Year", ylab = "Water Level (feet)", main = "Well Water Depth with Additive Holt-Winters ESM Forecast")
lines(well1, col = "red")
legend(x = "topleft", legend = c("Predicted", "Test Data"), col = c("blue", "red"), lty = c(1, 1))

#multiplicative holt winters plot
plot(HWES.mult, xlab = "Year", ylab = "Water Level (feet)", main = "Well Water Depth with Multiplicative Holt-Winters ESM Forecast")
lines(well1, col = "red")
legend(x = "topleft", legend = c("Predicted", "Test Data"), col = c("blue", "red"), lty = c(1, 1))






