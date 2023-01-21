## clear the console 

rm(list = ls()) 

## import libraries
 
library(AER)
library(readxl)
library(dynlm)
library(vars)
library(quantmod)
library(scales)
library(fGarch)
library(tidyverse)
library(haven)
library(tseries)
library(ggplot2) 

##import data

## set wd 

setwd("/Users/jackreid/documents/uni/forecasting6003/assignment 2")


## read data into r 

data <- read_xlsx("dataA2.xlsx")


## convert to time series 


hourwk <- ts(data$hourwk, start = c(1959, 1), end = c(2017, 4), frequency = 4)

rgdp <- ts(data$rgdp, start = c(1959, 1),  end = c(2017, 4), frequency = 4)

gdpdf <- ts(data$gdpdf, start = c(1959, 1),  end = c(2017, 4),  frequency = 4)

## view 

head(data)

tail(data)
# Q1
## plot data 

plot(hourwk)
plot(rgdp)
plot(gdpdf)

#Q2 ADF test

##check stationarity 
adf.test(hourwk) 
adf.test(rgdp) 
adf.test(gdpdf)

## apply johansen's test 

library("urca")
johansen=ca.jo(data.frame(gdpdf,hourwk,rgdp), type="trace", K=2, ecdet="none", spec="longrun")
summary(johansen)

 

#Q3 

#transform variables 


## 2nd diff of log gdpdf

log_gdpdf <- log(gdpdf)
log_gdpdf_d1 <- diff(log_gdpdf)
log_gdpdf_d2 <- diff(log_gdpdf_d1)

log_gdpdf_d2 

## convert to growth rates

rgdp_d1 <- diff(rgdp)

na.omit(rgdp_d1)

hourwk_d1 <- diff(hourwk)


na.omit(hourwk_d1)







rgdp_d1 <- window(rgdp_d1, start = c(1959, 3), )

rgdp_d1


hourwk_d1 <- window(hourwk_d1, start = c(1959, 3), )  


hourwk_d1 

## check new variables for stationarity 

adf.test(log_gdpdf_d2) 
adf.test(rgdp_d1) 
adf.test(hourwk_d1) 



#redo johansen's test 

johansen_test2 = ca.jo(data.frame(log_gdpdf_d2,hourwk_d1,rgdp_d1), type="trace", K=2, ecdet="none", spec="longrun")
summary(johansen_test2)

## this time reject the null of no cointegration, and of r<1 and r<2, at all significance level. 
##implying rank of matrix is 3, we need a linear combination of all three series to form a stationary series

# Q4 

#impose restrictions and 

VAR_data <- window(ts.union(log_gdpdf_d2, hourwk_d1, rgdp_d1), start = c(1959, 3), end = c(2017, 4))

# select lags for VAR model 

data<-na.omit(data)

VARselect(VAR_data, lag.max=8,type="const")[["selection"]]

## bic suggests 1 lag 


var1 <- VAR(VAR_data, p=1, type="const")

summary(var1) 



## add restrictions: 
    ### log_gdpdf_d2 not impactd by rgdp_d1 
    #### hourwk_d1 not impacted by log_gdpdf_d2 or rgdp_d1 


restrict <- matrix(c(1, 1, 0, 1,
                     0, 1, 0, 1, 
                     1, 1, 1, 1),
                   nrow=3, ncol=4, byrow=TRUE) 
var2 <- restrict(var1, method = "man", resmat = restrict)

summary(var2)  
coef(var2) 


#Q5 

## impulse response function 

irf_gdp_hourwk <- irf(var2, impulse = "hourwk_d1", response = "rgdp_d1",
n.ahead = 10, ortho = TRUE, runs = 100)  
irf_gdp_hourwk
plot(irf_gdp_hourwk)

