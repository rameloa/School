install.packages("forecast") #-- do this only once 
install.packages("fpp") #-- do this only once 
#These packages are from the "Forecasting Principles and Practice" - excellent and free book: https://www.otexts.org/fpp
library(forecast)
library(fpp)

#============================================
# Intuitive Understandings of the ARIMA model 
#============================================

# First-order ARIMA 
?arima.sim

par(mfrow=c(2,2))

set.seed(123)
plot.ts( arima.sim( list(order = c(0, 0, 0) ), n = 100)   ) # White noise

set.seed(123)
plot.ts( arima.sim( list(order = c(1,0,0), ar = 0.9 ), n = 100)   ) # Autoregressive

set.seed(123)
plot.ts( arima.sim( list(order = c(1,0,1), ar=0.9, ma = 0.9 ), n = 100)   ) # Autoregressive with Moving average

set.seed(123)
plot.ts( arima.sim( list(order = c(1,1,1), ar=0.9, ma = 0.9 ), n = 100)   ) # Non-stationary Autoregressive with Moving average


# Effect of Coefficients
par(mfrow=c(2,2))

set.seed(123)
plot.ts( arima.sim( list(order = c(1,0,0), ar = 0.9 ), n = 100)   ) # Autoregressive with large positive coef.
set.seed(123)
plot.ts( arima.sim( list(order = c(1,0,0), ar = 0.2 ), n = 100)   ) # Autoregressive with small positive coef.
set.seed(123)
plot.ts( arima.sim( list(order = c(1,0,0), ar = -0.2 ), n = 100)   ) # Autoregressive with small negative coef.
set.seed(123)
plot.ts( arima.sim( list(order = c(1,0,0), ar = -0.9 ), n = 100)   ) # Autoregressive with large negative coef.



# Random walk 
par(mfrow=c(2,2))
set.seed(123)
plot.ts( arima.sim( list(order = c(1,0,0), ar = 0.999999 ), n = 10000)   )
set.seed(124)
plot.ts( arima.sim( list(order = c(1,0,0), ar = 0.999999 ), n = 10000)   )
set.seed(125)
plot.ts( arima.sim( list(order = c(1,0,0), ar = 0.999999 ), n = 10000)   )
set.seed(126)
plot.ts( arima.sim( list(order = c(1,0,0), ar = 0.999999), n = 10000)   )



# Autocorrelation Function (ACF) and partial Autocorrelation Function (PACF)
AR.ts<-arima.sim( list(ar = c(0.3, 0.5) ), n = 1000)
MA.ts<-arima.sim( list(ma = c(0.7, 0.8) ), n = 1000)
ARMA.ts<-arima.sim( list(ar = c(0.3, 0.5), ma = c(0.7, 0.8) ), n = 1000)

par(mfrow=c(1,2))
Acf(AR.ts,lag.max =25)
Pacf(AR.ts,lag.max =25) 

par(mfrow=c(1,2))
Acf(MA.ts,lag.max =25)
Pacf(MA.ts,lag.max =25) 

par(mfrow=c(1,2))
Acf(ARMA.ts,lag.max =25)
Pacf(ARMA.ts,lag.max =25) 


#============================================
# Forecasting New Covid-19 Cases in the US
#============================================


# Data Source: https://github.com/owid/covid-19-data/tree/master/public/data


#library (readr)
#vaccinationfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
#Covid19casesfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
#Variantfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/variants/covid-variants.csv"

#vaccination.data<-read_csv(url(vaccinationfile))
#Covid19cases.data<-read_csv(url(Covid19casesfile))
#Variant.data<-read_csv(url(Variantfile))

# install.packages("dplyr")
# Use the pipe %>% function in "dplyr" package
library(dplyr)

#US.vaccination.data = vaccination.data %>% filter(vaccination.data$iso_code == "USA")
US.cases.data = Covid19cases.data %>% filter(Covid19cases.data$iso_code == "USA")
#US.Variant.data = Variant.data %>% filter(Variant.data$location == "United States", Variant.data$variant == "Delta")

plot.ts(US.cases.data$new_cases)
plot.ts(US.cases.data$stringency_index)
#plot.ts(US.vaccination.data$total_vaccinations)
#plot.ts(US.vaccination.data$daily_vaccinations_raw)
#plot.ts(US.Variant.data$num_sequences)



# Preprocessing 

USNewcase<- US.cases.data$new_cases
USNewcase<-tail(USNewcase,-100) # Remove the first 100 days when the case numbers are small

USNewcase <- ts(USNewcase, frequency=365, start=c(2020, 122))

plot.ts(USNewcase, xlab="Date", ylab="US New cases")
# We notice two properties:
# (1)  the variance increases with mean (heteroscedasticity)
# (2)  the data has cyclic pattern(seasonality)  
# We cannot directly apply the ARIMA model when either of the above behaviors shows up! 

#-----------(1) Stabilizing the Variance---------------
# To stabilized the variance, we need to transform the original data using Box-Cox Transform, 
# also known as Power transform. It also makes the data more Gaussian   
USNewcase.lambda <- BoxCox.lambda(USNewcase)  # The transform is parameterized by lambda
# lambda is usually chosen between -2 and 2
USNewcase.BoxCox<-BoxCox(USNewcase, USNewcase.lambda)

# Check the transformed data and compare it with log transform
par(mfrow=c(1,2))
plot.ts(USNewcase.BoxCox, xlab="Date", ylab="US New cases")
plot.ts(log(USNewcase), xlab="Date", ylab="US New cases")
# if lambda is close to zero, BoxCox is essentially doing log transform.

# Transformedback<-InvBoxCox(USNewcase.BoxCox, USNewcase.lambda, biasadj = FALSE, fvar = NULL) # transform the forecasting back to original scale

logUSNewcase<-log(USNewcase) # We use log transform for simplicity 
plot.ts(logUSNewcase, xlab="Date", ylab="log US New cases")


#-----------(2) Remove Seasonality through Seasonal Differencing ---------------
# To check the period of cyclic pattern, use the autocorrelation function 
Acf(diff(logUSNewcase,1),lag.max =25) # We see spikes at p=7,14,21.. What does it suggest?  # Here we first perform regular differencing "diff(logUSNewcase,1)" to make the series more stationary, so its seasonality becomes easier to detect in the Acf plot

# We now remove the seasonality using seasonal differencing
logUSNewcase.deSeasonality <- diff(logUSNewcase,7) # period is 7 because of the weekly pattern 
plot.ts(logUSNewcase.deSeasonality, xlab="Date", ylab="log US New Case after removing trend and seasonality")
Acf(logUSNewcase.deSeasonality,lag.max =25) 

# What does "logUSNewcase.deSeasonality" mean? 

#-------------Check Stationarity -------------------
# Perform the augmented Dickey-Fuller (ADF) test to check stationarity. The null hypothesis assumes that the series is non-stationary.
adf.test(USNewcase,alternative = "stationary") # What? the test suggests stationarity? Now try the following
adf.test(logUSNewcase.deSeasonality,alternative = "stationary")

#-------------Automatic ARIMA Modeling -------------------
# To begin, we use an automated algorithm to find a good model. However, there is no guarantee that it is the best model. So we treat it as a starting point. 
model.auto <- auto.arima( logUSNewcase.deSeasonality, stepwise=FALSE, seasonal= FALSE) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
model.auto
# It suggests a ARIMA(4,0,1) model with zero mean
checkresiduals(model.auto)  # Check the quality of fit. Residuals should: 
# (1) not have any significant autocorrelation
# (2) follow normal distribution
# (3) have stable variance over time

# We can use the auto selected model to make forecasting 
fit.yourself <- Arima(logUSNewcase, order=c(4,0,1), seasonal=list(order=c(0,1,0),period=7)) # The seasonal differencing with period=7 is equivalent to "seasonal=list(order=c(0,1,0),period=7)"
fit.yourself
autoplot( forecast(fit.yourself,20) )

# Plot the forecasting in the original scale
fc<-forecast(fit.yourself,20)

fc$x <- exp(fc$x)
fc$mean <- exp(fc$mean)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
autoplot(fc)


#-------------Improving the Automatically selected Model -------------------
# Note that the auto.arima function may not always find the model with the lowest AIC/AICc/BIC. 
# To improve the existing model, we may explore other models manually to see if one could yield a lower criterion than the existing model. For example: 
fit.alternative1 <- Arima(logUSNewcase, order=c(4,0,2), seasonal=list(order=c(0,1,0),period=7)) 
fit.alternative1
checkresiduals(fit.alternative1)
fc1<-forecast(fit.yourself,20)

fc1$x <- exp(fc1$x)
fc1$mean <- exp(fc1$mean)
fc1$lower <- exp(fc1$lower)
fc1$upper <- exp(fc1$upper)
autoplot(fc1)

# You can also split the time series into a training set and a validation set, and then compare the MSE in the validation set to select the best model.  
# Another opportunity of improvement is carefully selecting the length of your training set. Using more recent data for training can leads to better forecasting if your historical data has a structural change that makes earlier data obsolete. 





model.raw <- auto.arima( USNewcase, stepwise=FALSE, seasonal= TRUE)
model.raw
fit.raw <- Arima(USNewcase, order=c(0,1,5)) 
autoplot( forecast(fit.raw,20) )

#============================================
# ARIMA with Covariates ("dynamic regression")
#============================================

# with insurance and advertising data (also part of FPP)
plot(insurance, main="Insurance advertising and quotations", xlab="Year")
View(insurance)

# Lagged predictors. Test 0, 1, 2 or 3 lags.
Advert <- cbind(insurance[,2],
                c(NA,insurance[1:39,2]),
                c(NA,NA,insurance[1:38,2]),
                c(NA,NA,NA,insurance[1:37,2]))
colnames(Advert) <- paste("AdLag",0:3,sep="")
Advert


# Choose optimal lag length for advertising based on AIC
# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1], d=0)
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2], d=0)
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3], d=0)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4], d=0)

# Compute Akaike Information Criteria
AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)


# Compute Bayesian Information Criteria
BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)

#Best fit (as per AIC and BIC) is with all data (1:2), so the final model becomes
fit <- auto.arima(insurance[,1], xreg=Advert[,1:2], d=0) # d is the order of first-differencing
fit

# forecast insurance quotes with advertising = 10
fc10 <- forecast(fit, xreg=cbind(rep(10,20),c(Advert[40,1],rep(10,19))), h=20)
plot(fc10, main="Forecast quotes with advertising set to 10", ylab="Quotes")

# see how forecasts with advertising = 8 will differ from advertising = 2
par(mfrow=c(1,2))
fc8 <- forecast(fit, xreg=cbind(rep(8,20),c(Advert[40,1],rep(8,19))), h=20)
plot(fc8, main="Forecast quotes with advertising set to 8", ylab="Quotes")

fc2 <- forecast(fit, xreg=cbind(rep(2,20),c(Advert[40,1],rep(2,19))), h=20)
plot(fc2, main="Forecast quotes with advertising set to 2", ylab="Quotes")

