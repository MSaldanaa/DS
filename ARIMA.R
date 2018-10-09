######### INITIALIZATION
# Install required packages
#install.packages("forecast")
#install.packages("graphics")

# Load the packages
library(forecast)
library(graphics)

# Load the data in R
Air <- AirPassengers
########################







######### DATA EXPLORATION
class(Air)# Check the class of data
start(Air) # Start date for the dataset
end(Air) # End date for the dataset
frequency(Air)# Frequency of months
summary(Air)# Summary of data
#################################




######### VIZUALIZATION
# Plotting the time series
plot(Air, ylab= "Passengers (1000's)")

# Divindng the data into Trend , Seasonal effect and random (noise) 
plot(decompose(Air)) 
#################################


######### ARIMA Model Identification and Forecasting
######### USING THE AUTO.ARIMA() FUNCTION 
######### Find appropriate values of p,d,q 
######### 1. AR Order 
######### 2. Differencing
######### 3. MA order 
best_pdq <- auto.arima(AirPassengers)
best_pdq


######### BUILD FORECAST MODEL
######### OPTION 1. USING ARIMA() FUNCTION
######### INSERT p,d,q VALUES INTO THE ARIMA() FUNCTION
fit <- arima(AirPassengers, order=c(0, 1, 1), list(order=c(0, 1, 0), period = 12))
######### OPTION 2. USE THE AUTO.ARIMA() FUNCTION
fit <- auto.arima(AirPassengers,trace = TRUE)

######### FORECAST FOR NEXT 30 DAYS
forecast <- predict(fit, n.ahead=30)
######### # calculate upper (U) and lower (L) prediction intervals
U <- forecast$pred + 2*forecast$se # se: standard error (quantile is 2 as mean=0)
L <- forecast$pred - 2*forecast$se
# plot observed and predicted values
ts.plot(AirPassengers, forecast$pred, U, L, col=c(1, 2, 4, 4), lty=c(1, 1, 2, 2))
legend("topleft", c("Actual", "Forecast", "Upper & Lower Bounds"), col=c(1, 2, 4),lty=c(1, 1, 2))
