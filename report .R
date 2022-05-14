## Read in project data file
filename = "projectdata.txt"
project_data = read.table(filename, header=FALSE)
## Extract the data according to your reference number
y = project_data[,1]
y
#Check the length
length(y)
# Create the data matrix
years = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
year = rep(years, 1, each = 12)
month = rep(1:12, 10)
df = data.frame(year,month,y)
ddf = as.matrix(df)
# Now lets convert the data to a ts object
data = ts(ddf[,3], start = ddf[1,1:2], end = ddf[nrow(ddf), 1:2], frequency = 12)
plot(data, type = "l", xlab = "Time", ylab = "Sales", main = "Product sales from 2011-2020")

# From the plot we see that there is an evident seasonal effect 
# We also observe an upwards liner trend 
# and the process does not appear to be stationary so we cannot use models for stationary 
# processes 

# ARIMA
# So it may be appropriate to fit a seasonal arima model

# Now what we need to do first is choose values for p, q and d 
# Let's plot the acf 
acf(data, lag.max = 40) 
pacf(data, lag.max = 40)
# The acf appears to decay to zero but not very fast, which confirms non stationarity. We 
# observe some spikes at lag 1 (12 months) lag two (24 months) etc. So we start by computing
# the seasonal deifferences 
par(mfrow = c(1,3))
data_seas = diff(data, lag = 12)
plot(data_seas, type = "l", xlab = "Time", ylab = "Seasonaly differenced data", main = "Product sales from 2011-2020")
acf(data_seas, lag.max = 100)
pacf(data_seas, lag.max = 100)
# Again we observe clear evidence of non stationarity, and the linear trend is still present 
# From the acf we see that it decays very slowly to zero, giving more evidence of non
# stationarity 
# So now we can try another differencing but this time non seasonal
data_diff = diff(data_seas)
plot(data_diff, type = "l", xlab = "Time", ylab = "Non-seasonaly differenced data", main = "Product sales from 2011-2020")
acf(data_diff, lag.max = 100)
pacf(data_diff, lag.max = 100)
# The time series now look much more stationary. 
# Looking at the non seasonal seasonal acfs we see that q = 1 and p = 0
# Now for the seasonal acfs we see that q star  = 1 and p star = 0

# So we can now fit the (0,1,1)x(0,1,1) arima model:

mod = arima(data, order = c(0,1,1), seasonal = c(0,1,1))
mod

par(mfrow = c(1,2))

# Let's examine the residuals now 
resids = mod$residuals
# plot residuals
plot(resids, ylab = "Residuals")
# Plot acf and pacf
acf(resids)
pacf(resids)

# In general things look OK. However some acfs and pacfs seem to be a bit different 
# than zero 

# we can try overfiting, and increase p by 1 so we have:
mod1 = arima(data, order = c(1,1,1), seasonal = c(0,1,1))
mod1

# The change in likelihood is: 
loglik1 = 2*(mod1$loglik-mod$loglik)
1 - pchisq(loglik1, 1)

# the p value is  not significant so we accept the null hypothesis and conclude that the 
#  ar parameter is not needed in the model 


# Now we try to increase p star by 1:
mod2 = arima(data, order = c(0,1,1), seasonal = c(1,1,1))
mod2

loglik2 = 2*(mod2$loglik-mod$loglik)
1 - pchisq(loglik2, 1)

# The p value is not significant so we reject the null hypothesis.The extra ar term is not
# needed

# Now we increase q by 1:

mod3 = arima(data, order = c(0,1,2), seasonal = c(0,1,1))
mod3

loglik3 = 2*(mod3$loglik-mod$loglik)
1 - pchisq(loglik3, 1)

# The p value is not significant so the extra ma parameter is not needed 

# Finally we increase q star by 1: 
mod4 = arima(data, order = c(0,1,1), seasonal = c(0,1,2))
mod4

loglik4 = 2*(mod4$loglik-mod$loglik)
1 - pchisq(loglik4, 1)

# the p value is not significant 


# Underfitting 
# we reduce q by 1
mod5 = arima(data, order = c(0,1,0), seasonal = c(0,1,1))
mod5

loglik5 = 2*(mod$loglik-mod5$loglik)
1 - pchisq(loglik5, 1)
# We reject the the null hypothesis of the simpler model 
# Now we reduce q star by 1
mod6 = arima(data, order = c(0,1,1), seasonal = c(0,1,0))
mod6

loglik6 = 2*(mod$loglik-mod6$loglik)
1 - pchisq(loglik6, 1)
# again reject the null hypothesis of a simpler model 

# So our final model is an arima (0,1,1)x(0,1,1)
# Now we fit again the residuals 
final_mod = arima(data, order = c(0,1,1), seasonal = c(0,1,1))
final_mod

## Generate forecasts
forecast = predict(final_mod, n.ahead=6)
## Extract point forecasts and prediction interval limits
preds = forecast$pred
lower = preds - 1.96 * forecast$se
upper = preds + 1.96 * forecast$se

## Merge data and forecasts
data_f = ts(c(data, upper), start=start(data),
              frequency=frequency(data))
preds_f = ts(c(data[length(data)], preds), start=end(data),
             frequency=frequency(data))
## Plot data in black and forecasts in red
plot(data_f, ylab="Product sales", type="n")
lines(data, col="black")
lines(preds_f, col="red")
## Add 95% prediction intervals
lines(lower, lty=2, col="red")
lines(upper, lty=2, col="red")
legend("topleft", c("data", "forecasts", "95% prediction intervals"),
       col=c("black", "red", "red"), lty=c(1, 1, 2))

# We see that the intervals are quite narrow for the 6 months forecast and as expected the 
# sales seem to follow a similar trend to the sales up to that point. Since the prediction
# is short term, the sarima model seems appropriate. However we need to check the predictive
#performance of the model. To do that we fit the model to the first nine years:

data_nine = ts(ddf[,3], start = ddf[1,1:2], end = ddf[108, 1:2], frequency = 12)

#Now fit our model to this data:
data_nine_mod = arima(data_nine,  order = c(0,1,1), seasonal = c(0,1,1))
forecast_nine = predict(data_nine_mod, n.ahead=12)
forecast_data = forecast_nine$pred
actual_data = tail(data,12)

# So now we can find the MSE 
mse_nine = mean((actual_data - forecast_data)^2)
mse_nine
# In general it seems that the predictions are quite close to the actual values
# Now we can plot the actual and the forecast data 
## Extract point forecasts and prediction interval limits
preds_nine = forecast_nine$pred
lower_nine = preds_nine - 1.96 * forecast_nine$se
upper_nine = preds_nine + 1.96 * forecast_nine$se

## Merge data and forecasts
data_f_nine = ts(c(data_nine, actual_data), start=start(data_nine[]),
            frequency=frequency(data_nine))
preds_f_nine = ts(c(data_nine[length(data_nine)], preds_nine), start=end(data_nine),
             frequency=frequency(data_nine))
lower_nine_ts = ts(c(data[108],lower_nine), start = end(data_nine), frequency = frequency(data))
upper_nine_ts = ts(c(data[108],upper_nine), start = end(data_nine), frequency = frequency(data))

## Plot data in black and forecasts in red
plot(data_f_nine, ylab="Product sales", type="n")
lines(data_f_nine, col="black")
lines(preds_f_nine, col="red")
## Add 95% prediction intervals
lines(lower_nine_ts, lty=2, col="red")
lines(upper_nine_ts, lty=2, col="red")

legend("topleft", c("data ", "forecasts for tenth year", "95% prediction intervals"),
       col=c("black", "red", "red"), lty=c(1, 1, 2))
# In general we see that the prediction are very close to the actual observed values. We 
# also observe that the upper interval of the predictions is closer to the actual values

# TIME SERIES REGRESSION MODEL
# Now fit a regression model: 
# We first need to create the data frame 
months = rep(factor(month.abb, levels = month.abb), length.out = length(y))
y_df = data.frame(Sales = y, t = 1:length(y), month = months)
y_df

# Fit the model 
mean_model = lm(Sales ~ ., data = y_df)
fits = mean_model$fitted.values
resids_mean = mean_model$residuals

summary(mean_model)

par(mfrow = c(1,2))
## Convert fits and resids to ts objects:
fits = ts(fits, start=start(data), end=end(data), frequency=frequency(data))
resids = ts(resids_mean, start=start(data), end=end(data), frequency=frequency(data))
## Plot the data and overlay the fitted values
plot(data, ylab="Product Sales", type = "l")
lines(fits, col="red")
legend("topleft", c("data", "fitted values"), col=c("black", "red"), lty=1)
## Plot the residuals
plot(resids, ylab="Residuals")

# We observe that in general the fitted values are quite close to the actual values.
# However we see that in some points there is some deviation 

# The residuals seem to be autocorrelated and not independent
acf(resids)
pacf(resids)

# The acf seems to gradually tail of to 0, and pacf seems to cut off after lag 1. So we can
# fit an AR(1) model as follows: 
mod_res = arima(resids, order = c(1,0,0), include.mean = FALSE)
mod_res
# Now lets examine the residuals on this fit 
plot(mod_res$residuals, ylab = "Residuals")
acf(mod_res$residuals)
pacf(mod_res$residuals)
# Now from these plots we do not observe any obvious problem 

# Next we try overfitting 
mod_res_over = arima(resids, order = c(2,0,0), include.mean = FALSE)
mod_res_over

loglik_mean = 2*(mod_res_over$loglik - mod_res$loglik)
1 - pchisq(loglik_mean,1)
# The p value is very significant so we add this parameter. Now we overfit again: 
mod_res_over1 = arima(resids, order = c(3,0,0), include.mean = FALSE)
mod_res_over1

loglik_mean1 = 2*(mod_res_over1$loglik - mod_res_over$loglik)
1 - pchisq(loglik_mean1,1)
# Now the p value is not significant so we do not add the third ar parameter

# Also we can try an arma(2,1):
mod_res_arma = arima(resids, order = c(2,0,1), include.mean = FALSE)
mod_res_arma

loglik_mean_arma = 2*(mod_res_arma$loglik - mod_res_over$loglik)
1 - pchisq(loglik_mean_arma,1)
# The p value is not significant so the ma term is nit needed. 
# So our final model for the residuals is: 
mod_res_over = arima(resids, order = c(2,0,0), include.mean = FALSE)
# We can plot the acf and pacf again:
plot(mod_res_over$residuals, ylab = "Residuals")
acf(mod_res_over$residuals)
pacf(mod_res_over$residuals)



# Now we can generate forecasts 
## Construct predictors over the forecast period
new_month = rep(factor(month.abb, levels=month.abb), 1)
new_y_data = data.frame(t=121:126, month=new_month[1:6])
## Perform prediction
forecast = predict(mean_model, new_y_data, interval="prediction")
## Extract forecasts and prediction interval limits
preds_naive = ts(forecast[,1], frequency=12, start=c(2021, 1))
lower_naive = ts(forecast[,2], frequency=12, start=c(2021, 1))
upper_naive = ts(forecast[,3], frequency=12, start=c(2021, 1))

## Forecast residuals from fitted ARMA model
forecast_resids = predict(mod_res_over, n.ahead=6)
## Extract point forecasts and forecast variance for residuals
preds_resids = forecast_resids$pred
predvar_resids = forecast_resids$se^2
## Obtain variance used in construction of confidence interval
confvar = predict(mean_model, new_y_data, interval="confidence", se.fit=TRUE)$se.fit^2
## Construct overall forecasts
preds = preds_naive + preds_resids
## Construct overall prediction intervals
predvar = confvar + predvar_resids
lower = preds - 1.96 * sqrt(predvar)
upper = preds + 1.96 * sqrt(predvar)


## Merge data and forecasts
y_f = ts(c(data, upper_naive), start=start(data), frequency=frequency(data))
preds_naive_f = ts(c(data[length(data)], preds_naive), start=end(data),
                   frequency=frequency(data))
lower_naive_f= ts(c(data[length(data)],lower_naive), start = end(data), frequency = frequency(data))
upper_naive_f= ts(c(data[length(data)],upper_naive), start = end(data), frequency = frequency(data))
preds_f = ts(c(data[length(data)], preds), start=end(data), frequency=frequency(data))

## Plot data in black and forecasts in red and blue
plot(y_f, ylab="Product Sales", type="n")
lines(data, col="black")
lines(preds_naive_f, col="blue")
lines(preds_f, col="red")
lines(lower_naive, lty=2, col="blue")
lines(upper_naive, lty=2, col="blue")
lines(lower, lty=2, col="red")
lines(upper, lty=2, col="red")
legend("topleft", c("data", "forecasts", "naive forecasts", "95% prediction intervals",
                    "naive 95% prediction intervals"), col=c("black", "red", "blue", "red", "blue"),
       lty=c(1, 1, 1, 1, 2, 2))


# Now we can check the performance of the model by fitting it to the first 9 years 
y_df_nine = y_df[1:108,]
y_df_nine

mean_model_nine = lm(Sales ~., data = y_df_nine)
resids_nine = mean_model_nine$residuals
# Transform them into time series 
resids_nine_ts = ts(resids_nine, start=start(data_nine), end=end(data_nine), frequency=frequency(data_nine))

## Perform prediction
new_month_ten = rep(factor(month.abb, levels=month.abb), 1)
new_month_ten_data = data.frame(t=109:120, month=new_month)
forecast_nine = predict(mean_model_nine, new_month_ten_data, interval="prediction")

# First we need the naive predictions
preds_naive_nine = ts(forecast_nine[,1], frequency=12, start=c(2020, 1))
lower_naive = ts(forecast_nine[,2], frequency=12, start=c(2020, 1))
upper_naive = ts(forecast_nine[,3], frequency=12, start=c(2020, 1))

## Forecast residuals from fitted ARMA model
# First fit the arma model to the data up to 2019
mod_res_nine = arima(resids_nine_ts, order = c(2,0,0), include.mean = FALSE)
# Now find the predictions 
forecast_resids_nine = predict(mod_res_nine, n.ahead=12)
## Extract point forecasts and forecast variance for residuals
preds_resids_nine = forecast_resids_nine$pred
predvar_resids_nine = forecast_resids_nine$se^2
## Obtain variance used in construction of confidence interval
confvar_nine = predict(mean_model_nine, y_df[109:120,], interval="confidence", se.fit=TRUE)$se.fit^2
## Construct overall forecasts
preds_nine = preds_naive_nine + preds_resids_nine
## Construct overall prediction intervals
predvar_nine = confvar_nine + predvar_resids_nine
lower_nine = preds_nine - 1.96 * sqrt(predvar_nine)
upper_nine = preds_nine + 1.96 * sqrt(predvar_nine)

mse_mean_mod = mean((actual_data - preds_nine)^2)
mse_mean_mod

# Now we can plot the actual and predicted data 
## Merge data and forecasts
data_f_mean_nine = ts(c(data_nine, actual_data), start=start(data_nine[]),
                 frequency=frequency(data_nine))
preds_f_mean_nine = ts(c(data_nine[length(data_nine)], preds_nine), start=end(data_nine),
                  frequency=frequency(data_nine))


## Plot data in black and forecasts in red and blue
plot(data_f_mean_nine, ylab="Product Sales", type="n")
lines(data_f_mean_nine, col="black")
lines(preds_f_mean_nine, col="red")
lines(lower_nine, lty=2, col="red")
lines(upper_nine, lty=2, col="red")
legend("topleft", c("data", "forecasts", "95% prediction intervals"),
       col=c("black", "red", "red" ),lty=c(1, 1, 2))
# The predicted values seem to be close to the actual ones, but it seems that the arima model
# from before is a better fit for this data 

# We saw that the linear model is not the best for these data. What we could do is fit 
# the quadratic model 
t_sq = y_df$t^2
quad_fit = lm(Sales ~ t + t_sq + month, data = y_df)

fits_quad = quad_fit$fitted.values
resids_quad = quad_fit$residuals


par(mfrow = c(1,2))
## Convert fits and resids to ts objects:
fits_quad = ts(fits_quad, start=start(data), end=end(data), frequency=frequency(data))
resids_quad = ts(resids_mean, start=start(data), end=end(data), frequency=frequency(data))
## Plot the data and overlay the fitted values
plot(data, ylab="Product Sales", type = "l")
lines(fits_quad, col="red")
legend("topleft", c("data", "fitted values"), col=c("black", "red"), lty=1)
## Plot the residuals
plot(resids_quad, ylab="Residuals")

# The residual seem correlated so we need to account for this 
par(mfrow = c(1,2))
acf(resids_quad)
pacf(resids_quad)
# The acf gradually goes to zero and pacf cuts of after lag 1 so we can use the previus model
# for the residuals 
mod_res_quad = arima(resids_quad, order = c(1,0,0), include.mean = FALSE)
# Now lets examine the residuals on this fit 
plot(mod_res_quad$residuals, ylab = "Residuals")
acf(mod_res_quad$residuals)
pacf(mod_res_quad$residuals)
# Now from these plots we do not observe any obvious problem 
# Lets overfit
# Now lets examine the residuals on this fit 
mod_res_quad_over = arima(resids_quad, order = c(2,0,0), include.mean = FALSE)

loglik_quad = 2*(mod_res_quad_over$loglik - mod_res_quad$loglik)
1 - pchisq(loglik_quad,1)
# The p value is very significant so we add this parameter. Now we overfit again: 
mod_res_quad_over1 = arima(resids_quad, order = c(3,0,0), include.mean = FALSE)

loglik_quad1 = 2*(mod_res_quad_over1$loglik - mod_res_quad_over$loglik)
1 - pchisq(loglik_quad1,1)
# Now the p value is not significant so we do not add the third ar parameter

# Lets now use this model to predict values for the tenth year and compare them to the 
# actual values 
quad_y_df = data.frame(Sales = y, t = 1:120,t_sq = t_sq, month = months) 
quad_y_df_nine = data.frame(Sales = y[1:108], t = 1:108,t_sq = t_sq[1:108], month = months[1:108])
quad_model_nine = lm(Sales ~ ., data = quad_y_df_nine)
quad_resids_nine = quad_model_nine$residuals
# Transform them into time series 
quad_resids_nine_ts = ts(quad_resids_nine, start=start(data_nine), end=end(data_nine), frequency=frequency(data_nine))

## Perform prediction
new_month_ten = rep(factor(month.abb, levels=month.abb), 1)
quad_new_month_ten_data = data.frame(t=109:120, t_sq = (t_sq[109:120]), month=new_month)
quad_forecast_nine = predict(quad_model_nine, quad_new_month_ten_data, interval="prediction")

# First we need the naive predictions
quad_preds_naive_nine = ts(quad_forecast_nine[,1], frequency=12, start=c(2020, 1))
quad_lower_naive = ts(quad_forecast_nine[,2], frequency=12, start=c(2020, 1))
quad_upper_naive = ts(quad_forecast_nine[,3], frequency=12, start=c(2020, 1))

## Forecast residuals from fitted ARMA model
# First fit the arma model to the data up to 2019
quad_mod_res_nine = arima(quad_resids_nine_ts, order = c(2,0,0), include.mean = FALSE)
# Now find the predictions 
quad_forecast_resids_nine = predict(quad_mod_res_nine, n.ahead=12)
## Extract point forecasts and forecast variance for residuals
quad_preds_resids_nine = quad_forecast_resids_nine$pred
quad_predvar_resids_nine = quad_forecast_resids_nine$se^2
## Obtain variance used in construction of confidence interval
quad_confvar_nine = predict(quad_model_nine, quad_y_df[109:120,], interval="confidence", se.fit=TRUE)$se.fit^2
## Construct overall forecasts
quad_preds_nine = quad_preds_naive_nine + quad_preds_resids_nine
## Construct overall prediction intervals
quad_predvar_nine = quad_confvar_nine + quad_predvar_resids_nine
quad_lower_nine = quad_preds_nine - 1.96 * sqrt(quad_predvar_nine)
qua_upper_nine = quad_preds_nine + 1.96 * sqrt(quad_predvar_nine)

quad_mse = mean((actual_data-quad_preds_nine)^2)
quad_mse
# We see that the error for this model is much better, but still high when compared to the one
# for the arima model 


# DLM FIT 
library(dlm)
# Now we will fit the dlm model 
build_y = function(params) {
  dlmModPoly(order=2, dV=exp(params[1]), dW=exp(params[2:3])) +
    dlmModTrig(s=12, dV=0, dW=exp(rep(params[4], 11)))
}

# Check convergence 
y_fit = dlmMLE(data, parm=c(0,0,0,0), build=build_y)
y_fit$convergence

# Build the model using the maximum likelihood estimates 
y_mod = build_y(y_fit$par)


# Applying smoothing and adding trend line
y_smooth = dlmSmooth(data, y_mod)
y_level = y_smooth$s[,1]
plot(data, ylab = "Product sales", type = "l")
lines(y_level, col="red")

# We can also generate forecasts for the next 6 months 
## Generate forecasts
y_filt = dlmFilter(data, y_mod)
y_preds = dlmForecast(y_filt, nAhead=6)
## Extract point forecasts and prediction interval limits
preds_dlm = y_preds$f[,1]
lower_dlm = preds_dlm - 1.96 * sqrt(unlist(y_preds$Q))
upper_dlm = preds_dlm + 1.96 * sqrt(unlist(y_preds$Q))
lower_dlm_ts = ts(c(data[length(data)],lower_dlm), start = end(data), frequency = frequency(data))
upper_dlm_ts = ts(c(data[length(data)],upper_dlm), start = end(data), frequency = frequency(data))


# Now plot the data and the intervals
y_f = ts(c(data, upper_dlm), start=start(data),
            frequency=frequency(data))
preds_dlm_f = ts(c(data[length(data)], preds_dlm), start=end(data),
             frequency=frequency(data))
## Plot data, forecasts and prediction intervals
plot(y_f, col="red", ylab="Milk production", type="n")
lines(data, col="black")
lines(preds_dlm_f, col="red")
lines(lower_dlm_ts, lty=2, col="red")
lines(upper_dlm_ts, lty=2, col="red")
legend("topleft", c("data", "forecasts", "95% prediction intervals"),
       col=c("black", "red", "red"), lty=c(1, 1, 2))


# Now we can fit the model to the first 9 years to see the prediction performance
y_fit_nine = dlmMLE(data[1:108], parm=c(0,0,0,0), build=build_y)
y_fit_nine$convergence
y_mod_nine = build_y(y_fit_nine$par)


## Generate forecasts
y_filt_nine = dlmFilter(data[1:108], y_mod_nine)
y_preds_nine = dlmForecast(y_filt_nine, nAhead=12)
## Extract point forecasts and prediction interval limits
preds_dlm_nine = y_preds_nine$f[,1]
lower_dlm_nine = preds_dlm_nine - 1.96 * sqrt(unlist(y_preds_nine$Q))
upper_dlm_nine = preds_dlm_nine + 1.96 * sqrt(unlist(y_preds_nine$Q))
preds_dlm_nine_ts = ts(c(data[108],preds_dlm_nine), start = end(data_nine), frequency = frequency(data))
lower_dlm_nine_ts = ts(c(data[108],lower_dlm_nine), start = end(data_nine), frequency = frequency(data))
upper_dlm_nine_ts = ts(c(data[108],upper_dlm_nine), start = end(data_nine), frequency = frequency(data))

mse_dlm = mean((actual_data_nine - preds_dlm_nine)^2)
mse_dlm

# Now we can plot the actual values and the predicted ones 
## Plot data in black and forecasts in red and blue
plot(data, ylab="Product Sales", type="n")
lines(data, col="black")
lines(preds_dlm_nine_ts, col="red")
lines(lower_dlm_nine_ts, lty=2, col="red")
lines(upper_dlm_nine_ts, lty=2, col="red")
legend("topleft", c("data", "forecasts", "95% prediction intervals"),
       col=c("black", "red", "red" ),lty=c(1, 1, 2))

# We see that the predictions are very close to the predictions. However the arima model 
# still has the smallest mean squared error 















