
#Includes only the r code from each section of the R Markdown file

# 0.1 Libraries
library(prophet)
library(zoo)
library(ggplot2)


## 0.2 The Data
monthly_co2_data = read.csv('data/monthly_co2.csv')
class(monthly_co2_data$co2)
head(monthly_co2_data)


# 1 Time Series Analysis

# 1.1 Constructing the Time Series
co2_ts = ts(monthly_co2_data$co2, start = c(1959,1), frequency = 12)
summary(co2_ts)
plot(co2_ts, main = "Monthly atmospheric CO2 concentrations from 1959 to 2025", xlab = "Year", ylab = "CO2 concentrations (ppm)")


# 1.3 Time Series Decomposition
co2_decompose = decompose(co2_ts)
plot(co2_decompose)

# 1.4 Linear Regression Trend Analysis
time_index = 1:length(co2_ts)
linear_model = lm(co2_ts ~ time_index)
summary(linear_model)

plot(time_index, as.numeric(co2_ts), type = "l", main = "CO2 Time Series with Linear Trend", xlab = "Months since January 1959", ylab = "CO2 Concentration (ppm)")
lines(time_index, fitted(linear_model), col = "red", lwd = 2)

# 1.5 Seasonality Analysis
acf(co2_ts, lag.max = 60, main = "Autocorrelation Function of CO2 Time Series")


# 2 Prophet Forecasting

# 2.2 Data Preparation
co2_df = data.frame(ds = as.Date(as.yearmon(time(co2_ts))), y = as.numeric(co2_ts))
head(co2_df)

# 2.3 Prophet Model
prophet_model = prophet(co2_df)

# 2.4 Forecasting
future_120_months = make_future_dataframe(prophet_model, periods = 120, freq = "month")
future_240_months = make_future_dataframe(prophet_model, periods = 240, freq = "month")

forecast_predictions_120 = predict(prophet_model, future_120_months)
forecast_predictions_240 = predict(prophet_model, future_240_months)

plot(prophet_model, forecast_predictions_120) + labs(title = "120 Month Forecast of Atmospheric CO2", x = "Year", y = "CO2 Concentration (ppm)")
plot(prophet_model, forecast_predictions_240) + labs(title = "240 Month Forecast of Atmospheric CO2", x = "Year", y = "CO2 Concentration (ppm)")

# 2.5 Interactive Forecasting Plots

dyplot.prophet(prophet_model, forecast_predictions_120, xlab="Year", ylab="CO2 Concentration (ppm)", main="120 Month Forecast of Atmospheric CO2")
dyplot.prophet(prophet_model, forecast_predictions_240, xlab="Year", ylab="CO2 Concentration (ppm)", main="240 Month Forecast of Atmospheric CO2")

prophet_plot_components(prophet_model, forecast_predictions_120)
prophet_plot_components(prophet_model, forecast_predictions_240)

# 2.6 Prophet Components
prophet_plot_components(prophet_model, forecast_predictions_120)
prophet_plot_components(prophet_model, forecast_predictions_240)




