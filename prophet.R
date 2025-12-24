library(prophet)

prophetdata <- data.frame(
  ds = preprocessed_data$Date,
  x = preprocessed_data$Absences_orig_smth,
  y = preprocessed_data$Cases_orig_smth
)

fit_and_forecast_prophet <- function(data) {
  model <- prophet()
  model <- add_regressor(model, "x")
  model_fit <- fit.prophet(model, data)
  forecast <- predict(model_fit, data.frame(ds = data$ds, x = data$x))
  
  return(forecast)
}

prophet_forecast <- fit_and_forecast_prophet(prophetdata)
rmse <- function(predictions2, actual2) {
  sqrt(mean((predictions2 - actual2)^2, na.rm = TRUE))
}


nrmse <- function(predictions2, actual2) {
  rmse_val2 <- rmse(predictions2, actual2)
  rmse_val2 / sqrt(mean(actual2^2, na.rm = TRUE))
}

pred<-prophet_forecast$yhat
act<-prophet_data$y
rmsepro <- rmse(pred, act)
nrmsepro <- nrmse(pred, act)

cat("RMSE:", rmsepro, "\n")
cat("NRMSE:", nrmsepro, "\n\n")

library(dplyr)

lag <- 0
lagged_data <- prophetdata %>%
  mutate(y = lag(y, lag))
best_lag <- NULL
best_rmse <- Inf
lag_range <- -20:20 
for (lag in lag_range) {
  if (lag == 0) {
    lagged_data <- prophetdata
  } else if (lag > 0) {
    lagged_data <- prophetdata %>%
      mutate(y = dplyr::lead(y, lag))
  } else {
    lagged_data <- prophetdata %>%
      mutate(y = dplyr::lag(y, abs(lag)))
  }
  prophet_forecast <- fit_and_forecast_prophet(lagged_data)
  pred <- prophet_forecast$yhat
  act <- lagged_data$y
  rmsepro <- rmse(pred, act)
  if (rmsepro < best_rmse) {
    best_lag <- lag
    best_rmse <- rmsepro
  }
  cat("Lag:", lag, "RMSE:", rmsepro, "\n")
}

cat("Best Lag:", best_lag, "\n")

best_lag<-2
lagged_data <- prophetdata %>%
  mutate(y = dplyr::lead(y, best_lag))
lagged_data <- na.omit(lagged_data)
lagged_data$y
prophet_forecast1 <- fit_and_forecast_prophet(lagged_data)
prophet_forecast1$yhat
pred1<-prophet_forecast1$yhat
act1<-lagged_data$y
rmsepro1 <- rmse(pred1, act1)
nrmsepro1 <- nrmse(pred1, act1)
cat("RMSE:", rmsepro1, "\n")
cat("NRMSE:", nrmsepro1, "\n\n")

plot_data2 <- data.frame(
  Date = lagged_data$ds,
  actual1 = act1,
  Predicted = pred1
)

library(ggplot2)

ggplot(plot_data2, aes(x = Date)) +
  geom_line(aes(y = actual1, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1, linetype = "dashed") +
  labs(x = "Date", y = "Outcomes", title = "Actual vs. Predicted Outcomes") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(legend.position = "top")

