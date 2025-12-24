pkg_list = c("tidyverse", "lubridate", "zoo", "dplyr", "tidyr", "lubridate", "ggplot2", "mgcv", "reticulate", "MASS")
pacman::p_load(char=pkg_list)
df <- read.csv("newdata.csv")


data <- data.frame(
  Date = df$Date,
  Cases = df$Cases,
  Absences = df$Absences
)


preproc_data <- function(data, 
                         groupVar = NULL,
                         varsToInterp = NULL,
                         varsToProcess = NULL,
                         minDate = NULL,
                         maxDate = NULL,
                         maxgap = 24,
                         procSuffix = "_smth",
                         origSuffix = "_orig") {
  
  if (is.null(varsToInterp)) {
    varsToInterp = names(data %>% select(where(is.numeric)))
  }
  if (is.null(varsToProcess)) {
    varsToProcess = unique(c(varsToInterp))
  } 
  print(paste0("Grouping by ", paste(groupVar, collapse = " ")))
  print(paste0("Applying interpolation to ", paste(varsToInterp, collapse = " ")))
  print(paste0("Variables to be processed: ",paste(varsToProcess, collapse = " ")))
  
  dataProc <- data %>% 
    mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
           week = cut(Date, breaks = "1 week", labels = FALSE),
           Absences = as.numeric(gsub(",", "", Absences))) %>% 
    # add complete step to list dates by day instead of week
    complete(Date = seq.Date(min(Date),max(Date),by="days")) %>% 
    group_by(across(all_of(groupVar))) %>%
    rename_with(.fn = ~ paste0(.x, origSuffix), .cols = matches(varsToInterp)) %>% 
    mutate(across(where(is.numeric) & matches(varsToInterp), .fns = ~na.approx(.x, x=Date, maxgap=maxgap))) %>% 
    rename_with(.fn = ~ paste0(.x, procSuffix), .cols = matches(varsToInterp)) %>% 
    mutate(across(where(is.numeric) & ends_with(procSuffix), ~round(.x))) %>%
    mutate(across(where(is.numeric) & ends_with(procSuffix), 
                  .fns = ~c(0, diff(.x)), 
                  .names = "{.col}_d1")) %>% 
    mutate(across(where(is.numeric) & ends_with(procSuffix), 
                  .fns = ~c(0, 0, diff(.x, lag=1, difference=2)), 
                  .names = "{.col}_d2"))
  
  return(dataProc)
}

preprocessed_data <- preproc_data(data, 
                                  varsToInterp = c("Cases", "Absences"),
                                  varsToProcess = c("Cases", "Absences")
                                  
)

preprocessed_data %>%  
  ggplot() + 
  geom_line(aes(x=Date, y=10*Cases_orig_smth, colour="Cases")) + 
  geom_line(aes(x=Date, y=Absences_orig_smth, color="School absences")) + 
  scale_x_date(breaks="week") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


predata <- data.frame(
  Date = preprocessed_data$Date,
  Absences_orig_smth = preprocessed_data$Absences_orig_smth,
  Cases_orig_smth = preprocessed_data$Cases_orig_smth
)
rmse <- function(predictions, actual) {
  sqrt(mean((predictions - actual)^2, na.rm = TRUE))
}


nrmse <- function(predictions, actual) {
  rmse_val <- rmse(predictions, actual)
  rmse_val / sqrt(mean(actual^2, na.rm = TRUE))
}



predictorVar <- "Absences_orig_smth"
outcomeVar <- "Cases_orig_smth"
start_date <- "2022-09-05"
end_date <- "2023-06-26"


poisson_model <- function(data,
                          predictorVar,
                          outcomeVar,
                          start_date,
                          end_date,
                          lag_range = c(-20, 20),
                          replace.na = NULL) {
  
  
  timeWindow <- seq.Date(as.Date(start_date), as.Date(end_date), by = "week")
  
  
  lag_values <- list()
  rsquared_values <- list()
  
  
  for (lag in lag_range) {
    laggedData <- data
    
    
    if (lag >= 0) {
      laggedData[[outcomeVar]] <- dplyr::lead(laggedData[[outcomeVar]], lag)
    } else {
      laggedData[[outcomeVar]] <- dplyr::lag(laggedData[[outcomeVar]], abs(lag))
    }
    
    
    laggedData <- na.omit(laggedData)
    
    
    poisson_model <- glm(
      formula = paste(outcomeVar, "~", paste(predictorVar, collapse = "+")),
      data = laggedData,
      family = poisson(link = "log")
    )
    
    
    rsquared <- summary(poisson_model)$deviance / summary(poisson_model)$null.deviance
    lag_values <- c(lag_values, lag)
    rsquared_values <- c(rsquared_values, rsquared)
  }
  
  
  result_df <- data.frame(Lag = unlist(lag_values), R_squared = unlist(rsquared_values))
  
  return(result_df)
}

rsqaure_values <- poisson_model(
  predata,
  predictorVar,
  outcomeVar,
  start_date,
  end_date,
  lag_range = seq(-50, 50),
  replace.na = NULL
)

print(rsqaure_values)

ggplot(data = rsqaure_values, aes(x = Lag, y = R_squared)) +
  geom_line(aes(color = "Data Points"), size = 1) +
  geom_smooth(method = "loess", se = FALSE, aes(color = "Smoothed Curve"), span = 0.3) + 
  labs(x = "Delay (Lag)", y = "R-squared") +
  ggtitle("R-squared vs. Delay (Lag) for Poisson Regression Model") +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  scale_color_manual(name = "Legend", values = c("blue", "red"), labels = c("Data Points", "Smoothed Curve"))


subset_data <- subset(rsqaure_values, Lag >= 0 & Lag <= 50)
lag <- subset_data$Lag[which.max(subset_data$R_squared)]


laggedData <- predata 
laggedData[[outcomeVar]] <- dplyr::lag(laggedData[[outcomeVar]], lag)
laggedData[[predictorVar]] <- dplyr::lag(laggedData[[predictorVar]], lag)
laggedData <- na.omit(laggedData)
laggedData
predata

poisson_model <- glm(
  formula = paste(outcomeVar, "~", paste(predictorVar, collapse = "+"), " + lag(", outcomeVar, ", ", lag, ")"),
  data = laggedData,
  family = poisson()
)

summary(poisson_model)

predictions <- predict(poisson_model, newdata = laggedData, type = 'response')


actual <- laggedData$Cases_orig_smth


rmse_val <- rmse(predictions, actual)
nrmse_val <- nrmse(predictions, actual)



cat("Lag:", lag, "\n")
cat("RMSE:", rmse_val, "\n")
cat("NRMSE:", nrmse_val, "\n\n")

plot_data <- data.frame(
  Date = laggedData$Date,
  Actual = actual,
  Predicted = predictions
)


ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1, linetype = "dashed") +
  labs(x = "Date", y = "Outcomes", title = "Actual vs Predicted Outcomes") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(legend.position = "top")


negative_binomial_model <- function(data, predictorVar, outcomeVar, lag) {
  laggedData1 <- data
  
  if (lag >= 0) {
    laggedData1[[outcomeVar]] <- dplyr::lead(laggedData1[[outcomeVar]], lag)
  } else {
    laggedData1[[outcomeVar]] <- dplyr::lag(laggedData1[[outcomeVar]], abs(lag))
  }
  
 
  laggedData1 <- laggedData1 %>%
    filter(!is.na(Cases_orig_smth), !is.na(Absences_orig_smth))
  
  
  negbin_model <- glm.nb(
    formula = paste(outcomeVar, "~", predictorVar),
    data = laggedData1
  )
  
  rsquared <- summary(negbin_model)$deviance / summary(negbin_model)$null.deviance
  return(rsquared)
}

lag_range <- seq(-50, 50)


lag_r2_values <- c()
for (lag in -lag_range) {
  r2 <- negative_binomial_model(predata, predictorVar, outcomeVar, lag)
  lag_r2_values <- c(lag_r2_values, r2)
}


lag_r2_df <- data.frame(Lag = lag_range, R_squared = lag_r2_values)


print(lag_r2_df)



best_lag <- NULL
best_r2 <- -Inf
for (lag in 0:max(lag_range)) {
  r2 <- negative_binomial_model(predata, predictorVar, outcomeVar, lag)
  if (r2 > best_r2) {
    best_r2 <- r2
    best_lag <- lag
  }
}


cat("Best Lag:", best_lag, "\n")
cat("Best R-squared:", best_r2, "\n")


lag_r2_values <- c()
for (lag in lag_range) {
  r2 <- negative_binomial_model(predata, predictorVar, outcomeVar, lag)
  lag_r2_values <- c(lag_r2_values, r2)
}

lag_r2_df <- data.frame(Lag = lag_range, R_squared = lag_r2_values)


ggplot(data = lag_r2_df, aes(x = Lag, y = R_squared)) +
  geom_line(aes(color = "Data Points"), size = 1) +
  geom_smooth(method = "loess", se = FALSE, aes(color = "Smoothed Curve"), span = 0.3) +
  labs(x = "Lag", y = "R-squared") +
  ggtitle("R-squared vs. Lag for Negative Binomial Model") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "Legend", values = c("blue", "red"), labels = c("Data Points", "Smoothed Curve"))


laggedData1 <- predata
laggedData1[[outcomeVar]] <- dplyr::lag(laggedData1[[outcomeVar]], best_lag)
laggedData1[[predictorVar]] <- dplyr::lag(laggedData1[[predictorVar]], best_lag)


laggedData1 <- na.omit(laggedData1)


negbin_model <- glm.nb(
  formula = paste(outcomeVar, "~", paste(predictorVar, collapse = "+"), " + lag(", outcomeVar, ", ", best_lag, ")"),
  data = laggedData1
)


summary(negbin_model)
predictions1 <- predict(negbin_model, newdata = laggedData1, type = 'response')
actual1 <- laggedData1$Cases_orig_smth

rmse <- function(predictions1, actual1) {
  sqrt(mean((predictions1 - actual1)^2, na.rm = TRUE))
}

nrmse <- function(predictions1, actual1) {
  rmse_val1 <- rmse(predictions1, actual1)
  rmse_val1 / sqrt(mean(actual1^2, na.rm = TRUE))
}

rmse_val1 <- rmse(predictions1, actual1)
nrmse_val1 <- nrmse(predictions1, actual1)


cat("Lag:", best_lag, "\n")
cat("RMSE:", rmse_val1, "\n")
cat("NRMSE:", nrmse_val1, "\n\n")


plot_data1 <- data.frame(
  Date = laggedData1$Date,
  actual1 = actual1,
  Predicted = predictions1
)


ggplot(plot_data1, aes(x = Date)) +
  geom_line(aes(y = actual1, color = "actual1"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1, linetype = "dashed") +
  labs(x = "Date", y = "Outcomes", title = "actual1 vs Predicted Outcomes") +
  scale_color_manual(values = c("actual1" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(legend.position = "top")


GAMmodel <- function(data, predictorVar, outcomeVar, lag, start_date, end_date) {
  laggedData2 <- data
  
  if (lag >= 0) {
    laggedData2[[outcomeVar]] <- lead(laggedData2[[outcomeVar]], lag)
  } else {
    laggedData2[[outcomeVar]] <- lag(laggedData2[[outcomeVar]], abs(lag))
  }
  
  laggedData2 <- laggedData2[laggedData2$Date >= start_date & laggedData2$Date <= end_date, ]
  
  formula <- as.formula(paste(outcomeVar, " ~ s(", predictorVar, ")"))
  
  if (nrow(laggedData2) < 5) { 
    return(NA) 
  }
  
  model <- gam(formula, data = laggedData2, family = poisson(link = 'log'))
  
  null_formula <- as.formula(paste(outcomeVar, " ~ 1"))
  null_model <- gam(null_formula, data = laggedData2, family = poisson(link = 'log'))

  psuedor2 <- 1 - (deviance(model, type = "response") / deviance(null_model, type = "response"))

  # alternative method for calculating pseudo R.sq
  # psuedor2 = summary(model)$r.sq
  
  return(psuedor2)
}

lag_range <- seq(-50, 50)


psuedor2_values <- sapply(lag_range, function(lag) {
  GAMmodel(predata, predictorVar, outcomeVar, lag, start_date, end_date)
})


result_df <- data.frame(Lag = lag_range, DevianceExplained = psuedor2_values)
print(result_df)

subset_data1 <- subset(result_df, Lag >= 0 & Lag <= max(lag_range) & DevianceExplained <= 1)
best_lag1 <- subset_data1$Lag[which.max(subset_data1$DevianceExplained)]

cat("Best Lag:", best_lag1, "\n")


ggplot(data = result_df, aes(x = Lag, y = DevianceExplained)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "red", span = 0.3) +
  labs(x = "Lag", y = "Psuedo R^2") +
  ggtitle("Deviance Explained vs. Lag") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("blue", "red"), labels = c("R-squared", "Smoothed Curve"))

laggedData2 <- predata 
laggedData2[[outcomeVar]] <- dplyr::lag(laggedData2[[outcomeVar]], best_lag1)
laggedData2[[predictorVar]] <- dplyr::lag(laggedData2[[predictorVar]], best_lag1)
laggedData2 <- na.omit(laggedData2)

formula <- as.formula(paste(outcomeVar, " ~ s(", predictorVar, ")"))
model <- gam(formula, data = laggedData2, family = poisson(link = 'log'))
predictions2 <- predict(model, newdata = laggedData2, type = "response")

summary(model)

rmse <- function(predictions2, actual2) {
  sqrt(mean((predictions2 - actual2)^2, na.rm = TRUE))
}


nrmse <- function(predictions2, actual2) {
  rmse_val2 <- rmse(predictions2, actual2)
  rmse_val2 / sqrt(mean(actual2^2, na.rm = TRUE))
}



actual2 <- laggedData2$Cases_orig_smth


rmse_val2 <- rmse(predictions2, actual2)
nrmse_val2 <- nrmse(predictions2, actual2)



cat("Lag:", best_lag1, "\n")
cat("RMSE:", rmse_val2, "\n")
cat("NRMSE:", nrmse_val2, "\n\n")



plot_data2 <- data.frame(
  Date = laggedData2$Date,
  actual1 = actual2,
  Predicted = predictions2
)


ggplot(plot_data2, aes(x = Date)) +
  geom_line(aes(y = actual2, color = "actual2"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1, linetype = "dashed") +
  labs(x = "Date", y = "Outcomes", title = "actual vs Predicted Outcomes") +
  scale_color_manual(values = c("actual2" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(legend.position = "top")

