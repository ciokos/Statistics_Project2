# Read the dataset 'bmi2_data.csv' into R
D <- read.table("bmi2_data.csv", header = TRUE, sep = ";")

# Add log-BMI to the dataset
D$logbmi <- log(D$bmi)

# Summary statistics

# Histograms:
# logbmi
hist(D[,5], main = "Density Histogram of Log-BMI", xlim = c(2.7,3.8),xlab="log-BMI", prob=TRUE,
     las=1, 
     col = "green")
abline(v=median(D[,5]),col="red")
abline(v=mean(D[,5]),col="blue")

# age
hist(D[,3], main = "Density Histogram of Age", xlim = c(10,80),xlab="age", prob=TRUE,
     las=1, 
     col = "green")
abline(v=median(D[,3]),col="red")
abline(v=mean(D[,3]),col="blue")

# fastfood
hist(D[,4], main = "Density Histogram of Fast food", xlim = c(0,365),xlab="fastfood", prob=TRUE,
     las=1, 
     col = "green")
abline(v=median(D[,4]),col="red")
abline(v=mean(D[,4]),col="blue")

# Box Plots
# logbmi
boxplot(D[,5], xlab="Log-BMI")

# age
boxplot(D[,3], xlab="Age")

# fastfood
boxplot(D[,4], xlab="Fast food")

# Scatter Plots
# logbmi vs age
plot(D[,5], D[,3], xlab = "logbmi", ylab = "age")
# logbmi vs fastfood
plot(D[,5], D[,4], xlab = "logbmi", ylab = "fastfood")

Tbl <- data.frame()
for(i in 2:5){
  Tbl[i-1, "Number of obs."] <- sum(!is.na(D[,i]))
  Tbl[i-1, "Sample mean"] <- mean(D[,i])
  Tbl[i-1, "Sample st. dev."] <- sd(D[,i])
  Tbl[i-1, "Lower quartile"] <- quantile(D[,i], 0.25)
  Tbl[i-1, "Median"] <- median(D[,i])
  Tbl[i-1, "Upper quartile"] <- quantile(D[,i], 0.75)
}

row.names(Tbl) <- c("bmi", "age","fastfood", "logbmi")
xtable(Tbl, align = "p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}")

# Subset containing the first 840 observations (for model estimation)
D_model <- subset(D, id <= 840)

# Subset containing the last 7 observations (for validation)
D_test <- subset(D, id >= 841)

# Estimate multiple linear regression model
fit <- lm(logbmi ~ age + fastfood, data = D_model)

# Show parameter estimates etc.
summary(fit)

# Plots for model validation

# Observations against fitted values
plot(fit$fitted.values, D_model$logbmi, xlab = "Fitted values",     
       ylab = "log(BMI)")

# Residuals against each of the explanatory variables
plot(D_model$EXPLANATORY_VARIABLE, fit$residuals, 
        xlab = "INSERT TEXT", ylab = "Residuals")

# Residuals against fitted values
plot(fit$fitted.values, fit$residuals, xlab = "Fitted values", 
     ylab = "Residuals")

# Normal QQ-plot of the residuals
qqnorm(fit$residuals, ylab = "Residuals", xlab = "Z-scores", 
       main = "")
qqline(fit$residuals)

# Confidence intervals for the model coefficients
confint(fit, level = 0.95)

# Predictions and 95% prediction intervals
pred <- predict(FINAL_MODEL, newdata = D_test, 
                interval = "prediction", level = 0.95)

# Observed values and predictions
cbind(id = D_test$id, logbmi = D_test$logbmi, pred)

