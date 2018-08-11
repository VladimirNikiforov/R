#http://www.r-exercises.com/2017/05/05/forecasting-arimax-model-exercises-part-5/

#http://www.r-exercises.com/2017/05/05/forecasting-arimax-model-exercises-part-5-solutions/

####################
#                  #
#    Exercise 1    #
#                  #
####################
require(ggplot2)
require(gridExtra)
df <- read.csv("Icecream.csv")
p1 <- ggplot(df, aes(x = X, y = cons)) +
  ylab("Consumption") +
  xlab("") +
  geom_line() +
  expand_limits(x = 0, y = 0)
p2 <- ggplot(df, aes(x = X, y = temp)) +
  ylab("Temperature") +
  xlab("") +
  geom_line() +
  expand_limits(x = 0, y = 0)
p3 <- ggplot(df, aes(x = X, y = income)) +
  ylab("Income") +
  xlab("Period") +
  geom_line() +
  expand_limits(x = 0, y = 0)
grid.arrange(p1, p2, p3, ncol=1, nrow=3)
#plot of chunk forecasting-part-5
####################
#                  #
#    Exercise 2    #
#                  #
####################
require(forecast)
fit_cons <- auto.arima(df$cons)
fcast_cons <- forecast(fit_cons, h = 6)

####################
#                  #
#    Exercise 3    #
#                  #
####################
require(forecast)
autoplot.forecast(fcast_cons)
#plot of chunk forecasting-part-5
####################
#                  #
#    Exercise 4    #
#                  #
####################
require(forecast)
accuracy(fit_cons)
##                        ME       RMSE        MAE        MPE     MAPE
## Training set 0.0001020514 0.03525274 0.02692065 -0.9289035 7.203075
##                   MASE       ACF1
## Training set 0.8200619 -0.1002901
# The MASE is equal to 0.8200619

####################
#                  #
#    Exercise 5    #
#                  #
####################
require(forecast)
fit_cons_temp <- auto.arima(df$cons, xreg = df$temp)
fcast_temp <- c(70.5, 66, 60.5, 45.5, 36, 28)
fcast_cons_temp <- forecast(fit_cons_temp, xreg = fcast_temp, h = 6)
autoplot.forecast(fcast_cons_temp)
#plot of chunk forecasting-part-5
####################
#                  #
#    Exercise 6    #
#                  #
####################
summary(fcast_cons_temp)
## 
## Forecast method: Regression with ARIMA(0,1,0) errors
## 
## Model Information:
## Series: df$cons 
## Regression with ARIMA(0,1,0) errors 
## 
## Coefficients:
##       df$temp
##        0.0028
## s.e.   0.0007
## 
## sigma^2 estimated as 0.001108:  log likelihood=56.03
## AIC=-108.06   AICc=-107.59   BIC=-105.32
## 
## Error measures:
##                       ME       RMSE        MAE      MPE     MAPE      MASE
## Training set 0.002563685 0.03216453 0.02414157 0.564013 6.478971 0.7354048
##                    ACF1
## Training set -0.1457977
## 
## Forecasts:
##    Point Forecast     Lo 80     Hi 80     Lo 95     Hi 95
## 31      0.5465774 0.5039101 0.5892446 0.4813234 0.6118313
## 32      0.5337735 0.4734329 0.5941142 0.4414905 0.6260566
## 33      0.5181244 0.4442225 0.5920263 0.4051012 0.6311476
## 34      0.4754450 0.3901105 0.5607796 0.3449371 0.6059529
## 35      0.4484147 0.3530078 0.5438217 0.3025024 0.5943270
## 36      0.4256524 0.3211393 0.5301654 0.2658135 0.5854913
# the coefficient for the temperature variable is 0.0028
# the standard error of the coefficient is 0.0007
# the mean absolute scaled error is 0.7354048, which is smaller than
# the error for the initial model (0.8200619)

####################
#                  #
#    Exercise 7    #
#                  #
####################
require(lmtest)
coeftest(fit_cons_temp)
## 
## z test of coefficients:
## 
##           Estimate Std. Error z value  Pr(>|z|)    
## df$temp 0.00284529 0.00074313  3.8288 0.0001288 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# The p-value is equal to 0.0001288, it implies that the coefficient is significant at
# 5% level.

####################
#                  #
#    Exercise 8    #
#                  #
####################
temp_column <- matrix(df$temp, ncol = 1)
income <- c(NA, NA, df$income)
income_matrix <- embed(income, 3)
vars_matrix <- cbind(temp_column, income_matrix)
print(vars_matrix)
##       [,1] [,2] [,3] [,4]
##  [1,]   41   78   NA   NA
##  [2,]   56   79   78   NA
##  [3,]   63   81   79   78
##  [4,]   68   80   81   79
##  [5,]   69   76   80   81
##  [6,]   65   78   76   80
##  [7,]   61   82   78   76
##  [8,]   47   79   82   78
##  [9,]   32   76   79   82
## [10,]   24   79   76   79
## [11,]   28   82   79   76
## [12,]   26   85   82   79
## [13,]   32   86   85   82
## [14,]   40   83   86   85
## [15,]   55   84   83   86
## [16,]   63   82   84   83
## [17,]   72   80   82   84
## [18,]   72   78   80   82
## [19,]   67   84   78   80
## [20,]   60   86   84   78
## [21,]   44   85   86   84
## [22,]   40   87   85   86
## [23,]   32   94   87   85
## [24,]   27   92   94   87
## [25,]   28   95   92   94
## [26,]   33   96   95   92
## [27,]   41   94   96   95
## [28,]   52   96   94   96
## [29,]   64   91   96   94
## [30,]   71   90   91   96
####################
#                  #
#    Exercise 9    #
#                  #
####################
require(forecast)
fit_vars_0 <- auto.arima(df$cons, xreg = vars_matrix[, 1:2])
fit_vars_1 <- auto.arima(df$cons, xreg = vars_matrix[, 1:3])
fit_vars_2 <- auto.arima(df$cons, xreg = vars_matrix[, 1:4])
print(fit_vars_0$aic)
## [1] -113.3357
print(fit_vars_1$aic)
## [1] -111.9228
print(fit_vars_2$aic)
## [1] -110.2497
# The AIC can be used because the models have the same order of integration (0).
# The model with the lowest value of the AIC is the first model.
# Its AIC is equal to -113.3357.

####################
#                  #
#    Exercise 10   #
#                  #
####################
require(forecast)
expected_temp_income <- matrix(c(fcast_temp, 91, 91, 93, 96, 96, 96),
                               ncol = 2, nrow = 6)
fcast_cons_temp_income <- forecast(fit_vars_0,
                                   xreg = expected_temp_income,
                                   h = 6)
autoplot.forecast(fcast_cons_temp_income)
plot of chunk forecasting-part-5
accuracy(fit_cons)[, "MASE"]
## [1] 0.8200619
accuracy(fit_cons_temp)[, "MASE"]
## [1] 0.7354048
accuracy(fit_vars_0)[, "MASE"]
## [1] 0.7290753
# the model with two external regressors has the lowest 
# mean absolute scaled error (0.7290753)