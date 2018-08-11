library(dplyr)
library(forecast)
library(ggplot2)

my_df <- read.csv("c:/R/Rscripts/fact.csv", sep=",")
str(my_df)

ggplot(my_df, aes(read, math, col = gender))+
  geom_point(size = 5)+
  facet_grid(.~hon)+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))

?glm
fit  <- glm(rsumm ~ read + math + gender, my_df, family = "binomial")
summary(fit)

exp(fit$coefficients)

head(predict(object = fit))

head(predict(object = fit, type = "response"))

my_df$prob  <- predict(object = fit, type = "response")
?arima


df_store <- filter(my_df,ID_OBJ==589)
df_store$N_YEAR <- factor(df_store$N_YEAR,ordered=T)
df_store$N_DAY_YEAR <- factor(df_store$N_DAY_YEAR,ordered=T)
str(df_store)
df_store_ordered <- df_store[order(df_store$N_YEAR, df_store$N_DAY_YEAR), ]
df_store_ordered$HOLIDAY <- factor(df_store_ordered$HOLIDAY,ordered=T)
df_store_ordered$PRE_HOLIDAY <- factor(df_store_ordered$PRE_HOLIDAY,ordered=T)
str(df_store_ordered)
df_store_ordered$HOLIDAY <- ifelse(is.na(df_store_ordered$HOLIDAY),0,1)
df_store_ordered$PRE_HOLIDAY <- ifelse(is.na(df_store_ordered$PRE_HOLIDAY),0,1)


#http://www.r-exercises.com/2017/05/05/forecasting-arimax-model-exercises-part-5-solutions/

ts(df_store_ordered,start=2015,frequency = 365)
m_aa <- auto.arima(ts(df_store_ordered,start=2015,frequency = 365)[,c(4)],parallel = T,stepwise = F,xreg = df_store_ordered[,8:10])

df_ts <- ts(df_store_ordered$AMOUNT,start=2015,frequency = 365)
m_aa <- auto.arima(df_ts,xreg = df_store_ordered[,8:10])
#fcast_temp <- c(70.5, 66, 60.5, 45.5, 36, 28)
#fcast_cons_temp <- forecast(fit_cons_temp, xreg = fcast_temp, h = 6)
f_aa <- forecast(m_aa, h=365)
plot(f_aa)
