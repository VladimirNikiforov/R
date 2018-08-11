#Основы статистики часть3
#https://stepik.org/course/%D0%9E%D1%81%D0%BD%D0%BE%D0%B2%D1%8B-%D1%81%D1%82%D0%B0%D1%82%D0%B8%D1%81%D1%82%D0%B8%D0%BA%D0%B8-%D0%A7%D0%B0%D1%81%D1%82%D1%8C-3-2152/


library(ggplot2)
qplot(x=hp,y=mpg,data=mtcars)
qplot(x=hp^0.5,y=mpg,data=mtcars)
qplot(x=hp^-0.5,y=mpg,data=mtcars)
qplot(x=-hp^-0.5,y=mpg,data=mtcars)
qplot(x=hp,y=mpg,data=mtcars)
qplot(x=hp,y=mpg,data=mtcars)
#https://shiny.rstudio.com/

fit1 <- lm(mpg~hp,mtcars)
fit2 <- lm(mpg~I(-hp^-0.7),mtcars)
summary(fit1)
summary(fit2)

qplot(x=log(hp),y=log(mpg),data=mtcars)
fit3 <- lm(log(mpg)~log(hp),mtcars)
summary(fit3)
#=>при изменении hp на 1% мы получим изменение mpg на -0.5%


hist(fit1$residuals)
shapiro.test(fit1$residuals) #- тест Шапиро-Вилка что наши остатки распределены нормально
shapiro.test(fit2$residuals)
shapiro.test(fit3$residuals)
hist(fit3$residuals)
qplot(x=fit3$fitted.values,y=fit3$residuals)

########
Трансформация Бокса — Кокса (Box-Cox transformation) — широко используемый метод трансформации данных. В контексте регрессии он обычно используется для трансформации зависимой переменной в случае, если у нас есть ненормальное распределение ошибок и/или нелинейность взаимосвязи, а также в случае гетероскедастичности.

Идея трансформации очень простая:
  
  ynew=(y^p−1)/p, если p≠0

ynew=log(y), если p=0

Параметр p подбирается по схожей идее: мы будем использовать то p, при котором качество модели максимально (обычно используется метод максимального правдоподобия). 

Например, в случае множественной регрессии мы можем трансформировать зависимую переменную, чтобы добиться более высокого качества модели и выполнения требования к данным.
#
library(MASS)
boxcox(lm(dist~speed,data=cars),lambda=seq(0,1,by=.1))

######
#Гетероскедастичность
library(dplyr)
diamonds_2 <- sample_n(diamonds,500)
qplot(x=price,y=carat,data=diamonds_2)+geom_smooth(method=lm)
fit_1 <- lm(carat~price, diamonds_2)
coefficients(fit_1)
plot(fit_1)


qplot(x=log(price),y=log(carat),data=diamonds_2)+
  geom_smooth(method = lm)

library(lmtest)
bptest(lm(price~carat,diamonds_2)) #p-value<0.05=>есть гетероскедастичность - наша ошибка не постоянна!
bptest(lm(log(price)~log(carat),diamonds_2))

fit_2 <- lm(log(carat)~log(price), diamonds_2)
shapiro.test(fit_1$residuals) #p_value<0.05 => наши остатки распределены не нормально
shapiro.test(fit_2$residuals) #p_value<0.05 => наши остатки распределены не нормально
plot(fit_2)
summary(fit_1)
summary(fit_2)

#Мультиколлениарность
set.seed(42)
d <- data_frame(y=rnorm(30),
                x_1=rnorm(30),
                x_2=x_1,
                x_3=rnorm(30))
fit <- lm(y~.,d)
summary(fit) # - in coefficients will be NA!

pairs(d)
select(d,-y)%>%pairs()

###############
#Мультиколлинеарность 2
head(cars)
qplot(x=speed,y=dist,data=cars)
fit_1 <- lm(dist~speed,cars)
summary(fit_1)

cars <- mutate(cars,speed_2=speed^2,speed_3=speed^3)
pairs(cars)
fit_2 <- lm(dist~.,cars)
summary(fit_2)
data(cars)#возврат старых значений

library(DAAG)
head(swiss)
fit_1 <- lm(Fertility~.,swiss)
summary(fit_1)
cor.test(~ Fertility+Examination,swiss)
vif(fit_1)

fit_2 <- lm(Fertility~.,select(swiss,-Examination))
summary(fit_2)
vif(fit_2)


###############################
# Давайте реализуем простейший вариант теста для проверки наличия гетероскедастичности.  
# Напишите функцию hetero_test, которая получает на вход набор данных. 
# Первая колонка в данных - зависимая переменная, остальные колонки - независимые. 
# Функция строит регрессионную модель, используя эти переменные, а затем проверяет, есть ли в данных  гетероскедастичность.
# Для этого функция строит вспомогательную регрессию, в которой зависимая переменная - это квадраты остатков исходной модели, 
# а независимые переменные - это предикторы из исходной модели. 
# Функция должна возвращать значение R квадрат этой вспомогательной модели.
hetero_test(mtcars)
#[1] 0.4660497

hetero_test <-  function(test_data){
  
  
}



#################################################
# В этой задаче вам необходимо оценить 95% интервал для медианы при помощи basic bootstrap. Напишите функцию median_cl_boot, которая получает на вход числовой вектор произвольной длины и возвращает вектор из двух значений - верхней и нижней границы доверительного интервала для медианы.
# 
# Для расчета доверительного интервала используйте симуляцию из 1000 бутстрапированных выборок.
# 
# Подсказка!
median_cl_boot <- function(x){
  m <- matrix(sample(x, size = 10^3*length(x), replace = TRUE), nrow = length(x))
  d   <- apply(m, 2, median)
  quantile(d, c(0.025, 0.975))
}

# median_cl_boot <- function(x) {
#   n <- length(x)
#   bs.medians <- sapply(1:1000, function(i) median(sample(x, n, replace = T)))
#   quantile(median(x) - bs.medians, probs = c(0.975, 0.025)) + median(x)
# }
# 
# median_cl_boot <- function(x){
#   
#   x_median = median(x)
#   samples = replicate(1000, sample(x, length(x), replace = T))
#   samples_median = apply(samples, 2, median)
#   dist = x_median - samples_median
#   percentiles = quantile(dist, probs = c(0.025,0.975))
#   conf_int = percentiles + x_median
#   
# }
# 
# median_cl_boot <- function(x){
#   boots <- sapply(1:1000, function(i)
#   {
#     median_boot <- median(x[sample(1:length(x),length(x), replace = TRUE)]-median(x))
#   })
#   return(quantile(boots, c(0.025,0.975))+median(x))
# }
###################################
# В этой задаче вам необходимо оценить 95% интервал для коэффициента наклона в линейной регрессии при помощи basic bootstrap. Напишите функцию slope_cl_boot, которая получает на вход dataframe с двумя переменными x и y произвольной длины и возвращает вектор из двух значений - верхней и нижней границы доверительного интервала для коэффициента наклона в модели y ~ x.
# 
# Для расчета доверительного интервала используйте симуляцию из 1000 бутстрапированных выборок.
# 
# Подсказка: как в данном случае извлекать бутсрапированную выборку? В отличии от предыдущего задания мы теперь должны случайно извлекать пары x и y, иными словами - строки из dataframe.
# http://www.statmethods.net/advstats/bootstrapping.html
slope_cl_boot <- function(df){
  # Bootstrap 95% CI for coeff lm
  library(boot)
  # function to obtain statistics
  rsq <- function(formula, data, indices) {
    d <- data[indices,] # allows boot to select sample 
    fit <- lm(formula, data=d)
    return(summary(fit)$coefficients[2,1])
  } 
  # bootstrapping with 1000 replications 
  results <- boot(data=df, statistic=rsq, 
                  R=1000, formula=y~x)
  # get 95% confidence interval 
  boot.ci(results, type="basic")[4]$basic[4:5]
}

# slope_cl_boot <- function(x, bootNums = 1000){
#   xRows <- nrow(x)
#   cor <- lm(y ~ x, data = x)$coefficients[2]
#   cors <- replicate(bootNums, lm(y ~ x, data = x[sample(xRows, replace=T),])$coefficients[2] - cor)
#   cor + quantile(cors, c(.025,.975))
# }
# 
# slope_cl_boot <- function(x){
#   fit = lm(y ~ x, d)
#   slope = coef(fit)[2]
#   samples = replicate(1000, d[sample(1:nrow(d), nrow(d), replace = T), ], simplify = F)
#   samples_slopes = sapply(samples, function(i) coef(lm(i[[2]] ~ i[[1]]))[2])
#   dist = slope - samples_slopes
#   percentiles = quantile(dist, probs = c(0.025,0.975))
#   conf_int = percentiles + slope
# }

