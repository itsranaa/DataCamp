
#Loading Packages
library(tseries)
library(forecast)
library(TSA)
library(lmtest)
library(fGarch)
library(rugarch)
library(aTSA)
library(readxl)
library(dplyr)

#Loading Data
data1 <- read.csv("C:\\Users\\LENOVO\\Downloads\\datakurs.csv")
train <- data1[c(1:356),]
test <- data1[c(357:366),]
train$Date <- as.Date(train$ï..Date,"%m/%d/%y")
train_ts <- ts(train$kurs,frequency = 365, start=c(2020,1))
plot(train$Date, train_ts,
     main="Nilai Tukar Rupiah Terhadap Dolar AS 1/1/20 - 21/12/20",type='l')

#Akan dianalisis log return, maka ditransformasi ke log terlebih dahulu
log_train <- (log(train_ts))
df_log_train <- diff(log_train)
plot(df_log_train)

#Spesifikasi Model
auto.arima(log_train)
eacf(df_log_train) #Model 1,1,0 - 0,1,1 - 1,1,1 - 2,1,1
fit110 <- arima (log_train, order= c(1,1,0))
fit011 <- arima (log_train, order= c(0,1,1))
coeftest(fit011)
fit111 <- arima (log_train, order= c(1,1,1))
fit211 <- arima (log_train, order= c(2,1,1))
cbind(fit110,fit011, fit111, fit211)

#Dari keempat model,ARIMA (0,1,1) memiliki AIC terkecil, maka akan dipilih model tsb
coeftest(fit011) #parameter signifikan

#Diagnostic Checking
#Uji Independensi Residual
adf.test(fit011$residuals) #H0 ditolak, residual stasioner
Box.test (fit011$residuals^2, lag=1, type="Ljung") #H0 tidak ditolak, tidak ada autokorelasi
jarque.bera.test(fit011$residuals) #residual tidak berdistribusi normal
arch.test(fit011) #uji heteroskedastisitas - Ada efek ARCH

#Overfitting
overfit1 <- arima(log_train, order = c(1,1,1))
coeftest(overfit1)
overfit2 <- arima (log_train, order = c(0,1,2))
coeftest(overfit2)
cbind(fit011,overfit1,overfit2)
#Parameter tidak seluruhnya signifikan, maka model arima (0,1,1) merupakan best model

######## GARCH MODEL ##########
garch10 <- garch (df_log_train*100, order=c(1,0), trace  = F)
garch11 <- garch (df_log_train*100, order=c(1,1), trace  = F)
garch20 <- garch (df_log_train*100, order=c(2,1), trace  = F)
aic_score <- AIC (garch10, garch11, garch20)
aic_score #Maka dipilih GARCH (1,1)

#Model Selection ARIMA + GARCH
finalspec <- ugarchspec (variance.model = list(model= 'sGARCH', garchOrder = c (1,1)),
                     mean.model = list(armaOrder = c(0,1), include.mean=FALSE),
                     distribution.model = "norm")
final_model <- ugarchfit (spec = finalspec, data = df_log_train, out.sample =10)
plot(final_model, which =9)

#Forecasting 10 periode
forecast <- ugarchforecast(final_model, data = df_log_train,n.ahead =10)
print(forecast)
plot(forecast)

#For loop untuk peramalan dalam bentuk mata uang
x = 14126.05
return <- c(-0.00012466,-0.0003441,-0.0003441,-0.0003441,-0.0003441,
            -0.0003441,-0.0003441,-0.0003441,-0.0003441,-0.0003441 )
lt = 0
for (i in 1:10){
  lt = x *((2.71828)^(return[i]))
  print(lt)
  x = lt
}
fitted <- c (14124.9,14119.43,14114.57,14109.72,14104.86,
             14100.01,14095.16,14090.31,14085.46,14080.62)
test$Date <- as.Date(test$ï..Date,"%m/%d/%y")
fitted_data <- data.frame( fitted,test$Date)

#Plot Peramalan
plot(train$kurs,type="l", main='Nilai Tukar Rupiah Terhadap Dolar AS  Tahun 2020',
     xlab= 'Periode',
     ylab=  'Nilai Tukar')
lines(seq(356,365),fitted, lwd= 2, col= 'red')
lines(seq(356,365),test$kurs[1:10], lwd= 3, col= 'blue')
legend('bottomright',col=c('blue','red'),
       legend = c('nilai aktual','nilai prediksi'),lwd=2,bty='n')

#Forecast Dengan Model ARIMA saja
arima_f <- forecast(fit011, lead =10) #didapat forecast exp(9.56)

#MAPE 
mape <- (mean(abs(test$kurs-fitted)/test$kurs))*100
mape
mape_arima <- (mean(abs(test$kurs-exp(9.56))/test$kurs))*100
mape_arima








