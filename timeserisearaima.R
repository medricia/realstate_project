# (1) 데이터 준비
data <- c(34500,33000,32000,32500,34000,35000,34000,33000,35000,36000,37000
          ,39000,38000,36500,35000,36000,36000,36000,37000,36000,37000,37000
          ,39000,40000,39000,38000,37000,37000,37000,38000,38000,38000,39000
          ,40000,40000,41000)
length(data)# 48

# (2) 시계열자료 생성
tsdata <- ts(data, start=c(2016, 1),frequency=12)

#tsdata <- AirPassengers # 실제 data 적용.
tsdata
head(tsdata)
tail(tsdata)

# (3) 시계열요소분해 시각화
ts_feature <- stl(tsdata, s.window="periodic")
plot(ts_feature)

# 단계2 : 정상성시계열 변환
par(mfrow=c(1,2))
ts.plot(tsdata)
diff <- diff(tsdata)
plot(diff) # 차분 시각화


# 단계3 : 모형 식별과 추정
library(forecast)
ts_model2 <- auto.arima(tsdata)
ts_model2 # ARIMA(0,1,1)(1,1,0)[12] / ARIMA(2,1,1)(0,1,0)[12], 계절성일 경우에 ()하나가 더옴 ex) ()()[12] 앞에는 모형, 뒤에 계절성 

# 단계4 : 모형 생성
model <- arima(tsdata, c(0,1,1), seasonal = list(order = c(1,1,0)))
#model <- arima(tsdata, c(2,1,1), seasonal = list(order = c(0,1,0)))
model

# 단계5 : 모형 진단(모형 타당성 검정)
# (1) 자기상관함수에 의한 모형 진단
tsdiag(model)

# (2)Box-Ljung에 의한 잔차항 모형 진단
Box.test(model$residuals, lag=1, type = "Ljung") # p-value = 0.5618 / p-value = 0.9879

# 단계6 : 미래 예측
par(mfrow=c(1,2))
fore <- forecast(model, h=48) # 2년 예측 , 상한을 띄지만 어느시점기준으로 하햔에 올수있다
plot(fore)
fore2 <- forecast(model, h=6) # 6개월 예측
plot(fore2)
fore

actual <- as.numeric(fore$actual)
predicted <- fore$`Point Forecast`

rmse <- sqrt(mean((actual - predicted)^2))
mae <- mean(abs(actual - predicted))
r2 <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)

print(rmse)
