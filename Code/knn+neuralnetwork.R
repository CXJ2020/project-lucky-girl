data<-read.csv("hushen.csv")
data_time<-ts(data$收盘)
plot.ts(data_time, ylab = "Closing Price", main = "after COVID-19")
#k取数据点数的平方根1310*（1/2）=36

library(tsfknn)
par(mfrow = c(2,1))
##knn回归的原=原理：将lags个值作为特征
predknn_after_covid <- knn_forecasting(data_time, h = 65, lags = 1:30, k = 36, msas = "MIMO")
#h-预测多少个值；lags-每组数据都是由30个值组成;找到36个最相近的组
plot(predknn_after_covid, main = "After COVID-19")
plot(data_time)
plot(predknn_after_covid)
#计算单个隐藏层的神经节点数
alpha <- 1.5^(-10)
hn_after_covid <- length(data$收盘)/(alpha*(length(data$收盘) + 65))
##应用单个隐藏层的前馈神经网络模型，该模型适用于单变量时间序列
#BoxCox.lambda将数据转换为正态分布
library(MASS)
library(forecast)
lambda_after_covid <- BoxCox.lambda(data$收盘)
dnn_pred_after_covid <- nnetar(data$收盘, size = hn_after_covid, lambda = lambda_after_covid)
dnn_forecast_after_covid <- forecast(dnn_pred_after_covid, h = 65, PI = TRUE)
plot(dnn_forecast_after_covid, title = "after COVID-19")
par(mfrow=c(3,1))
##定义计算误差的函数
rmse<-function(y,f){
  sqrt(mean((y-f)^2))}

##将预测值进行组合
dnn_predict<-dnn_forecast_after_covid[["mean"]]
predict_knn<-predknn_after_covid[["neighbors"]]
predicr_frame<-data.frame(knn_predict,knn_predict)

