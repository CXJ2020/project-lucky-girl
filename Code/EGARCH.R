setwd("C:/Users/10982/E-GARCH")
NSDK<-read.csv("NSDK.csv")
RJ225<-read.csv("日经225指数.csv")
SZ<-read.csv("上证指数.csv")

#包载入
library(fGarch)
library(rugarch)#garch拟合与预测
library(TSA)#BIC准则确定arma阶数  eacf确定garch阶数
library(tseries)
library(zoo)#转换成时间序列类型
library(forecast)#auto.arima() arma阶数确定方法
library(psych)#数据描述统计分析
library(ggplot2)#绘图
library(FinTS)#JB统计量



#使用 diff 函数直接做差分求对数收益率
dNSDK<-diff(log(NSDK$close))
head(ddNSDK)


#正态性检验
par(mfrow=c(1,3),oma=c(0.2,0.2,0.2,0.2))
hist(dNSDK,main="NSDK Index Log Return Distribution",col="yellow",xlab="",ylim=c(0,40),probability=T)
lines(density(dNSDK),lwd=1);rug(dNSDK)#first graph
qqnorm(dNSDK);qqline(dNSDK)#second graph
dNSDK.ts<-ts(dNSDK,start=c(2019, 5 ,10),end=c(2020, 3 ,26),freq=222)
plot(dNSDK.ts,type="l",main=" Daily Rate of Return of  
     NSDK",xlab="Date", ylab="Rate", cex.main=0.95, las=1)#third graph
#分布为数据分布函数，QQ图，对数收益率序列点图。收益率分布图、QQ图可以看出
#金融时间序列确实表现出尖峰厚尾性，相对于标准正态分布，峰度更高，两段的尾部更厚，
#也就是极值更多。由收益率波动序列可以看出NSDK股票指数收益率序列在样本区间内均表现
#出一定的波动性和聚集性，尤其在后半段波动性更为明显。


#正态性检验
jarque.bera.test(dNSDK)
#P越小越非正态


#ACF (用ggplot2作图)
par(mfrow=c(2,1)) 
bacf <- acf(dNSDK, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))
ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_segment(mapping = aes(xend = lag, yend = 0),color='blue',size=5,alpha=I(1/2)) +
  geom_hline(aes(yintercept = 0.3),linetype = 2, color = 'darkblue')+
  geom_hline(aes(yintercept=0))

# pacf(用ggplot2作图)
bpacf <- pacf(dNSDK, plot = FALSE)
bpacf <- with(bpacf, data.frame(lag, acf))
ggplot(data = bpacf, mapping = aes(x = lag, y = acf)) +
  geom_segment(mapping = aes(xend = lag, yend = 0),color='blue',size=5,alpha=I(1/2))
  geom_hline(aes(yintercept = 0.2), linetype = 2, color = 'darkblue')+
  geom_hline(aes(yintercept=0))

#ACF PACF 调用forecast函数作图   
library("forecast")
forecast::ggtsdisplay(dNSDK)
ggAcf(dNSDK)
ggPacf(dNSDK)
#ACF图与PACF图大部分函数值在置信区间内（图中蓝色虚线区域）上下跳跃,所以收益率序列自相关性很低,
#或者说具有很弱的自相关性，因此在条件期望模型中不需要引入自相关性部分，
#满足GARCH模型中的均值方程，收益率由一个常数项加上一个随机扰动项组成。

#平稳性检验
#若 ADF  p<0.05 认为序列是均值回归的
adf.test(dNSDK,alt="stationary")
#p-value = 0.01043<0.05,故数据是平稳的


#ACF PACF 调用forecast函数作图   
library("forecast")
forecast::ggtsdisplay(dNSDK^2)
#但NSDK收益率平方的ACF值表现出了一定的相关性和持续性，其大部分值都超过了置信区间
#（图中蓝色虚线）。注意到NSDK收益率平方的ACF值在滞后10期后之后缓慢衰退,
#说明了方差序列具有一定程度的序列相关性,因此采用 GARCH 模型来描述股价波动过程中的
#条件方差。

#ARCH效应检验
#得先arima拟合模型，对残差进行LM检验
armamodel=auto.arima(dNSDK)#自动基于AIC最小准则，寻找最佳拟合模型
armamodel
plot(residuals(armamodel))
par(mfrow=c(1,1))
lmresult=McLeod.Li.test(y=residuals(armamodel))#残差arch效应很显著
#由图可知，残差序列滞后23阶后，残差自回归函数的系数显著，序列仍然存在自相关。因此 拒绝原假设，说明样本序列存在显著的ARCH效应。
#综上，对数收益率序列具有波动聚集性，序列平稳，有显著ARCH效应。序列时候GARCH模型建模。
ArchTest(dNSDK,lag=23)
#检验的原假设是：不存在 ARCH 效应。检验结果为卡方统计量的值为389.3，对应的
# P 值几乎为0，也就是说在 1% 的显著性水平上拒绝原假设，从而拒绝不存在 ARCH 效应的假设，
#收益率序列存在 ARCH 效应，可以进行 GARCH 模型的拟合。

myspec=ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(2, 1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "std"
)

myfit=ugarchfit(myspec,data=dNSDK,solver="gosolnp")
myfit

par(mfcol=c(1,1))
#残差正态性检验
plot(myfit,which=8)
plot(myfit,which=9)
shapiro.test(coredata(residuals(myfit)))#值越大，越表示不是正态，P越小越非正态
#残差相关性检验
acf(coredata(residuals(myfit)))
acf(residuals(myfit))
plot(myfit,which=10)
plot(myfit,which=11)
#系数是否显著
myfit #看P值是否够小
#拟合效果  残差如何
plot(myfit,which=3)
plot(residuals(myfit)) #看残差

plot(myfit,which=12)


