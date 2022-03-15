rm(list=ls())
a = c(1340435.04, 1336807.39, 1416379.04, 1492278.87, 1401790.42, 1353982.61, 1344152.29, 1380266.48, 1342358.03, 1358967.97, 1390961.08, 1566046.44, 1476068.83, 1438616.76, 1432834.61, 1422438.72, 1398955.5, 1363730.71, 1362604.36, 1368236.75, 1348838.34, 1458211.89, 1430962.37, 1631717.16, 1457179.98, 1437459.95, 1508513.31, 1470133.64, 1457653.74, 1425507.77, 1457282.93, 1443338.14, 1474048.92, 1549536.22, 1571034.8, 1776413.12, 1588076.3, 1557627.71, 1573957.6, 1569240.7, 1517240.24, 1496742.06, 1567795.42, 1501114.64, 1547922.93, 1533609.48, 1577889.36, 1797978.87, 1661692.84, 1622701.06, 1818420.42, 1569240.7, 1671335.16, 1562346.08, 1574525.32, 1544559.65, 1637495.61,1560385.57, 1633210, 1857941.79, 1725128.38, 1711623.51, 1811090.48, 1763533.17, 1746717.85, 1684583.12, 1664658.29, 1679477.28, 1794287.84, 1825664.51, 1907863.84, 2179174.28, 1994577.62, 1978886.94, 1983633.07, 1975812.32, 1897917.15, 1873544, 1769750.47, 1868735.07, 1781046.41, 1791194.52, 1896156.51, 2157229.65, 1945444.2, 1937337.46, 2039324.37, 1957219.31, 1930695.53, 1900671.87, 1824826.08, 1928744.27, 1926382.16, 1956009.42, 2100129.91, 2329706.58, 2139666.39, 2241313.48, 2153220.11, 2158698.81, 2111848.42, 2014073.95, 2003090.93, 2018840.38, 2005600.83, 2055946.55, 2203274.21, 2134035.86, 2249769.69, 2186983.4, 2296174.39, 2307392.55, 2353337.05, 2300831.63, 2395917.03, 2370886.01, 2427329.31, 2498977.64, 2659710.45, 2908462.4)
length(a)
A<-ts(a,frequency=12,start=c(2011))
A
plot.ts(A)
library(tseries)
library(ggplot2)#time series
library(lmtest)#time series
library(forecast)#time series
adf.test(A,alternative="stationary")

B<-diff(A)
B

library("writexl")
df = data.frame(name = c(B))
print (df)
w = write_xlsx(df,"C:\\Users\\Boluwatife Deborah\\Documents\\ring.xlsx")
w
adf.test(B,alternative="stationary")
plot(B)
C<-diff(B)
C

bf = data.frame(name = c(C))
print (bf)
t = write_xlsx(bf,"C:\\Users\\Boluwatife Deborah\\Documents\\cry.xlsx")
t

dwtest(lm(A~c(1:120)))

adf.test(C,alternative="stationary")
plot(C)


acf(B)
pacf(B)
auto.arima(C,d=2,seasonal= TRUE)

arima(B,order=c(2,1,0))
arima(B,order=c(2,1,1))
arima(B,order=c(2,1,2))
fit<-arima(B,order=c(2,1,0))
mean(B)
fit

fit<-auto.arima(B,d=1,seasonal=FALSE)
tsdiag(fit)

coeftest(fit)
Box.test(fit$residuals,lag=1,type="Box-Pierce")

##Ho:there is no autocorrelation among the residuals of the fitted model
##H1:Not Ho
###to run the Ljungbox statistic
residuals<-resid(fit)
Box.test(residuals,type="Ljung-Box")


fcast<-forecast(fit,h=12)
fcast
plot(fcast)
write.csv(fcast,"five years.csv")


cf = data.frame(name = c(fcast))
print (cf)
q = write_xlsx(cf,"C:\\Users\\Boluwatife Deborah\\Documents\\burn.xlsx")
q

getwd()
