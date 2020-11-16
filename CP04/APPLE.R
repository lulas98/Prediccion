# En esta práctica se hará la predicción de ventas de Apple usando ETS y Arima.
# Se realizarán las predicciones de manera agregada y desagregada.
rm(list = ls())

library(magrittr)
library(dplyr)
require(forecast)
require(xts)
require(ggplot2)
library(ggfortify)


apple <-read.csv("IngresosApple.csv",sep=";")


##### En esta primera parte vamos a obtener las ventas totales por trimestre. #####

Ingresos <- apple$Ingresos
rawDate <- seq(as.Date("2008/04/01"), as.Date("2017/7/01"), by = "quarter")

xIngresos <- xts(Ingresos, order.by=rawDate)

xIngresos <- to.quarterly(xIngresos)

TotalIngresos = as.zoo(xIngresos$xIngresos.Close)

names(TotalIngresos) 
View(TotalIngresos)


#####  modelo ETS y el modelo ARIMA  #####

##### Modelo ETS  #####

## Grafico General
autoplot(TotalIngresos)+ggtitle("Ingresos Trimestrales Apple")+xlab("Trimestres")+ylab("Ventas")
## Grafico por Trimestres
ggfreqplot(as.ts(TotalIngresos),freq=4,nrow=1,facet.labeller=c("1T","2T","3T","4T"))+ggtitle("Ingresos Trimestrales")

#Select number of observation to compare forecast
comit=4

#Data Size
Obs=length(TotalIngresos)

#sub_sample
#oIngresos=zIngresos[1:(Obs-comit),]
zIngresos =as.zoo(xIngresos$xIngresos.Close)
oIngresos <- window(TotalIngresos,start=index(TotalIngresos[1]),end=index(TotalIngresos[Obs-comit]))

#Fit Simple Exponential Smoothing
fit1 <- ses(oIngresos)

#Fit Holt
fit2 <- holt(oIngresos)

#Fit Holt- exponential
fit3 <- holt(oIngresos,exponential= TRUE, initial="simple")

#Fit Holt - damped
fit4 <- holt(oIngresos,damped=TRUE)

#Fit Holt - (exponential+damped)
fit5 <- holt(oIngresos,exponential=TRUE,damped=TRUE)

# Results for first model:
fit1$model

#Plot models fitted
plot(fit3, type="o", ylab="Ventas",  flwd=1, plot.conf=FALSE)
lines(window(oIngresos),type="o")
lines(fit1$mean,col=1)
lines(fit2$mean,col=3)
lines(fit4$mean,col=5)
lines(fit5$mean,col=6)
legend("topleft", lty=1, pch=1, col=1:7,
       c("Data","SES","Holt's","Exponential",
         "Additive Damped","Multiplicative Damped"))


#seasonal model Holt-winters
fit6 <- hw(oIngresos,seasonal="additive")
fit7 <- hw(oIngresos,seasonal="multiplicative")

#Plot models
plot(fit7,ylab="Ingresos",
     plot.conf=FALSE, type="o", fcol="black", xlab="Year")
lines(window(TotalIngresos),type="o",col="darkblue")
lines(fitted(fit6), col="red", lty=2)
lines(fitted(fit7), col="green", lty=2)
lines(fit6$mean, type="o", col="red")
lines(fit7$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))


#Calculate Components
states <- cbind(fit6$model$states[,1:3],fit7$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states, xlab="Year")
fit6$model$state[,1:3]
fitted(fit6)
fit6$mean


## Select automatic ETS
etsfit<-ets(oIngresos)
#forecast model
fingresos.ets=forecast(etsfit)
#Results
summary(fingresos.ets)

#Plot
plot(fingresos.ets)
lines(window(TotalIngresos),type="o")

#Actual and Forecast
matrix(c(fingresos.ets$mean[1:comit],TotalIngresos[(Obs-comit+1):Obs]),ncol=2)


## Select automatic ETS
etsfit2<-ets(oIngresos,damped=TRUE)
#forecast model
fingresos.ets2=forecast(etsfit2)
#Results
summary(fingresos.ets2)

#Plot
plot(fingresos.ets2)
lines(window(TotalIngresos),type="o")

#Actual and Forecast
matrix(c(fingresos.ets2$mean[1:comit],fingresos.ets$mean[1:comit],TotalIngresos[(Obs-comit+1):Obs]),ncol=3)

#Plot all models
plot(fingresos.ets2)
lines(window(TotalIngresos),type="o")
lines(fingresos.ets$mean,type="o",col="red")

#Prediccion en tiempo real (los datos mas recientes)
model2=ets(TotalIngresos, damped=T)
f.model2=forecast(model2)
plot(f.model2)


##### Modelo ARIMA para todas las variables agregadas #####

df_new <- data.frame(value = as.vector(TotalIngresos),
                     time = time(TotalIngresos))

ggplot(df_new)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ylab("Ingresos")+ggtitle("Ingresos Trimestrales Apple")+xlab("Trimestres")


#Log transformation?
zlIngresos=log(zIngresos)
df_newl <- data.frame(value = as.vector(zlIngresos),
                      time = time(zlIngresos))
ggplot(df_newl)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ylab("Ingresos")+ggtitle("Ingresos Trimestrales LOG Apple")+xlab("Trimestres")


#Difference
ggtsdisplay(zlIngresos)
ggtsdisplay(diff(zlIngresos))
ggtsdisplay(diff(zlIngresos,4))
ggtsdisplay(diff(diff(zlIngresos,4),1))

#Select number of observation to compare forecast
comit=3

#Data Size
Obs=length(TotalIngresos)

#sub_sample
oIngresos <- window(TotalIngresos,start=index(TotalIngresos[1]),end=index(TotalIngresos[Obs-comit]))

#out sample (real data to forecast performance)
pIngresos <- window(TotalIngresos,start=index(TotalIngresos[Obs-comit+8]),end=index(TotalIngresos[Obs+8]))


#ARIMA MODEL
fit1=auto.arima(oIngresos,lambda=0)
summary(fit1)

#residual analysis
ggtsdisplay(fit1$residuals)

#box-Ljung Test
Box.test(fit1$residuals,lag=4, fitdf=3, type="Lj")
Box.test(fit1$residuals,lag=8, fitdf=3, type="Lj")
Box.test(fit1$residuals,lag=12, fitdf=3, type="Lj")

fventas.arima=forecast(fit1)

ggplot(df_new)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ geom_forecast(fventas.arima,alpha=0.4)+ggtitle("ARIMA: Predicción Apple")

fventas.arima

