#Cargamos los datos

library(readxl)

turismomaza <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/turismomaza.xlsx")

#Transformamos los datos en una serie temporal 

turistas.ts <- ts(turismomaza$turistas, start = c(2006,1), frequency = 12)

print(turistas.ts)

#Trazamos la serie de tiempo 

plot(turistas.ts)

library(ggplot2)
library(plotly)
library(ggthemes)
library(scales)

ggplot(data=turismomaza, aes(periodo, turistas)) + geom_line() +
  ggtitle("Llegada de turistas a Mazatlán desde 2006 a 2020") +
  ylab("Turistas (miles de personas)") +
  xlab("Periodo") + 
  theme(plot.title = element_text(hjust = 0.4)) 

autocorrelacion<-acf(turistas.ts, type ="correlation", plot = FALSE)

plot(autocorrelacion)

componentes.ts = decompose(turistas.ts)

plot(componentes.ts)

library(forecast)

ndiffs(turistas.ts)

nsdiffs(turistas.ts)

diff.turistas.ts<-autoplot(diff(turistas.ts), ts.linetype = "dashed", ts.colour = "blue")

diff.turistas.ts

autoplot(acf(diff(turistas.ts), plot = FALSE))

diff.turistasts.12<-diff(turistas.ts, lag = 12)

autoplot(diff.turistasts.12, ts.colour = "darkorange", ts.linetype = "dashed")

library(tseries)

adf<-adf.test(diff.turistasts.12)

adf$p.value


autoplot(acf(diff.turistasts.12, plot = FALSE))

autoplot(pacf(diff.turistasts.12, plot = FALSE))

#Mis modelos
arima1<- arima(turistas.ts,order=c(2,1,0), method = "ML")
arima1$aic

arima2<- arima(turistas.ts,order=c(2,1,1), method = "ML")
arima2$aic

arima3<- arima(turistas.ts,order=c(2,1,2), method = "ML")
arima3$aic

arima4<- arima(turistas.ts,order=c(1,1,2), method = "ML")
arima4$aic

mod_arima <- auto.arima(turistas.ts, seasonal = FALSE)

mod_arima

tsdisplay(residuals(mod_arima), lag.max=12, main='Residuos (0,1,2)')

kpss<-kpss.test(diff.turistasts.12)

kpss$p.value

mod_sarima <- auto.arima(turistas.ts, stepwise = TRUE, approximation = TRUE) #Selección por pasos y estimacion max. verosimilitud

mod_sarima

library(lmtest)

coeftest(mod_sarima)

autoplot(acf(mod_sarima$residuals, plot = FALSE))

autoplot(pacf(mod_sarima$residuals, plot = FALSE))

library(ggfortify)

ggtsdiag(mod_sarima)

independencia <- Box.test(mod_sarima$residuals, type="Ljung-Box") # Test de Ljung-Box
independencia$p.value

qqnorm(mod_sarima$residuals)
qqline(mod_sarima$residuals) 

normalidad <-shapiro.test(mod_sarima$residuals)    # Test de Shapiro-Wilk
normalidad$p.value  

prediccion <- forecast(mod_sarima, h=36) #nivel confianza 95%, h = periodos
autoplot(prediccion)

