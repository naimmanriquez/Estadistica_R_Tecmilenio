## Librerias con las que vamos a trabajar

library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)

## En este caso vamos a trabajar con Samsung y primero llamamos en R los datos
## de Samsung

getSymbols("005930.KS",from="2015-08-01",to="2020-09-11")

## Ahora vamos a decirle a R que nos de los retornos diarios 

KS_log_returns<-`005930.KS`%>%Ad()%>%dailyReturn(type='log')

### Gráficamos la serie

`005930.KS`%>%Ad()%>%chartSeries()

## Le pedimos que nos de una linea de tendencia de como se va a comportar

`005930.KS`%>%chartSeries(TA='addBBands();addVo();addMACD()',subset='2020-03-01::2020-09-09')

## Ahora si queremos comparar con otras industrias 

## Google

getSymbols("GOOGL",from="2015-08-01",to="2020-09-11")

# Facebook

getSymbols("FB",from="2015-08-01",to="2020-09-11")

# Apple

getSymbols("AAPL",from="2015-08-01",to="2020-09-11")

## Comparamos

data<-cbind(diff(log(Cl(`005930.KS`))),diff(log(Cl(GOOGL))),diff(log(Cl(AAPL))),diff(log(Cl(FB))))

## Pedimos gráfico de correlaciones

chart.Correlation(data)

## La correlacion es un numero entre -1 y 1 que mide la asociación entre una 
## variable y otra. Si es 0.5 o mayor la relacion es positiva y fuerte
## en este ejemplo nos dice que hay una relación positiva fuerte entre
## amazon y google si sube la accion de amazon es probable que suba la de google



fb_log_returns<-FB%>%Ad()%>%dailyReturn(type='log')

FB%>%Ad()%>%chartSeries()

FB%>%chartSeries(TA='addBBands();addVo();addMACD()',subset='2020')



fb_log_returns<-FB%>%Ad()%>%dailyReturn(type='log')
FB%>%Ad()%>%chartSeries()
