
# EJERCICIO 7 #

# PRIMER PROBLEMA

library(readxl)

ejercicio7a <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio7a.xlsx")

## Los datos de la demanda anual de bolsas de fertilizante de una empresa agrícola se muestran en la siguiente tabla.

View(ejercicio7a)

library(forecast)

ejerciciotimeseries <- ts(ejercicio7a$demanda)

ejerciciotimeseries

## Grafica la serie de tiempo. 

plot(ejerciciotimeseries)

## Encuentra el valor de pronóstico para la demanda de fertilizante 
## para cada año, comenzando por el año 4 por medio de un promedio móvil 
## de k=3 años y realiza el pronóstico para el año 12. 

moving_average = forecast(ma(ejerciciotimeseries[1:11], order=3), h=2)

plot(moving_average)

rm(list = ls())



### PROBLEMA 2

## Aplica el suavizamiento exponencial y un valor inicial 
## de 38 para realizar lo que se solicita en los incisos del ejercicio.

library(readxl)

ejercicio7b <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio7b.xlsx")

ejerciciotimeseries <- ts(ejercicio7b$yt)

ejerciciotimeseries

## Grafica la serie de tiempo. 

plot(ejerciciotimeseries)

## Encuentra el valor de pronóstico para cada periodo t

fcast_ses <- ses(ejerciciotimeseries, h = 4)

plot(fcast_ses)

summary(fcast_ses)

rm(list = ls())

### PROBLEMA 3

## Las ventas de equipos de cocina han aumentado durante los últimos cinco años. 
## El gerente había pronosticado, antes de iniciar el negocio, 
## que las ventas del primer año serían de 360 equipos de cocina. 

library(readxl)

ejercicio7c <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio7c.xlsx")

library(forecast)

ejerciciotimeseries <- ts(ejercicio7c$ventas)

ejerciciotimeseries

## Grafica la serie de tiempo.

autoplot(ejerciciotimeseries, xlab='Tiempo', ylab='Ventas') 

fcast_ses <- ses(ejerciciotimeseries, h = 3)

plot(fcast_ses)

summary(fcast_ses)





