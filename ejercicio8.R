
### EJERCICIO 8 ###
## PROBLEMA 1 ###

### Para los siguientes valores

library(readxl)

ejercicio8a <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio8a.xlsx")

### Calcula e interpreta:
### a) La desviaci�n absoluta media (DAM).
### b) El error cuadr�tico medio (ECM). 
### c) El error porcentual absoluto medio (EPAM) 
### d) El error porcentual medio (EPM). 

## Error Porcentual Absoluto Medio (MAPE o Mean Absolute Percentage Error) 
## Mean Squared Error (MSE) o Error cuadratico medio
## Mean Absolute Error (MAE) es la desviaci�n absoluta media
## Error porcentual medio es MPE

library(forecast)

fit1 <- rwf(ejercicio8a$yt[1:5], h = 5)

accuracy(fit1)

rm(list=ls())

### PROBLEMA 2 ###

##Se utilizaron dos modelos de pron�stico para producir los valores 
##futuros de la venta en millones de unidades de peri�dicos 
##en circulaci�n en M�xico, estos valores
##se muestran en la tabla siguiente, 
##junto con los valores reales observados.

library(readxl)

ejercicio8b <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio8b.xlsx")

## Calcula e interpreta los incisos solicitados para el modelo 1 y el modelo 2:
## a) La desviaci�n absoluta media (DAM).
## b) El error cuadr�tico medio (ECM).
## c) El error porcentual absoluto medio (EPAM).
## d) El error porcentual medio (EPM).
## e) �Cu�l de los dos modelos es m�s preciso para realizar pron�sticos y por qu�?

library(forecast)

fit1 <- rwf(ejercicio8b$modelo1[1:4], h = 4)

accuracy(fit1)

fit2 <- rwf(ejercicio8b$modelo2[1:4], h = 4)

accuracy(fit2)

rm(list=ls())

### PROBLEMA 3 ###

## Se utilizaron tres t�cnicas de pron�stico para predecir los valores 
## de la producci�n de botellas en miles semanales de una marca local. 
## Estos valores se dan en la siguiente tabla.

library(readxl)

ejercicio8c <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio8c.xlsx")

View(ejercicio8c)

## Calcula e interpreta los incisos solicitados para la t�cnica 1, t�cnica 2 y t�cnica 3:
## a) La desviaci�n absoluta media (DAM).
## b) El error cuadr�tico medio (ECM).
## c) El error porcentual absoluto medio (EPAM).
## d) El error porcentual medio (EPM).
## e) �Cu�l de los dos modelos es m�s preciso para realizar pron�sticos y por qu�?

library(forecast)

fit1 <- rwf(ejercicio8c$tecnica1[1:5], h = 5)

accuracy(fit1)

fit2 <- rwf(ejercicio8c$tecnica2[1:5], h = 5)

accuracy(fit2)

fit3 <- rwf(ejercicio8c$tecnica3[1:5], h = 5)

accuracy(fit3)


