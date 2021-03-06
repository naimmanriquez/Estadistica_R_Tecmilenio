## EJERCICIO 12 A ##

## Se llevo a cabo un conjunto de ensayos experimentales para determinar 
## una forma de predecir el tiempo de cocimiento en minutos (Y) 
## a varios niveles de amplitud del horno, (pies, X1) 
## y temperatura de cocci�n (grados Celsius, X2). 
## Los datos obtenidos fueron registrados como se muestra a continuaci�n:

library(readxl)

ejercicio12a <- read_excel("C:/Users/naim_/Desktop/Curso_R/ejercicio12a.xlsx")

attach(ejercicio12a)

# Definir variables

Y <- cbind(Y)
X1 <- cbind(X1, X2)

# PARTE A #

## Estima la ecuaci�n de regresi�n m�ltiple.

olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level=0.95)

# PARTE B #

## Interpreta los coeficientes individuales de la ecuaci�n de regresi�n lineal 
## m�ltiple considerando el contexto del problema.

## respuesta: interpretar

## PARTE C ##

## Pronostica el tiempo de cocimiento cuando el nivel de amplitud 
## del horno es de 5 pies y la temperatura de cocci�n es de 20 grados Celsius.

## respuesta: calcular con la formula, sustituir valores x1 y x2


rm(list = ls())


### EJERCICIO 12 B ###

#El supervisor de una empresa est� examinando la relaci�n existente 
#entre la calificaci�n que obtiene un empleado en una prueba de aptitud, 
#su experiencia previa y el �xito en el trabajo. 
#Se estudia y se pondera la experiencia de un empleado en trabajos anteriores 
#y se obtiene una calificaci�n entre 2 y 12. 
#La medida del �xito en el empleo se basa en un sistema de puntuaci�n 
#que incluye producci�n total y eficiencia, 
#con valor m�ximo posible de 50. El supervisor tom� una muestra de seis empleados
#con menos de un a�o de antig�edad y obtuvo lo siguiente:

library(readxl)

ejercicio12b <- read_excel("C:/Users/naim_/Desktop/Curso_R/ejercicio12b.xlsx")

attach(ejercicio12b)

# Definir variables

Y <- cbind(Y)
X1 <- cbind(X1, X2)

# PARTE A #

## Estima la ecuaci�n de regresi�n m�ltiple.

olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level=0.95)

# PARTE B #

## Interpreta los coeficientes individuales de la ecuaci�n de regresi�n lineal 
## m�ltiple; considerando el contexto del problema.

# PARTE C #

## Si un empleado obtuvo 83 puntos en la prueba de aptitud 
## y ten�a una experiencia en trabajos anteriores de 7 a�os, 
## �qu� evaluaci�n de desempe�o puede esperar?


