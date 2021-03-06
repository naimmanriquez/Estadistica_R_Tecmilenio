### PROBLEMA 1 ###

## 1. La energ�a el�ctrica consumida (Y) 
## cada mes por una planta qu�mica se considera relacionada 
## con la temperatura ambiente promedio, grados Fahrenheit (X1), 
## n�mero de d�as al mes (X2), 
## la pureza promedio del producto, en porciento (X3) 
## y las toneladas obtenidas del producto (X4). 
## Se dispone de los datos hist�ricos del a�o anterior. 

library(readxl)

ejercicio13a <- read_excel("C:/Users/naim_/Desktop/Curso_R/ejercicio13a.xlsx")

attach(ejercicio13a)

# Definir variables

Y <- cbind(Y)
X1 <- cbind(X1, X2, X3, X4) 

## PREGUNTA 1 ##

## Estima e interpreta los coeficientes de la ecuaci�n de regresi�n lineal 
## m�ltiple.

olsreg1 <- lm(Y ~ X1)

summary(olsreg1)

## PREGUNTA 2 ##

## Interpreta los coeficientes de regresi�n en el contexto del problema.

## PREGUNTA 3 ##

## Prueba la significancia global del modelo de regresi�n m�ltiple; 
## realiza todas las etapas de una prueba de hip�tesis.

## supuetos del modelo ##

## Homocedasticidad ##

library(lmtest)

bptest(olsreg1)

# Residuos
e1hat <- resid(olsreg1)
summary(e1hat)

## Multicolinealidad ##

library(olsrr)

ols_vif_tol(olsreg1)

## Prueba la significancia de los coeficientes de regresi�n individuales. 
## Realiza todas las etapas de una prueba de hip�tesis para cada uno 
## de los coeficientes.

confint(olsreg1, level=0.99)

## Calcula e interpreta R2 en el contexto del problema y el error estandar.

summary(olsreg1)

rm(list = ls())



## PROBLEMA 2 ##

## Un negocio de ventas por cat�logo de computadoras personales, 
## software y hardware mantiene un almac�n centralizado 
## para la distribuci�n de los productos ordenados. 
## La administraci�n examina el proceso de distribuci�n 
## y est� interesada en examinar los factores que afectan 
## los costos. En la actualidad, se cobra una peque�a cuota por manejo, 
## independiente del monto de la orden. Se recolectaron datos de 
## los �ltimos 24 meses que indican los costos de distribuci�n (Y), 
## las ventas (X1) y el n�mero de �rdenes recibidas (X2). 

library(readxl)

ejercicio13b <- read_excel("C:/Users/naim_/Desktop/Curso_R/ejercicio13b.xlsx")

attach(ejercicio13b)

# Definir variables

Y <- cbind(Y)
X1 <- cbind(X1, X2) 


## PREGUNTA 1 ##

## Estima e interpreta los coeficientes de la ecuaci�n de regresi�n lineal 
## m�ltiple.

olsreg1 <- lm(Y ~ X1)

summary(olsreg1)

## PREGUNTA 2 ##

## Interpreta los coeficientes de regresi�n en el contexto del problema.

## PREGUNTA 3 ##

## Prueba la significancia global del modelo de regresi�n m�ltiple; 
## realiza todas las etapas de una prueba de hip�tesis.

## supuetos del modelo ##

## Homocedasticidad ##

library(lmtest)

bptest(olsreg1)

# Residuos
e1hat <- resid(olsreg1)
summary(e1hat)

## Multicolinealidad ##

library(olsrr)

ols_vif_tol(olsreg1)

## Prueba la significancia de los coeficientes de regresi�n individuales. 
## Realiza todas las etapas de una prueba de hip�tesis para cada uno 
## de los coeficientes.

confint(olsreg1, level=0.99)

## Calcula e interpreta R2 en el contexto del problema y el error estandar.

summary(olsreg1)


