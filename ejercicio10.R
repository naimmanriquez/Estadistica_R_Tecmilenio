library(ggplot2)
library(ggthemes)
library(readxl)
library(dplyr)
library(scales)
library(readxl)

## EJERCICIO 10 ##
## PROBLEMA 1 ##

## Las ventas de l�nea blanca var�an seg�n el estado del mercado de casas nuevas: 
## cuando las ventas de casas nuevas son buenas, 
## tambi�n se reflejan �stas en las cifras de lavaplatos, 
## lavadoras de ropa, secadoras y refrigeradores. 
## Una asociaci�n de comercio compil� los siguientes datos hist�ricos 
## (en miles de unidades) de las ventas de l�nea blanca y construcci�n de casas.

ejercicio10a <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio10a.xlsx")

## PUNTO A ##
## Realiza un diagrama de dispersi�n para estos datos.

ggplot(ejercicio10a, aes(x=casas, y=ventas)) +
  geom_point(alpha=0.6) +
  theme_minimal()

## PUNTO B ##
## Desarrolla una ecuaci�n para la relaci�n entre las ventas de l�nea blanca 
## (en miles) y la construcci�n de casas (miles).

regresion <- lm(formula = ventas ~ casas, data = ejercicio10a)

summary(regresion)

## PUNTO C ##
## Interpreta la pendiente de la recta de regresi�n.

## PUNTO D ##

## Calcula e interpreta el coeficiente de determinaci�n de la muestra, 
## r2, para estos datos.

### PUNTO E ##
## Interpreta el error est�ndar de estimaci�n.


rm(list= ls())


## PROBLEMA 3 ##

## Una compa��a de productos qu�micos desea estudiar los efectos 
## que el tiempo de extracci�n tiene en la eficiencia de una operaci�n 
## de extracci�n, obteni�ndose los datos que aparecen en la siguiente tabla:

library(readxl)

ejercicio10c <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio10c.xlsx")

View(ejercicio10c)

## PUNTO A ##
## Realiza un diagrama de dispersi�n para verificar 
## que una l�nea recta se ajustar� bien a los datos.

library(ggplot2)
library(ggthemes)

ggplot(ejercicio10c, aes(x=tiempo, y=eficiencia)) +
  geom_point(alpha=0.6) +
  theme_minimal()

## PUNTO B ##
## Obt�n una l�nea de regresi�n estimada.

p1 <- ggplot(ejercicio10c, aes(x = tiempo, y = eficiencia)) + geom_point()

p3 <- p1 + geom_point(color="red") + geom_smooth(method = "lm", se = TRUE)

m <- lm(formula = eficiencia ~ tiempo, data = ejercicio10c)

summary(m)


a <- signif(coef(m)[1], digits = 2)
b <- signif(coef(m)[2], digits = 2)
textlab <- paste("y = ",b,"x + ",a, sep="")

r1 <- p3 + geom_text(aes(x = 20, y = 70, label = textlab), color="black", size=3, parse = FALSE)

r2 <- p3 + annotate("text", x = 20, y = 70, label = textlab, color="black", size = 3, parse=FALSE)

r2

## PUNTO C ##
## Utiliza la ecuaci�n estimada de regresi�n para predecir 
## la eficiencia de extracci�n cuando el tiempo de extracci�n es de 35 minutos.

## PUNTO D ##
## Prueba la hip�tesis de que:
## H0 : ??1 = 0 en oposici�n a Ha: ??1 ??? 0. 

summary(m)
confint(m)





