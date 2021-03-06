library(ggplot2)
library(ggthemes)
library(readxl)
library(dplyr)
library(scales)

### EJERCICIO 9 ###

### PARTE 1 ### 
## Realizar un resumen sobre el m�todo de m�nimos cuadrados, 
## donde se explique la raz�n por la cual se le denomina as�.

### PARTE 2 ## 
## Resuelve el siguiente ejercicio:

## Una empresa de bienes ra�ces ha recopilado datos para ayudar a determinar 
## c�mo el n�mero de ventas de viviendas en la regi�n est� relacionado 
## con los niveles de tasas de inter�s de la hipoteca. 
## En la siguiente tabla se muestran el n�mero de viviendas vendidas 
## en la regi�n y las tasas de inter�s de la hipoteca para 12 meses, 
## seleccionados al azar.

ejercicio9 <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio9.xlsx")


### PUNTO A ###
## Realiza un diagrama de dispersi�n para estos datos con 
## el n�mero de casas vendidas en el eje vertical.

ggplot(ejercicio9, aes(x=interes, y=viviendas, size = interes, color=zona)) +
  geom_point(alpha=0.6) +
  scale_size(range = c(.1, 12), name="Tasa de interes") +
  theme_solarized_2()


## PUNTO B ##
## Describe la relaci�n entre el inter�s de la hipoteca 
## y el n�mero de viviendas vendidas.


## PUNTO C ##
## Determina la recta de regresi�n que describa como las tasas de inter�s 
## (X) afectan el n�mero de viviendas vendidas (Y). 
## �Qu� indica el coeficiente de regresi�n acerca de esta relaci�n?



p1 <- ggplot(ejercicio9, aes(x = interes, y = viviendas))
p1 + geom_point()
p3 <- p1 + geom_point(color="red") + geom_smooth(method = "lm", se = TRUE)

m <- lm(formula = viviendas ~ interes, data = ejercicio9)

summary(m)


a <- signif(coef(m)[1], digits = 2)
b <- signif(coef(m)[2], digits = 2)
textlab <- paste("y = ",b,"x + ",a, sep="")

r1 <- p3 + geom_text(aes(x = 11, y = 207, label = textlab), color="black", size=3, parse = FALSE)

r2 <- p3 + annotate("text", x = 11, y = 207, label = textlab, color="black", size = 3, parse=FALSE)

r2

## PUNTO D
## Pronostica el n�mero de viviendas vendidas si la tasa de inter�s es del 10%


## PUNTO E

## Determina el coeficiente de correlaci�n.

library(ggpubr)

ggscatter(ejercicio9, x = "interes", y = "viviendas", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Interes", ylab = "Viviendas vendidas")

