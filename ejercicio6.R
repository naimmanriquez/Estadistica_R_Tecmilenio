## Parte 1

## El gerente de un banco está interesado en reducir el tiempo que las personas 
## esperan para ver a su asesor financiero. También le interesa la relación 
## entre el tiempo de espera (Y) en minutos y el número de asesores atendiendo (X). 
## Se registraron los siguientes datos:

library(readxl)

ejercicio6a <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio6a.xlsx")

View(ejercicio6a)

## a) Calculen el coeficiente de correlación.

cor(ejercicio6a$x, ejercicio6a$y)

## b) Interpreta tus resultados.

## Hay una correlación negativa: entre mas asesores, menos minutos de espera

library(ggplot2)
library(ggthemes)

ggplot(data = ejercicio6a, aes(x=x, 
                                  y=y)) +
  geom_point(size=3) +
  labs(x = "Número de asesores atendiendo",
       y = "Minutos de espera") + theme_economist()

## c) Calcula la media, varianza y desviación estándar de cada variable 
## e interpreta tus resultados.

summary(ejercicio6a$x)
var(ejercicio6a$x)
sd(ejercicio6a$x)

summary(ejercicio6a$y)
var(ejercicio6a$y)
sd(ejercicio6a$y)

rm(list=ls())

### PARTE 2

## Una empresa refresquera está estudiando el efecto de su última 
## campaña publicitaria. Se eligieron personas al azar y se les llamó 
## para preguntarles cuantas latas de su refresco habían comprado 
## la semana anterior y cuántos anuncios de su refresco habían 
## leído o visto durante el periodo. Los datos se presentan a continuación:

library(readxl)

ejercicio6b <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio6b.xlsx")

View(ejercicio6b)

## a) Determina el coeficiente de correlación.

cor(ejercicio6b$x, ejercicio6b$y)

## b) Interpreta los resultados del inciso a).

## c) ¿Estás de acuerdo con el planteamiento de las variables del problema? 
## ¿Qué sucedería con la interpretación si X es el número de latas compradas 
## y Y el número de anuncios? 
## ¿Tendría sentido este último planteamiento?, ¿sí o no?, ¿por qué?

## d) Realiza un gráfico de dispersión para el problema inicial 
## e interpreta el mismo de manera detallada.

library(ggplot2)
library(ggthemes)

ggplot(data = ejercicio6b, aes(x=x, 
                               y=y)) +
  geom_point(size=3) +
  ggtitle("Gráfico variables x y") +
  ylab("Latas") +
  xlab("Anuncios") + theme_economist_white(gray_bg = FALSE) 

rm(list=ls())

### PARTE 3

## El siguiente conjunto de datos son las ventas semanales de un artículo 
## de comida (en miles). Determinen el coeficiente de autocorrelación 
## y prueben la hipótesis de que
## Hipótesis nula: no hay aytocorrelacion
## Hipótesis alternativa: hay autocorrelacion

## Utilicen ?? = 0.05 y un ?? = 0.01.

## a) Compara ambos resultados y realiza una conclusión de los mismos.
## b) ¿Es relevante emplear esta serie de tiempo para realizar pronósticos?

library(readxl)

ejercicio6c <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio6c.xlsx")

View(ejercicio6c)

ts <- ts(ejercicio6c, frequency=52, start=c(2020,1))

plot(ts)

print(ts)

autocorrelacion<-acf(ts, type ="correlation", plot = FALSE)

plot(autocorrelacion)

Box.test(ts, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 1)







