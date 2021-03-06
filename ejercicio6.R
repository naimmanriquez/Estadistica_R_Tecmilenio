## Parte 1

## El gerente de un banco est� interesado en reducir el tiempo que las personas 
## esperan para ver a su asesor financiero. Tambi�n le interesa la relaci�n 
## entre el tiempo de espera (Y) en minutos y el n�mero de asesores atendiendo (X). 
## Se registraron los siguientes datos:

library(readxl)

ejercicio6a <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio6a.xlsx")

View(ejercicio6a)

## a) Calculen el coeficiente de correlaci�n.

cor(ejercicio6a$x, ejercicio6a$y)

## b) Interpreta tus resultados.

## Hay una correlaci�n negativa: entre mas asesores, menos minutos de espera

library(ggplot2)
library(ggthemes)

ggplot(data = ejercicio6a, aes(x=x, 
                                  y=y)) +
  geom_point(size=3) +
  labs(x = "N�mero de asesores atendiendo",
       y = "Minutos de espera") + theme_economist()

## c) Calcula la media, varianza y desviaci�n est�ndar de cada variable 
## e interpreta tus resultados.

summary(ejercicio6a$x)
var(ejercicio6a$x)
sd(ejercicio6a$x)

summary(ejercicio6a$y)
var(ejercicio6a$y)
sd(ejercicio6a$y)

rm(list=ls())

### PARTE 2

## Una empresa refresquera est� estudiando el efecto de su �ltima 
## campa�a publicitaria. Se eligieron personas al azar y se les llam� 
## para preguntarles cuantas latas de su refresco hab�an comprado 
## la semana anterior y cu�ntos anuncios de su refresco hab�an 
## le�do o visto durante el periodo. Los datos se presentan a continuaci�n:

library(readxl)

ejercicio6b <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio6b.xlsx")

View(ejercicio6b)

## a) Determina el coeficiente de correlaci�n.

cor(ejercicio6b$x, ejercicio6b$y)

## b) Interpreta los resultados del inciso a).

## c) �Est�s de acuerdo con el planteamiento de las variables del problema? 
## �Qu� suceder�a con la interpretaci�n si X es el n�mero de latas compradas 
## y Y el n�mero de anuncios? 
## �Tendr�a sentido este �ltimo planteamiento?, �s� o no?, �por qu�?

## d) Realiza un gr�fico de dispersi�n para el problema inicial 
## e interpreta el mismo de manera detallada.

library(ggplot2)
library(ggthemes)

ggplot(data = ejercicio6b, aes(x=x, 
                               y=y)) +
  geom_point(size=3) +
  ggtitle("Gr�fico variables x y") +
  ylab("Latas") +
  xlab("Anuncios") + theme_economist_white(gray_bg = FALSE) 

rm(list=ls())

### PARTE 3

## El siguiente conjunto de datos son las ventas semanales de un art�culo 
## de comida (en miles). Determinen el coeficiente de autocorrelaci�n 
## y prueben la hip�tesis de que
## Hip�tesis nula: no hay aytocorrelacion
## Hip�tesis alternativa: hay autocorrelacion

## Utilicen ?? = 0.05 y un ?? = 0.01.

## a) Compara ambos resultados y realiza una conclusi�n de los mismos.
## b) �Es relevante emplear esta serie de tiempo para realizar pron�sticos?

library(readxl)

ejercicio6c <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/ejercicio6c.xlsx")

View(ejercicio6c)

ts <- ts(ejercicio6c, frequency=52, start=c(2020,1))

plot(ts)

print(ts)

autocorrelacion<-acf(ts, type ="correlation", plot = FALSE)

plot(autocorrelacion)

Box.test(ts, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 1)







