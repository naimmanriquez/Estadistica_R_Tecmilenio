## Parte I. Recolección de la información.

## Pregunta a 10 hombres y a 10 mujeres la siguiente información, 
## de manera individual:
## Su edad
## Tiempo que dedica diariamente a Internet

library(readxl)

usointernet <- read_excel("C:/Users/naim_/Downloads/usointernet.xlsx")

View(usointernet)

## Parte II. Obtención de resultados.

## Para los datos en general, determina el promedio de tiempo dedicado a Internet.

summary(usointernet$internet)

## Para el total de datos, 
## determina la varianza y la desviación estándar 
## del tiempo que dedican al uso de Internet.

var(usointernet$internet)

sd(usointernet$internet)

## Para los datos por género, 
## determina en promedio quién dedica más tiempo a Internet: 
## hombres o mujeres.


library(psych)

describeBy(usointernet$internet, usointernet$genero) 

library(ggplot2)

ggplot(data=usointernet, aes(edad, internet, col = genero)) + geom_point() +
  ggtitle("Edad vs Uso de Internet") +
  ylab("Uso de Internet") +
  xlab("Edad") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw()


## Para los hombres, calcula el coeficiente de correlación lineal 
## entre la edad y el tiempo dedicado al uso de Internet.

library(plyr)

ddply(usointernet, .(genero), summarise, "corr" = cor(edad, internet, method = "spearman"))

## Para los datos por género: 
## determina la mediana de la edad y del tiempo dedicado a Internet.

describeBy(usointernet$edad, usointernet$genero) 

## Parte III. Prueba de hipótesis

## Imagina que el promedio que dedica una persona a Internet (sin importar su género) es de 7 horas diarias. Con los datos anteriores, prueba las siguientes hipótesis:
  
## H0: µ = 7 contra la alternativa de que 
## Ha : µ ??? 7 con un nivel de significancia de 0.05.

sol.test=t.test(usointernet$internet,mu=7.0,alternative="two.sided",conf.level=0.95)

sol.test



