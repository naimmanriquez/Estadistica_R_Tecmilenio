## ACTIVIDAD 3 Opcion 1 ##

## EJERCICIO 1 ##

## Define lo que significan los siguientes t�rminos:
## Correlaci�n
## Autocorrelaci�n
## Promedio m�vil
## Suavizamiento exponencial


## EJERCICIO 2 ##

## Busca informaci�n de 20 casas en venta en donde las variables sean
## Y (metros de construcci�n) y X (metros de terreno); 
## lleva a cabo lo que se indica:

library(readxl)

aguascalientes <- read_excel("C:/Users/naim_/Downloads/aguascalientes.xlsx")

View(aguascalientes)

## Realiza y describe el diagrama de dispersi�n

library(ggplot2)
library(ggpubr)

ggplot(data = aguascalientes, aes(x=MT2, 
                                  y=MC2)) +
  geom_point(size=3) +
  ggtitle("Gr�fico entre metros de construcci�n y terreno") +
  labs(x = "Metros de terreno",
       y = "Metros de construcci�n") +
  theme_bw()

## Calcula e interpreta el coeficiente de correlaci�n muestral r.

cor(aguascalientes$MC2, aguascalientes$MT2)    

ggscatter(aguascalientes, x = "MT2", y = "MC2",
          color = "black", shape = 21, size = 3,
          add = "reg.line",  
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n"))

## Responde a la siguiente cuesti�n en un terreno urbano. 
## �A mayor cantidad en metros de construcci�n, mayor es el precio de la vivienda?

ggplot(data=aguascalientes, aes(MC2, PrecioMXN)) + geom_point() + stat_smooth() +
  ggtitle("Correlacion entre metros de construcci�n y precio") +
  ylab("Precio") +
  xlab("MC2") + 
  theme(plot.title = element_text(hjust = 0.5))


ggscatter(aguascalientes, x = "MC2", y = "PrecioMXN",
          color = "black", shape = 21, size = 3,
          add = "reg.line",  
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n"))

### EJERCICIO 3 ##

## Busca informaci�n de los cetes a 28 d�as- semanal, 
## periodicidad diaria y datos del Banco de M�xico. 
## Considera las �ltimas 20 cotizaciones de los cetes y realiza lo que se indica:

## Determina el coeficiente de autocorrelaci�n r1
## Determina la prueba de hip�tesis de lo siguiente:
## Hip�tesis nula: H0 : ??1 = 0 (La autocorrelaci�n es igual a cero).
## Hip�tesis alternativa: Ha: ??1??? 0 (La  autocorrelaci�n es diferente de cero).
## Donde ??k es el coeficiente de autocorrelaci�n poblacional en el lapso k.
## �Existe autocorrelaci�n entre los rendimientos de los CETES  a 28 d�as?

## EJERCICIO 4 ##

## Las llamadas de emergencia a un tel�fono 
## durante las �ltimas 24 semanas son �stas:

library(readxl)

llamadas <- read_excel("C:/Users/naim_/Downloads/llamadas.xlsx")

View(llamadas)

## Realiza y describe un diagrama de dispersi�n.

library(ggplot2)

ggplot(data = llamadas, aes(x=semana, y=llamadas)) +
  geom_point(size=3) +
  ggtitle("Gr�fico de dispersi�n") +
  labs(x = "Semana",
       y = "Llamadas") +
  theme_bw()

## Determina un promedio m�vil con k=3 periodos y pronostica el valor para la semana 25.

library(forecast)

llamadastimeseries <- ts(llamadas$llamadas)

moving_average = forecast(ma(llamadastimeseries[4:24], order=3), h=2)

plot(moving_average)

## ULTIMA PARTE ACTIVIDAD

## Con los conceptos vistos y puestos en pr�ctica, 
## brinda una respuesta justificada a cada una de las siguientes cuestiones

## �Qu� significa el coeficiente de correlaci�n?
## �C�mo se interpreta el coeficiente de correlaci�n? 
## �Para qu� sirve el coeficiente de autocorrelaci�n?
## �Cu�ndo utilizar�as el m�todo de promedios m�viles?
## �C�mo elegir�as la constante suavizamiento en el m�todo de suavizaci�n exponencial?


