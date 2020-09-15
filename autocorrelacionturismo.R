library(readxl)
turismo <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/turismomaza.xlsx")
View(turismo)

turismo.ts = ts(turismo$turistas, start = c(2006,1), frequency = 12)
plot(turismo.ts)

library(ggplot2)
library(plotly)
library(ggthemes)
library(scales)

ggplot(data=turismo, aes(periodo, turistas)) + geom_line() +
  ggtitle("Llegada de turistas a Mazatlán desde 2006 a 2020") +
  ylab("Turistas (miles de personas)") +
  xlab("Periodo") + 
  theme(plot.title = element_text(hjust = 0.4)) 

g <- ggplot(data=turismo, aes(periodo, turistas)) + geom_line(color = "steelblue") +
  ggtitle("Llegada de turistas a Mazatlán desde enero 2006 a junio 2020") +
  ylab("Turistas (miles de personas)") +
  xlab("Periodo") + 
  labs(caption = "Laboratorio de Políticas Públicas - Mazatlán")
  theme(plot.title = element_text(hjust = 0.4)) 

g + theme_economist_white() + scale_colour_economist()

g + theme_economist_white() +
  scale_colour_economist()

g + theme_economist_white(gray_bg = FALSE) +
  scale_colour_economist()

print(turismo.ts)

dif1.x = diff(turismo.ts)
plot(dif1.x)

serie_estacionaria<-dif1.x

serie_estacionaria

autocorrelacion<-acf(serie_estacionaria, type ="correlation", plot = FALSE)

plot(autocorrelacion)

Box.test(serie_estacionaria, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)



