library(readxl)
residuos <- read_excel("C:/Users/naim_/Desktop/Curso_R/Series Temporales/residuos.xlsx")
View(residuos)

residuos.ts = ts(residuos$residuos, start = c(1992), frequency = 1)
plot(residuos)

library(ggplot2)
library(plotly)

ggplot(data=residuos, aes(periodo, residuos)) + geom_line() +
  ggtitle("Cantidad de residuos en millones de toneladas") +
  ylab("Periodo") +
  xlab("Residuos") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw()

print(residuos.ts)

x = log(residuos.ts)
plot(x)

dif1.x = diff(x)
plot(dif1.x)

serie_estacionaria<-dif1.x

serie_estacionaria

autocorrelacion<-acf(serie_estacionaria, type ="correlation", plot = FALSE)

autocorrelacion

plot(autocorrelacion)

Box.test(x, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)



