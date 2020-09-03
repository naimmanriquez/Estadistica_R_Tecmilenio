library(readxl)
incidencia <- read_excel("C:/Users/naim_/Desktop/Curso_R/incidencia.xlsx")

### Estadisticos descriptivos

summary(incidencia)

### Sumar total para delitos en toda la serie de tiempo
## Totales
sum(incidencia$Total)

# Desviacion estandar

sd(incidencia$Total)

## Robo a casa habitación

sum(incidencia$RoboCH)

# Desviacion estandar

sd(incidencia$RoboCH)

library(plotly)
library(ggplot2)

ggplot(data=incidencia, aes(Fecha, Total)) + geom_line() +
  ggtitle("Delitos en Mazatlán") +
  ylab("Número de delitos") +
  xlab("Fecha") + 
  theme(plot.title = element_text(hjust = 0.4))

        
ggplot(data=incidencia, aes(Fecha, Total)) + geom_point() +
  ggtitle("Delitos en Mazatlán") +
  ylab("Número de delitos") +
  xlab("Fecha") + 
  theme(plot.title = element_text(hjust = 0.4))

g <- ggplot(data=incidencia, aes(Fecha, Total)) + geom_point() +
ggtitle("Delitos en Mazatlán") +
  ylab("Número de delitos") +
  xlab("Fecha") + 
  theme(plot.title = element_text(hjust = 0.4))

ggplotly(g)

summary(incidencia$Total)

g1 <- g + geom_hline(yintercept = 329)

g1


ggplot(data=incidencia, aes(Fecha, Total)) + geom_point() + stat_smooth() +
  ggtitle("Delitos en Mazatlán") +
  ylab("Número de delitos") +
  xlab("Fecha") + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data=incidencia, aes(Fecha, RoboCH)) + geom_point() + stat_smooth() +
  ggtitle("Total de robos a casa habitación") +
  ylab("Número de delitos") +
  xlab("Fecha") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=incidencia, aes(Fecha, RoboComercio)) + geom_point() + stat_smooth() +
  ggtitle("Total de robos a comercio") +
  ylab("Número de delitos") +
  xlab("Fecha") + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data=incidencia, aes(Fecha, RoboTR)) + geom_point() + stat_smooth() +
  ggtitle("Total de robos a transeunte") +
  ylab("Número de delitos") +
  xlab("Fecha") + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data=incidencia, aes(Fecha, RoboVE)) + geom_point() + stat_smooth() +
  ggtitle("Total de robos a vehiculo estacionado") +
  ylab("Número de delitos") +
  xlab("Fecha") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=incidencia, aes(Fecha, RoboVV)) + geom_point() + stat_smooth() +
  ggtitle("Total de robos violento a vehiculo") +
  ylab("Número de delitos") +
  xlab("Fecha") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(incidencia, aes(Fecha, Total)) +
  geom_point(na.rm=TRUE, color="blue", size=3, pch=18)


TotalCH <- ggplot(incidencia, aes(Fecha, RoboCH)) +
  geom_point(na.rm=TRUE, color="purple", size=1) + 
  ggtitle("Robos a casa habitación en Mazatlán, 2017 a 2020") +
  xlab("Fecha") + ylab("Total de robos")

TotalCH



library(gridExtra)


grid.arrange(g, g1, ncol=1)

library(corrplot)

correlacion <- incidencia[, c(3,4,5,6,7,8)]
M <- cor(correlacion)
head(round(M,2))
corrplot(M, method="circle")
corrplot(M, method="pie")
corrplot(M, method="color")
corrplot(M, method="number")
corrplot(M, type="upper")
corrplot(M, type="lower")
corrplot(M, type="upper", order="hclust")


cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}


p.mat <- cor.mtest(M)
head(p.mat[, 1:5])

corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.10)

corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.10, insig = "blank")








