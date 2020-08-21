# De manera individual, pregunta a 15 o 20 compañeros la siguiente información:
# Su género (hombre o mujer)
# Su estatura en centímetros
# Su peso en kilogramos

library(readxl)

act1 <- read_excel("C:/Users/naim_/Downloads/act1.xlsx")

# Organiza los datos recolectados en una tabla de frecuencia. 

table(act1$edad)

table(act1$estatura, act1$peso)

table(act1$genero, act1$peso)

table(act1$genero)

table1 <- table(act1$genero)

prop.table(x=table1)

table(act1$genero, act1$equipo)

table2 <- table(act1$genero, act1$equipo)

prop.table(x=table2)

summary(act1)

## histograma de peso-mujeres, histograma de peso-hombres

library(ggplot2)
library(plotly)

ggplot(act1, aes(x=peso)) + geom_histogram()


ggplot(data = act1,
             mapping = aes(x = peso,
                           fill = factor(genero))) +
  geom_histogram(bins = 9,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Peso',
       fill = 'vs genero',
       x = 'peso',
       y = 'conteos',
       subtitle = 'Histograma',
       caption = 'Fuente: alumnos del Tecmilenio')

g1 <- ggplot(data = act1,
             mapping = aes(x = peso,
                           fill = factor(genero))) +
  geom_histogram(bins = 9,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Peso',
       fill = 'vs genero',
       x = 'peso',
       y = 'conteos',
       subtitle = 'Histograma',
       caption = 'Fuente: alumnos del Tecmilenio')

ggplotly(g1)


ggplot(act1, aes(x=peso, y=estatura)) + geom_point()

ggplot(act1, aes(x=peso, y=estatura)) + 
  geom_point(aes(size=genero))

g2 <- ggplot(act1, aes(x=peso, y=estatura)) + 
  geom_point(aes(size=genero))

ggplotly(g2)


ggplot(act1, aes(x=peso, y=estatura)) + 
  geom_point(aes(size=jugador))

g3 <- ggplot(act1, aes(x=peso, y=estatura)) + 
  geom_point(aes(size=jugador))

ggplotly(g3)


