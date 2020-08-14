## Podemos cargar los datos inmobiliarios con la libreria de readxl

install.packages("readxl")

## Para poder cargar los datos necesitamos llamar la libreria con la opcion library(readxl)

library(readxl)

## Cargamos el archivo desde nuestro directorio y le ponemos el nombre de inmobiliarios

inmobiliarios <- read_excel("C:/Users/naim_/Downloads/inmobiliarios.xlsx")

## head y tail sirve para mostrar algunos de los datos

head(inmobiliarios)

tail(inmobiliarios)

## colnames sirve para mostrar los nombres de las variables

colnames(inmobiliarios)

## Algunos estadisticos descriptivos como media, mediana

# Para todas las variables: media, mediana

summary(inmobiliarios)

# Para una variable en especifico, especificamos el conjunto de datos inmobiliarios y con el signo $ especificamos la variable

summary(inmobiliarios$Ventas)

# Desviacion estandar

sd(inmobiliarios$Ventas)

# Sumatorias

sum(inmobiliarios$ofertatotal)

## Algunos gráficos

boxplot(inmobiliarios$ofvendida, col = "red", main = "Oferta Vendida por los desarrollos", xlab = "Oferta Vendida",
        ylimit = c(0,300)) 

## Tipo de vivienda

table(inmobiliarios$Tipología)

pie(table(inmobiliarios$Tipología)) 

## Segmento

table(inmobiliarios$Segmento)

pie(table(inmobiliarios$Segmento)) 

pie(table(inmobiliarios$Segmento), col = c("red", "blue", "green", "grey", "yellow"))  

pie(table(inmobiliarios$Segmento),  col = c("red", "blue", "green", "grey", "yellow"),
    main = "Tipos de segmentos") 

barplot(table(inmobiliarios$Tipología)) 

barplot(table(inmobiliarios$Tipología), xlab = "Tipología", ylab = "frecuencias",
        main = "Tipo de vivienda")

## Diagrama de dispersion

plot(inmobiliarios$Ventas, inmobiliarios$ofertatotal, pch = 2, col = "blue", main = "oferta vs ventas")



# Correlación 

cor.test(inmobiliarios$Ventas, inmobiliarios$ofertatotal)

cor.test(inmobiliarios$PrecioMAX, inmobiliarios$tamanoconst)

## Ahora vamos a usar la libreria de ggplot2 para gráficas mas elegantes

install.packages("ggplot2")

library(ggplot2)

ggplot(data=inmobiliarios, aes(oferta, Ventas)) + geom_point()

ggplot(data=inmobiliarios, aes(oferta, Ventas)) +
  geom_point() + 
  labs(subtitle = "Oferta vs Ventas", 
       y = "Ventas", 
       x = "Oferta", 
       title = "Oferta en relacion a ventas", 
       caption = "Fuente: Naim Manriquez") 

ggplot(data=inmobiliarios, aes(oferta, Ventas)) + geom_point() + stat_smooth()

ggplot(data=inmobiliarios, aes(oferta, Ventas,color=Segmento)) +
  geom_point() 

ggplot(data=inmobiliarios, aes(oferta, Ventas)) +
  geom_point() + # los puntos
  stat_smooth() + # líneas y bandas de suavizado (smooth)
  facet_wrap(~ Segmento) # los segmentos van en gráficos distintos

## Librería Plotly: Gráficos interactivos y más llamativos

install.packages("plotly")

library(plotly)

plot_ly(data = inmobiliarios, x = ~oferta, y = ~Ventas)

plot_ly(data = inmobiliarios, x = ~oferta, y = ~Ventas, color = ~Segmento)