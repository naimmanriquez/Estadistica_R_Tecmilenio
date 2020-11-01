library(readxl)

logistico_reg <- read_excel("C:/Users/naim_/Downloads/logistico_reg.xlsx")

View(logistico_reg)

attach(logistico_reg)

# Definir variables
Y <- cbind(seguro)
X <- cbind(jubilado, edad, estatus, ingreso, educa, casado, hispano)

# Estadisticos descriptivos
summary(Y)
summary(X)

table(Y)
table(Y)/sum(table(Y))

# Coeficientes regresion logistica
logit<- glm(Y ~ X, family=binomial (link = "logit"))
summary(logit) 

# Efectos marginales
LogitScalar <- mean(dlogis(predict(logit, type = "link")))
LogitScalar * coef(logit)

