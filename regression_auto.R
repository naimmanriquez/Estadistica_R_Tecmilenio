### REGRESION LINEAL 
# Dr. Naim Manriquez

library(readxl)

regression_auto <- read_excel("C:/Users/naim_/Downloads/regression_auto.xlsx")

attach(regression_auto)

# Definir variables

Y <- cbind(mpg)
X1 <- cbind(peso1)
X <- cbind(peso1, precio, importado)

# Estadisticos descriptivos
summary(Y)
summary(X)

# Matriz de correlacion
cor(Y, X)

# Grafico de dispersion 
plot(Y ~ X1, data = regression_auto)

# Regresión lineal con una sola variable 
olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level=0.95)
anova(olsreg1)

# Grafico de regresion
abline(olsreg1)

# Prediccion de valores de la variable dependiente
Y1hat <- fitted(olsreg1)
summary(Y1hat)
plot(Y1hat ~ X1)

# Residuos
e1hat <- resid(olsreg1)
summary(e1hat)
plot(e1hat ~ X1)

# Regresion lineal multiple
olsreg2 <- lm(Y ~ X)
summary(olsreg2)
confint(olsreg2, level=0.95)
anova(olsreg2)

# Prediccion de valores para variable dependiente
Yhat <- fitted(olsreg2)
summary(Yhat)

# Residuos
ehat <- resid(olsreg2)
summary(ehat)

