library(rio)
library(tidyverse)
library(ggplot2)
library(caret)
library(performance)
library(rcompanion)
library(gtsummary)
library(lmtest)

data<-import("datatab.xlsx")


#Inspección de los datos
data %>% 
  tbl_cross(row = Smoker_status, col = Disease, percent = "cell") 

#Modelo 1: Fumador como covariable
modelo1 <- glm( Disease ~ Smoker_status, data = data, family = binomial)
summary(modelo1)

#Predicciones del modelo: probabilidad de desarrollar la enfermedad siendo fumador
newdata <- list(Smoker_status = 1)
#Prediccion (en escala logaritmica)
my_log_odds <- predict(modelo1, newdata = newdata)
#Prediccion (en ratio)
my_odds <- exp(my_log_odds)
#Prediccion (en pobabilidad)
(my_prob <- my_odds / (1 + my_odds))


#Predicciones del modelo: probabilidad de desarrollar la enfermedad no siendo fumador
newdata <- list(Smoker_status = 0)
#Prediccion (en escala logaritmica)
my_log_odds <- predict(modelo1, newdata = newdata)
#Prediccion (en odds)
my_odds <- exp(my_log_odds)
#Prediccion (en probabilidad)
(my_prob <- my_odds / (1 + my_odds))


#Obtenemos los coeficientes en escala odds y odds ratio
exp(coef(modelo1))


#Tabla de clasificación (matrix de confusión)

#Primero guardamos las predicciones del modelo (la probabilidad de que pertenezcan
# a la categoría 1 o a la 0)
glm.probs = predict(modelo1, newdata=data, type="response")
#Después creamos otro vector donde se le asigne "Yes" (o 1) a las observaciones con
#una probailidad mayor a 0.50 y "No" (o 0) a las observaciones con una probabilidad
#menor a 0.5
glm.predict = ifelse(glm.probs>0.5,"Yes","No")

#Por último, creamos una tabla con los datos reales y los pronósticos. En la
#diagonal tenemos los datos que han sido clasificados correctamente.
table(glm.predict, data$Disease)

#Porcentaje correctamente clasificado (se suman las diagonales y se divide el total)
(10+14)/(10+6+6+14)



#Test de razon de verosimilitudes
modelo0<-glm( Disease ~ 1, data = data, family = binomial)
lrtest(modelo0, modelo1)

# R2
nagelkerke(modelo1, null = NULL, restrictNobs = FALSE)


#Modelo 2: Fumado, edad, y género como covariables

#Centramos las variables

data$c_edad<-data$Age - mean(data$Age, na.rm=TRUE)

modelo2<-glm(Disease ~ Smoker_status+ c_edad + Gender, data = data, family = binomial)
summary(modelo2)

#Predicciones del modelo: probabilidad de desarrollar la enfermedad siendo
#hombre fumador de edad media siendo fumador
newdata <- list(Smoker_status = 1, c_edad =0, Gender=0)
#Prediccion (en escala logaritmica)
my_log_odds <- predict(modelo2, newdata = newdata)
#Prediccion (en ratio)
my_odds <- exp(my_log_odds)
#Prediccion (en pobabilidad)
(my_prob <- my_odds / (1 + my_odds))

#Predicciones del modelo: probabilidad de desarrollar la enfermedad siendo
#mujer no fumadora de edad media 
newdata <- list(Smoker_status = 0, c_edad =0, Gender=1)
#Prediccion (en escala logaritmica)
my_log_odds <- predict(modelo2, newdata = newdata)
#Prediccion (en ratio)
my_odds <- exp(my_log_odds)
#Prediccion (en pobabilidad)
(my_prob <- my_odds / (1 + my_odds))

#Obtenemos los coeficientes en escala odds y odds ratio
exp(coef(modelo2))



#Tabla de clasificación (matrix de confusión)

#Primero guardamos las predicciones del modelo (la probabilidad de que pertenezcan
# a la categoría 1 o a la 0)
glm.probs = predict(modelo2, newdata=data, type="response")
#Después creamos otro vector donde se le asigne "Yes" (o 1) a las observaciones con
#una probailidad mayor a 0.50 y "No" (o 0) a las observaciones con una probabilidad
#menor a 0.5
glm.predict = ifelse(glm.probs>0.5,"Yes","No")

#Por último, creamos una tabla con los datos reales y los pronósticos. En la
#diagonal tenemos los datos que han sido clasificados correctamente.
table(glm.predict, data$Disease)

#Porcentaje correctamente clasificado (se suman las diagonales y se divide el total)
(11+15)/(11+5+5+15)


#Ajuste del modelo
# Test de razón de verosimilitudes
lrtest(modelo1, modelo2)

# R2
nagelkerke(modelo2, null = NULL, restrictNobs = FALSE)


######  ***   Chequeo de supuestos    **** ###### 

#Linealidad entre logit (Y=1) y las variables continuas (seleccionamos solo a la variable edad, que es la única continua) 

#Añadimos una columna con las probabilidades pronosticadas para cada participante
data$glm.probs<-glm.probs
#Hallamos el logaritmo
data$logit<-ln(data$glm.probs/(1-data$glm.probs))


library(SciViews) #Para poder hallar logaritmos

ggplot(data, aes(c_edad,logit))+
  geom_point(size = 1) +
  geom_smooth(method = "lm", formula = "y~x") +
  theme_bw() 
 

#Colinealidad 
car::vif(modelo2)

#Independencia de los residuos (tiene más sentidos con variables continuas)

plot(data$c_edad, residuals(modelo2))
plot(data$Smoker_status, residuals(modelo2))

