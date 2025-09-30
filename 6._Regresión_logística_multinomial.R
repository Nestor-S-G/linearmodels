library(rio)
library(tidyverse)
library(ggplot2)
library(caret)
library(performance)
library(rcompanion)
library(gtsummary)
library(lmtest)
library(nnet)

# Pedimos eliminar la notación científica
options(scipen=999)

# Importamos los datos
data<-import("CIS_Salarios.csv")

        
#Inspeccion de los datos
data %>% 
  tbl_cross(row = Genero, col = Ingresos, percent = "cell") 


#Modelo 1: Franja salarial, marcando 1 como categoria de referencia tanto en Ingresos como en Genero.

# Covertimos las variables a factores
data$Ingresos<-as.factor(data$Ingresos)
data$Genero<-as.factor(data$Genero)

data$Ingresos <- relevel(data$Ingresos, ref = "1") # fijamos la categoría de referencia
data$Genero <- relevel(data$Genero, ref = "1") # fijamos la categoría de referencia

modelo1 <- multinom(Ingresos ~ Genero, data = data)
summary(modelo1)

#Obtenemos el estadistico Z y la probabilidad asoaciada al estadistico Z
# Wald test (p-value)
wald <- summary(modelo1)$coefficients/summary(modelo1)$standard.errors
wald
round((p <- (1 - pnorm(abs(wald), 0, 1)) * 2),4) # contraste bilateral

#Obtenemos las odds del modelo
exp(coef(modelo1))

#Cambiamos la categoria de referencia de la variable dependiente para que se refiera a medio
data$Ingresos <- relevel(data$Ingresos, ref = "2") # fijamos la categoria de referencia

modelo11 <- multinom(Ingresos ~ Genero, data = data)
summary(modelo11)

#Obtenemos el estadistico Z y la probabilidad asoaciada al estadistico Z
# Wald test (p-value)
wald <- summary(modelo11)$coefficients/summary(modelo11)$standard.errors
wald
round((p <- (1 - pnorm(abs(wald), 0, 1)) * 2),4) # contraste bilateral

#Obtenemos las odds del modelo
exp(coef(modelo11))


############ *** Predicciones del modelo  ***  ######

#Predicciones para las mujeres
newdata <- data.frame(Genero=2)
newdata$Genero<-as.factor(newdata$Genero)
predict(object=modelo1, newdata=newdata, type="probs")

#Predicciones para las hombres
newdata <- data.frame(Genero=1)
newdata$Genero<-as.factor(newdata$Genero)
predict(object=modelo1, newdata=newdata, type="probs")


#Tabla de clasificacion (matrix de confusion)

#Primero guardamos las predicciones del modelo 
glm.probs = predict(modelo1, newdata=data, type="probs")
probabilidades<-as.data.frame(glm.probs)

#Creamos una nueva columna donde se indique la categoria que tiene una probabilidad mayor

probabilidades$pred<-ifelse(probabilidades$`1` > probabilidades$`2` & probabilidades$`1` > probabilidades$`3`, 1,
       ifelse(probabilidades$`2` > probabilidades$`1` & probabilidades$`2` > probabilidades$`3`, 2, 3))


#Por utimo, creamos una tabla con los datos reales y los pronosticos. En la
#diagonal tenemos los datos que han sido clasificados correctamente.
table(data$Ingresos, probabilidades$pred)

#Porcentaje correctamente clasificado (se suman las diagonales y se divide el total)
(389+445)/(1938) *100


#Test de razon de verosimilitudes
modelo0<-multinom( Ingresos ~ 1, data = data)
lrtest(modelo0, modelo1)

# R2
r2(modelo1)



#Modelo 2: Genero, edad y nivel educativo

#Centramos las variables
data$c_edad<-data$Edad  - mean(data$Edad , na.rm=TRUE)
data$c_estudios<-data$ESTUDIOS   - mean(data$ESTUDIOS  , na.rm=TRUE)

#Fijamos categorias de referencia
data$Ingresos <- relevel(data$Ingresos, ref = "1") # fijamos la categoría de referencia
data$Genero <- relevel(data$Genero, ref = "1") # fijamos la categoría de referencia

modelo2 <- multinom(Ingresos ~ Genero + c_edad + c_estudios, data = data)
summary(modelo2)


#Obtenemos el estadistico Zy la probabilidad asoaciada al estad?stico Z
# Wald test (p-value)

wald <- summary(modelo2)$coefficients/summary(modelo2)$standard.errors
wald
round((p <- (1 - pnorm(abs(wald), 0, 1)) * 2),4) # contraste bilateral


#Obtenemos las odds del modelo
exp(coef(modelo2))



#Modelo 3: Genero y nivel educativo

#Fijamos categorias de referencia

data$Ingresos <- relevel(data$Ingresos, ref = "1") # fijamos la categoría de referencia
data$Genero <- relevel(data$Genero, ref = "1") # fijamos la categoría de referencia

modelo3 <- multinom(Ingresos ~ Genero + c_estudios, data = data)
summary(modelo3)


#Obtenemos el estadistico Z y la probabilidad asoaciada al estadistico Z
# Wald test (p-value)
wald <- summary(modelo3)$coefficients/summary(modelo3)$standard.errors
wald
round((p <- (1 - pnorm(abs(wald), 0, 1)) * 2),4) # contraste bilateral


#Obtenemos las odds del modelo
exp(coef(modelo3))


############ *** PRedicciones del modelo  ***  ######

#Predicciones para las mujeres con nivel de estudios medios
newdata <- data.frame(Genero=2, c_estudios=0)
newdata$Genero<-as.factor(newdata$Genero)
predict(object=modelo3, newdata=newdata, type="probs")

#Predicciones para las hombres con nivel de estudios medios
newdata <- data.frame(Genero=1, c_estudios=0)
newdata$Genero<-as.factor(newdata$Genero)
predict(object=modelo3, newdata=newdata, type="probs")



#Tabla de clasificacion (matrix de confusion)

#Primero guardamos las predicciones del modelo 
glm.probs = predict(modelo3, newdata=data, type="probs")
probabilidades<-as.data.frame(glm.probs)

#Creamos una nueva columna donde se indique la categoria que tiene una probabilidad mayor

probabilidades$pred<-ifelse(probabilidades$`1` > probabilidades$`2` & probabilidades$`1` > probabilidades$`3`, 1,
                            ifelse(probabilidades$`2` > probabilidades$`1` & probabilidades$`2` > probabilidades$`3`, 2, 3))


#Por utimo, creamos una tabla con los datos reales y los pronosticos. En la
#diagonal tenemos los datos que han sido clasificados correctamente.
table(data$Ingresos, probabilidades$pred)

#Porcentaje correctamente clasificado (se suman las diagonales y se divide el total)
(472+576)/(1938) *100


#Test de razon de verosimilitudes
modelo0<-multinom( Ingresos ~ 1, data = data)
lrtest(modelo0, modelo3)
lrtest(modelo1, modelo3)

# R2
r2(modelo3)



# Supuestos #

#Relacion lineal entre el logit y las variables predictoras contonuas

data<-cbind(data, glm.probs)
library(SciViews) #Para poder hallar logaritmos

data$logit1<-ln(data$`2`/(data$`1`))
data$logit2<-ln(data$`3`/(data$`1`))



ggplot(data, aes(c_estudios,logit1))+
  geom_point(size = 1) +
  geom_smooth(method = "lm", formula = "y~x") +
  theme_bw() 

ggplot(data, aes(c_estudios,logit2))+
  geom_point(size = 1) +
  geom_smooth(method = "lm", formula = "y~x") +
  theme_bw()

#Colinealidad 
car::vif(modelo3)


#Independencia de los residuos (tiene más sentidos con variables continuas)
res1<-residuals(modelo3)[,1]
res2<-residuals(modelo3)[,2]
res3<-residuals(modelo3)[,3]

plot(data$c_estudios, res1)
plot(data$c_estudios, res2)
plot(data$c_estudios, res3)


#Sobredispersion
#Desvianza del modelo = 3757.461
#grados de libertad + numero de observaciones en la base de datos - modelo3$edf

3757.461/((nrow(data) - modelo3$edf))

#El resultado es menor que 2, asi que no parece que haya sobredispersion



################# ********   Regresion ordinal ******   #################

library(MASS)

modelo_ord_1 <- polr(Ingresos ~ Genero + c_estudios, data = data, Hess=TRUE)
summary(modelo_ord_1)

#Obtenemos el estadistico Z y la probabilidad asoaciada al estadistico Z
# Wald test (p-value)

wald <- coef(summary(modelo_ord_1))[,"Value"]/coef(summary(modelo_ord_1))[,"Std. Error"]
(p <- (1- pnorm(abs(wald), 0, 1)) * 2)

exp(coef(modelo_ord_1))

