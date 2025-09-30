library(rio)
library(tidyverse)
library(ggplot2)
library(caret)
library(performance)
library(rcompanion)
library(gtsummary)
library(lmtest)
library(nnet)

data <-read.csv("datos_CIS_limpia.csv")

#1

data %>% 
  tbl_cross(row = SEXO, col = IMPIDEAS, percent = "cell") 


#2

data$IMPIDEAS<-as.factor(data$IMPIDEAS)
data$SEXO<-as.factor(data$SEXO)

data$IMPIDEAS <- relevel(data$IMPIDEAS, ref = "1") # referencia 'igualdad social'
data$SEXO <- relevel(data$SEXO, ref = "2")  # referencia 'mujer'

modelo_1 <- multinom(IMPIDEAS ~ SEXO, data = data)
summary(modelo_1)

#Obtenemos el estadistico Z y la probabilidad asoaciada al estadistico Z
# Wald test (p-value)
wald <- summary(modelo_1)$coefficients/summary(modelo_1)$standard.errors
wald
round((p <- (1 - pnorm(abs(wald), 0, 1)) * 2),4) # contraste bilateral

#odds del modelo_1
exp(coef(modelo_1))


#Predicciones para las mujeres
newdata <- data.frame(SEXO=2)
newdata$SEXO<-as.factor(newdata$SEXO)
predict(object=modelo_1, newdata=newdata, type="probs")

#Predicciones para las hombres
newdata <- data.frame(SEXO=1)
newdata$SEXO<-as.factor(newdata$SEXO)
predict(object=modelo_1, newdata=newdata, type="probs")


#Tabla de clasificacion (matrix de confusion)

#Primero guardamos las predicciones del modelo 
glm.probs = predict(modelo_1, newdata=data, type="probs")
probabilidades<-as.data.frame(glm.probs)

#Creamos una nueva columna donde se indique la categoria que tiene una probabilidad mayor

probabilidades$pred <- ifelse(probabilidades$`1` > probabilidades$`2` & probabilidades$`1` > probabilidades$`3`, 1,
                              ifelse(probabilidades$`2` > probabilidades$`1` & probabilidades$`2` > probabilidades$`3`, 2, 3))


#Por utimo, creamos una tabla con los datos reales y los pronosticos. En la
#diagonal tenemos los datos que han sido clasificados correctamente.
table(data$IMPIDEAS, probabilidades$pred)

#Porcentaje correctamente clasificado (se suman las diagonales y se divide el total)
(389+445)/(805) *100



#3

data$c_clasesub<-data$CLASESUB - mean(data$CLASESUB, na.rm=TRUE)

modelo_2 <- multinom(IMPIDEAS ~ SEXO + c_clasesub, data = data)
summary(modelo_2)

#Obtenemos el estadistico Zy la probabilidad asoaciada al estad?stico Z
# Wald test (p-value)

wald <- summary(modelo_2)$coefficients/summary(modelo_2)$standard.errors
wald
round((p <- (1 - pnorm(abs(wald), 0, 1)) * 2),4) # contraste bilateral


#Obtenemos las odds del modelo
exp(coef(modelo_2))


#Predicciones para las mujeres con nivel de estudios medios
newdata <- data.frame(SEXO=2, c_clasesub=0)
newdata$SEXO<-as.factor(newdata$SEXO)
predict(object=modelo_2, newdata=newdata, type="probs")

#Predicciones para las hombres con nivel de estudios medios
newdata <- data.frame(SEXO=1, c_clasesub=0)
newdata$SEXO<-as.factor(newdata$SEXO)
predict(object=modelo_2, newdata=newdata, type="probs")


#4
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
