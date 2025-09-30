library(rio)
library(car)
library(lsr)
library(psych)
library(tidyverse)
library(ggplot2)
library(rstatix)
library(sjstats)
library(pwr)
library(lsmeans)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(interactions)
library(lme4)
library(ggplot2)
library(lmerTest)
rm(list = ls())


heck<-read.csv("heck2011.csv")

names(heck)


##### *** Modelo nulo: estimando una media para cada colegio  *** ##### 

modelo_0<-lmer(math ~ 1 + (1 | schcode), data = heck)
summary(modelo_0)



##### *** Modelo 1: media como resultados. Metemos una variable explicativa (publico o privado) en el nivel 2  *** ##### 

modelo_1<-lmer(math ~ 1 + public + (1 | schcode), data = heck)
summary(modelo_1)

#Comparamos el ajuste de este modelo con respecto al modelo nulo
anova(modelo_0, modelo_1)




##### *** Modelo 2: media como resultados. Metemos una variable explicativa (publico o privado) en el nivel 2 
# y otra variable explicativa a nivel 1 : SES *** ##### 

modelo_2<-lmer(math ~ 1 + ses + public + (1 | schcode), data = heck)
summary(modelo_2)

#Comparamos el ajuste de este modelo con respecto al modelo 1
anova(modelo_1, modelo_2)


##### *** Modelo 3: Pendiente como resultados. Metemos una variable explicativa en el nivel 1 : SES, y dejamos
#que la relacion entre SES y puntuacion en matematicas varie entre colegios*** ##### 

modelo_3<-lmer(math ~ 1 +  ses +( ses | schcode), data = heck)
summary(modelo_3)

#Covarianza etre los efectos aleatorios
Matrix::bdiag(VarCorr(modelo_3))


#Vamos a comparar el ajuste del modelo que asume una relaci?n constante para todos los colegios entre SES y math (modelo_3_1),
# y el ajuste del  modelo_3, que permite que la relaci?n entre SES y math sea diferente para cada colegio
modelo_3_1<-lmer(math ~ 1 +  ses +( 1 | schcode), data = heck)
summary(modelo_3_1)

anova(modelo_3_1, modelo_3)


#### **** Modelo 4:
#Centramos la variable % de estudiantes que quieren ir a la univerdad (pro4yrc)
heck$c_pro4yrc<-scale(heck$pro4yrc, center = TRUE, scale = FALSE)

#Ajustamos el modelo
modelo_4<-lmer(math ~ 1 +  ses + c_pro4yrc + ses:c_pro4yrc + ( ses | schcode), data = heck)
summary(modelo_4)

#Representamos la interaccion
interact_plot(modelo_4, pred = c_pro4yrc, modx = ses)

#Intercepto que la media para una edad 0 es de...