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

#1-----------------------

datos <-read.csv2("datos_Tarea_2.csv")

#Ðescriptivos
summary(datos$health)
summary(datos$c_gdppc_2021)
summary(datos$agea)

#Histograma
hist(datos$health)
hist(datos$c_gdppc_2021)
hist(datos$agea)

#2------------

modelo_0<-lmer(health ~ 1 + (1 | cntry), data = datos)
summary(modelo_0)

#3-----

datos$c_c_gdppc_2021<-scale(datos$c_gdppc_2021, center = TRUE, scale = FALSE)

modelo_1<-lmer(health ~ 1 + c_c_gdppc_2021 + (1 | cntry), data = datos)
summary(modelo_1)

anova(modelo_0, modelo_1)

#4---------

datos$c_agea<-scale(datos$agea, center = TRUE, scale = FALSE)

modelo_2<-lmer(health ~ 1 + c_c_gdppc_2021 + c_agea + (1 | cntry), data = datos)
summary(modelo_2)

#Comparamos el ajuste de este modelo con respecto al modelo 1
anova(modelo_1, modelo_2)


#5-----

modelo_3<-lmer(health ~ 1 + c_agea +(1 + c_agea | cntry), data = datos)
summary(modelo_3)

anova(modelo_2, modelo_3)
