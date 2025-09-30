###########################
#### Análisis de Supervivencia ####
###########################

library(survival)
library(prodlim)
library(ggplot2)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condSURV)
library(ggfortify)

#Importamos los datos
data(cancer, package="survival")
status <- pmax(rotterdam$recur, rotterdam$death)
rfstime <- with(rotterdam, ifelse(recur==1, rtime, dtime))

#Creamos intervalos para crear las tablas de mortalidad. Vamos a crear un intervalo por cada dos años
hist(rotterdam$dtime)
summary(rotterdam$dtime)

#En secuencia pongo el numero minimo de de dias y maximo de la base de datos, y en by = pongo la unidad de tiempo por la que quiero
#que se creen los intervalos, en este caso dos anhos (365*2=730)

interval <- cut(rotterdam$dtime, breaks = seq(0, 7300, by = 730), 
                labels = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20"))

barplot(table(interval))

#Calculo del numero de eventos
mortality_table <- data.frame(table(interval))
mortality_table$event <- table(interval, rotterdam$death)[,2]

#Número de casos censurados (casos perdidos o casos donde no se ha producido el evento). 
#Se calcula restando la frecuencia total de cada intervalo por el número de eventos
mortality_table$censored <- mortality_table$Freq - mortality_table$event

#Número de casos expuestos
mortality_table$exposed <- NA;
for(i in 1:nrow(mortality_table)){
  mortality_table$exposed[i] <- nrow(rotterdam) - sum(mortality_table$Freq[0:(i-1)]) - mortality_table$censored[i]/2
}

#Proporción de eventos 
mortality_table$prop_event <- round(mortality_table$event/mortality_table$exposed,3)

#Proporción de no eventos 
mortality_table$prop_noevent <- round(1 - mortality_table$event/mortality_table$exposed,3)


#Proporción acumulada de no eventos
mortality_table$acc_prop_noevent <- NA
for(i in 1:nrow(mortality_table)){
  if (i==1)  {
    mortality_table$acc_prop_noevent[i] = mortality_table$prop_noevent[i]
    } else { 
  mortality_table$acc_prop_noevent[i] <- round(mortality_table$prop_noevent[i]* mortality_table$acc_prop_noevent[i-1],3)
    }
}

#Mediana de los tiempos de espera

mySurv<-Surv(time=rotterdam$dtime, event=rotterdam$death)
modelo0<-survfit(mySurv ~ 1)
modelo0



#Funcion para obtener el numero de eventos y la funcion de supervivencia en ciertos momenos temporales

tabla_mort_reduc <- survfit(Surv(dtime, death) ~ 1, data=rotterdam)
summary(tabla_mort_reduc, times = c(1,365,365*(2:20)))


#Mediana de los tiempos de espera y kaplan Meyer

mySurv<-Surv(time=rotterdam$dtime, event=rotterdam$death)
modelo0<-survfit(mySurv ~ 1)
modelo0

#Obtenemos el grafico de supervivencia
survfit2(Surv(dtime, death) ~ 1, data = rotterdam) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()+
  add_risktable()

#Obtenemos la probabilidad de sobrevivir cuatro anhos y pico (1750/365 = 4.79 anhos)
summary(survfit(Surv(dtime, death) ~ 1, data = rotterdam), times = 1750)


#Metemos una variable dicotomica (tamaño del tumor) para comparar tiempos de espera

survdiff(Surv(dtime, death) ~ size, data = rotterdam)

survfit(Surv(dtime, death) ~ size, data = rotterdam) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()


# Regresion de Cox con variables continuas
modelo2 <- coxph(Surv(dtime,death) ~ age + er, data = rotterdam) 
summary(modelo2)


# Comparamos el modelo nulo con el modelo que incluye las variables model fit
### Deviance
-2*modelo2$loglik
### Likelihood ratio test
G2 <- -2*modelo2$loglik[1] - (-2*modelo2$loglik[2])
(p <- 1- pchisq(G2, 6)) # gl = number of parameters (2 dico + 2 cont + 1 factor with 3 categories (K-1))

# Regresion de Cox con una variable categ�rica
modelo3 <- coxph(Surv(dtime,death) ~ meno, data = rotterdam) 
summary(modelo3)

#Curva de supervivencia del modelo
cox_fit <- survfit(modelo3)
autoplot(cox_fit)
