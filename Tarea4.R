library(survival)
library(prodlim)
library(ggplot2)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condSURV)
library(ggfortify)


#1--------------
library(haven)
data <- read_sav("Reincidencia.sav")
summary(data$Meses)


#2--------------

tabla_mort_reduc <- survfit(Surv(Meses, Reincid) ~ 1, data=data)
summary(tabla_mort_reduc, times = c(1,10,10*(2:20)))


#3-------------

#Obtenemos el grafico
survfit2(Surv(Meses, Reincid) ~ 1, data = data) %>% 
  ggsurvfit() +
  labs(
    x = "Meses",
    y = "Probabilidad de no reincidir"
  ) + 
  add_confidence_interval()+
  add_risktable()


#4---------------

# Regresion de Cox con variables continuas
modelo2 <- coxph(Surv(Meses,Reincid) ~ Edad_Media + NivelIngresosAnual + RESUMEN.1, data = data) 
summary(modelo2)

