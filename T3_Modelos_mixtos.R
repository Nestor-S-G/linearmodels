library(rio)
library(lmerTest)
library(rstatix)
library(reshape)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(optimx)
library(lme4)
library(psych)
library(nlme)
library(interactions)
library(lcsm)
library(emmeans)

################### ****** Modelos mixtos ******    ################### 

# Importar la base de datos
heck<-import("heck2011.csv")

library(lme4)
#library(nlme) -> lme

#### *** MODELO 1: Un factor de efectos aleatorios (colegios)
mod1<-lmer(math ~  (1  | schcode), data=heck)
summary(mod1)


### Representación de las medias de los colegios

#Vamos a guardar en un objeto llamado medias todas las medias de concomiento en matemáticas de cada colegio
names(heck)
medias<- heck %>% group_by(ï..schcode)%>% summarise(media=mean(math, na.rm=TRUE), DT = sd(math, na.rm=TRUE), muestra=n())

# Reprentamos estas medias visualmente, y la línea roja representa el valor del interpceto del mod1
medias %>%
  ggplot()+
  aes(x=ï..schcode , y=media)+
  geom_point(color = "black", alpha = .7) +
  geom_hline(yintercept=57.67, color="red", size=2)+
  theme_bw()+
  theme(panel.background = element_blank(),
        axis.text = element_text(family="sans",size=14, color = "black"),
        axis.title=element_text(family="sans",size=14, color = "black"))
 

#Ahora vamos a representar gráficamente las observaciones de los estudiantes del colegio 1
#Seleccionamos las observaciones del colegio 3
cole_1<-subset(heck, schcode==1)  
cole_1 %>%
  ggplot()+
  aes(x=Rid , y=math)+
  geom_point(color = "black", alpha = .7) +
  geom_hline(yintercept=59, color="red", size=2)+
  theme_bw()+
  theme(panel.background = element_blank(),
        axis.text = element_text(family="sans",size=14, color = "black"),
        axis.title=element_text(family="sans",size=14, color = "black"))


# #### *** MODELO 2: Modelo mixto de dos factores: un factor de efecto fijo (SES) y con un factor de efectos aleatorios (colegios)
#Priemro centramos la variable SES para que la intepretación sea sobre la media de SES

heck$c_ses<-scale(heck$ses, center = TRUE, scale = FALSE)

#Ajustamos el modelo
mod2<-lmer(math ~ c_ses + (1  | schcode), data=heck)
summary(mod2)

# Comparamos el ajuste del modelo 1 con el ajuste del momdelo 2. Vamos a ver que nos saldrá un mensaje donde se nos advierte
# que se han reajustado los anterioires modelos utilziando Máxima Verosimilitud como método de estimación
anova(mod1, mod2)




                       ###### **** Modelos mixtos: medidas repetidas *** #######

#Vamos a utilizar la base de datos "depression" que está disponible dentro del paquete datarium
library(datarium)

data(depression)
head(as.data.frame(depression))

#La base de datos está en format ancho. Tenemos que restructurarla
#Para convertir una BBDD que está en formato ANCHO a formato LARGO, podemos aplicar el siguiente comando de la
#librería reshape

library(reshape2)
depression_res<-melt(depression, id.vars=c("id", "treatment"))
depression_res<- depression_res[order(depression_res$id),]

names(depression_res)<- c("id", "tratamiento", "tiempo", "depresion")

#Estadisticos descriptivos por grupo

describeBy(depression_res$depresion, depression_res$tiempo,  mat=TRUE)

# Representamos graficamente los datos con un boxplot

boxplot(depression_res$depresion~depression_res$tiempo,xlab = "Tiempo", ylab ="Depresion",
        col = c("#00AFBB", "#E7B800", "#FC4E07","#00AFBB"))

# Representamos graficamente los datos con las trayectorias
x_var_list <- c("t0", "t1", "t2", "t3")

# !! Hay que utilizar el formato ANCHO para representar las trayectorias
plot_trajectories(data = depression,
                  id_var = "id", 
                  var_list = x_var_list,
                  xlab = "Tiempo", ylab = "Value",
                  connect_missing = FALSE, 
                  title_n = TRUE)




##### *** MODELO 3: ANOVA tradicional de medidas repetida, donde la variable tiempo es un factor de efecto fijo
mod3 <- anova_test(data = depression_res, dv = depresion, wid = id, within = tiempo)
get_anova_table(mod3)

# Código para ver si hay diferencias entre cada uno de los momentos temporales
posthoc <- depression_res %>%
  pairwise_t_test(
    depresion ~ tiempo, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
posthoc


###### *** MODELO 4: ANOVA MIXTO de medidas repetidas, donde la variable sujeto es un factor de efectos aleatorios

mod4<-lmer(depresion ~  tiempo  + (1| id),data=depression_res)
summary(mod4)


#Obtenemos la matriz de varianzas covarianzas de los residuos
mod4_1<-lmer(depresion ~  -1 + tiempo + ( 1|id), data=depression_res,  na.action = na.omit)
summary(mod4_1)
anova(mod4_1)

vcov_resid <- vcov(mod4_1)
vcov_resid

#Comparaciones múltiples
library(multcomp)
post.hoc <-glht(mod4_1, linfct = mcp(tiempo = 'Tukey'))
summary(post.hoc)

#Si quisiéramos especificamos otra matriz de varianza-covarianzas (ej., "unstructure") tendríamos que
#cambiar de paquete, y en lugar de utilizar lme4 (que es lo que hemos utilzado hasta ahora), tendriamos que utilizar
#lme (del paquete nlme) 

#Vamos a ajustar otra vez el modelo3_1 con este paquete (lo llamaremos mod_3_1_2), y poniendo también la variable
#tiempo dentro del comando random

library(nlme)
mod_4_1_2<-lme(depresion ~ -1+  tiempo, random = ~1 + tiempo|id, data=depression_res)
summary(mod_4_1_2)
getVarCov(mod_4_1_2, type = "marginal")



#Comparaciones multiples

library(multcomp)
summary(glht(mod4,linfct = mcp(tiempo = "Tukey")))


###### *** MODELO 5: ANOVA MIXTO de dos factores: 
#Uno de medidas independientes (tratamiento) y otro de medias repetidas (tiempo),
#donde la variable sujeto es un factor de aleatorio

describeBy(depression_res$depresion, list(depression_res$tratamiento, depression_res$tiempo),mat=TRUE)


#Ajuste del modelo
mod5<-lmer(depresion ~  tiempo + tratamiento + tratamiento*tiempo + (1| id),data=depression_res)
anova(mod5)
summary(mod5)

#Representación gráfica

cat_plot(mod5, pred = tiempo, modx = tratamiento, geom = "line")

#Efectos simples
lsmeans(mod5,pairwise~tratamiento*tiempo)

#Podemos cambiar la categoría de referencia para realizar las comparaciones de diferencias entre
#momentos temporales que nos faltan:

#Categoría de referencia el momento t1
depression_res$tiempo <- relevel(factor(depression_res$tiempo), ref="t1")
mod5_1<-lmer(depresion ~  tiempo + tratamiento + tratamiento*tiempo + (1| id),data=depression_res)
summary(mod5_1)

#Categoría de referencia el momento t2
depression_res$tiempo <- relevel(factor(depression_res$tiempo), ref="t2")
mod5_2<-lmer(depresion ~  tiempo + tratamiento + tratamiento*tiempo + (1| id),data=depression_res)
summary(mod5_2)



#Ajusta mejor este modelo que el modelo que solo incluye el paso del tiempo?

anova(mod4, mod5)
        