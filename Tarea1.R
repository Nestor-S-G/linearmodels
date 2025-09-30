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
library(lme4)
library(multcomp)

#1-------------------------

ejercicio<-import('Ejercicio.csv')

ejercicio_long <- pivot_longer(data=ejercicio,
                           cols=c(Est_animo1, Est_animo2, Est_animo3),
                           names_to="Momento",
                           values_to="Puntuacion")

#2-------------

describeBy(ejercicio_long$Puntuacion, ejercicio_long$Momento, mat = TRUE)

#3-------------

a <- lm(ejercicio_long$Puntuacion ~ ejercicio_long$Momento)
summary(a)

posthoc <- ejercicio_long %>%
  pairwise_t_test(
    Puntuacion ~ Momento, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
posthoc


#5-------------

b = lmer(Puntuacion ~ Momento + (1|ID), data=ejercicio_long)
summary(b)

post.hoc <-glht(b, linfct = mcp(Momento = 'Tukey'))
summary(post.hoc)

#7-------------

x<-lmer(Puntuacion ~ Momento + Intensidad + Intensidad*Momento + (1| ID),data=ejercicio_long)
anova(x)
summary(x)

cat_plot(x, pred = Momento, modx = Intensidad, geom = "line")
lsmeans(x,pairwise~Intensidad*Momento)

#8-----------
anova(b, x)

