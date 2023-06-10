remove(list=ls())
gc()

library(dplyr)

library(here)
setwd(here())

source("funciones/funciones.r")

usu_individual_T422 <- read.csv("EPH_usu_4to_Trim_2022_txt/usu_individual_T422.txt.txt", sep=";")

usu_individual_T422<-auxiliares(usu_individual_T422)
usu_individual_T422$rama.eph<-as.factor(usu_individual_T422$rama.eph)

usu_individual_T422[usu_individual_T422$CAT_OCUP==1,]->patrones
usu_individual_T422[usu_individual_T422$CAT_OCUP==3,]->asalariados

usu_individual_T422 %>% filter(P21!=(-9) & P21>0)->baseAjustada
par(mfrow = c(1, 2))

#### tasa no respuesta ####

sum(usu_individual_T422$P21>0)->R
sum(usu_individual_T422$P21==(-9))->NR

NR/(R+NR)

sum(usu_individual_T422[usu_individual_T422$CAT_OCUP==1,]$P21>0)->R
sum(usu_individual_T422[usu_individual_T422$CAT_OCUP==1,]$P21==(-9))->NR

NR/(R+NR)

sum(usu_individual_T422[usu_individual_T422$CAT_OCUP==3,]$P21>0)->R
sum(usu_individual_T422[usu_individual_T422$CAT_OCUP==3,]$P21==(-9))->NR

NR/(R+NR)

usu_individual_T422 %>% filter(ESTADO==1) %>% group_by(NIVEL_ED) %>% summarise(n=n(),
                                                                       nr=sum(P21==(-9)),
                                                                       t=nr/n,
                                                                       p21=mean(P21))->nrE

usu_individual_T422 %>% filter(ESTADO==1) %>% group_by(rol) %>% summarise(n=n(),
                                                                               nr=sum(P21==(-9)),
                                                                               t=nr/n,
                                                                          p21=mean(P21))->nrE

usu_individual_T422 %>% filter(ESTADO==1) %>% group_by(CH04) %>% summarise(n=n(),
                                                                          nr=sum(P21==(-9)),
                                                                          t=nr/n,
                                                                          p21=mean(P21))->nrE

# hist(baseAjustada$P21, main = "Ingreso", col = "blue")
plot(density(baseAjustada$P21), main="Ingreso", col="blue")
plot(density(log(baseAjustada$P21)), main="Logaritmo del ingreso", col="green")
ks.test(baseAjustada$P21, "pnorm", mean=mean(baseAjustada$P21), sd=sd(baseAjustada$P21))
ks.test(log(baseAjustada$P21), "pnorm", mean=mean(log(baseAjustada$P21)), sd=sd(log(baseAjustada$P21)))

summary(baseAjustada$P21)

baseAjustada %>% filter(P21==4000000)->maximo

sum(baseAjustada$P21>350000)


#### patrones ####
usu_individual_T422 %>% filter(P21!=(-9) & P21>0 & CAT_OCUP==1)->baseAjustada
summary(baseAjustada$P21)
summary(baseAjustada$CH06)
summary(baseAjustada$PP3E_TOT)

par(mfrow = c(1, 2))

# hist(baseAjustada$P21, main = "Ingreso", col = "blue")
plot(density(baseAjustada$P21), main="Ingreso", col="blue")
plot(density(log(baseAjustada$P21)), main="Logaritmo del ingreso", col="green")

#### asalariados ####
usu_individual_T422 %>% filter(P21!=(-9) & P21>0 & CAT_OCUP==3)->baseAjustada
summary(baseAjustada$P21)
summary(baseAjustada$CH06)
summary(baseAjustada$PP3E_TOT)

par(mfrow = c(1, 2))


#### graficos ####
# hist(baseAjustada$P21, main = "Ingreso", col = "blue")
plot(density(baseAjustada$P21), main="Ingreso", col="blue")
plot(density(log(baseAjustada$P21)), main="Logaritmo del ingreso", col="green")

boxp<-usu_individual_T422 %>% filter(P21>0 & CAT_OCUP==1 | CAT_OCUP==3)
boxp %>% filter(P21>1000000)->altos
altos <- altos[order(altos$P21), ]

boxp %>%  mutate(sexo_catOcup = case_when(
  CH04==1 & CAT_OCUP== 1 ~ "Patrón",
  CH04==2 & CAT_OCUP== 1 ~  "Patrona",
  CH04==1 & CAT_OCUP== 3 ~  "Asalariado",
  CH04==2 & CAT_OCUP== 3 ~  "Asalariada"
))->boxp
par(mfrow = c(1, 1), cex.axis=1)
boxplot(boxp[boxp$P21<1000000,]$P21 ~ boxp[boxp$P21<1000000,]$INF, col = c("pink4", "red4","green4","blue4" ),
        xlab = "", ylab="", las = 2,outline=FALSE)

par(mfrow = c(1, 1))
boxplot(boxp[boxp$P21<1000000,]$P21 ~ boxp[boxp$P21<1000000,]$sexo_catOcup, col = c("pink4", "red4","green4","blue4" ),
        xlab = "", ylab="",outline=FALSE)


boxplot(asalariados[asalariados$P21>0,]$P21, patrones[patrones$P21>0,]$P21, col = c("blue4", "red4"),
        names = c("Asalariados", "Patrones"), main="Distribución muestral de P21")

boxplot(asalariados[asalariados$P21>0 & asalariados$P21<1000001,]$P21, patrones[patrones$P21>0 & patrones$P21<1000001,]$P21, col = c("blue4", "red4"),
        names = c("Asalariados", "Patrones"), main="Distribución muestral de P21 hasta $1M")

cor.test(asalariados[asalariados$P21>0,]$P21, asalariados[asalariados$P21>0,]$CH06, method = "spearman")

cor.test(patrones[patrones$P21>0,]$P21, patrones[patrones$P21>0,]$CH06, method = "spearman")


cor.test(asalariados[asalariados$P21>0,]$P21, asalariados[asalariados$P21>0,]$PP3E_TOT, method = "spearman")

cor.test(patrones[patrones$P21>0,]$P21, patrones[patrones$P21>0,]$PP3E_TOT, method = "spearman")

weighted.mean(asalariados[asalariados$P21>0,]$P21/100, asalariados[asalariados$P21>0,]$PONDIIO)
weighted.mean(patrones[patrones$P21>0,]$P21/100, patrones[patrones$P21>0,]$PONDIIO)

#### correlacion  ####
library(pgirmess)

usu_individual_T422 %>% filter(P21>0)->respondentes
respondentes %>%  mutate(sexo_catOcup = case_when(
  CH04==1 & CAT_OCUP== 1 ~ "Patrón",
  CH04==2 & CAT_OCUP== 1 ~  "Patrona",
  CH04==1 & CAT_OCUP== 3 ~  "Asalariado",
  CH04==2 & CAT_OCUP== 3 ~  "Asalariada"
))->respondentes

ks.test(respondentes$P21, "pnorm", mean=mean(respondentes$P21), sd=sd(respondentes$P21))
ks.test(log(respondentes$P21), "pnorm", mean=mean(log(respondentes$P21)), sd=sd(log(respondentes$P21)))

wilcox.test (P21 ~ CH04, data = respondentes)
kruskal.test(P21 ~ NIVEL_ED, data = respondentes)
kruskalmc(P21 ~ NIVEL_ED, data = respondentes)
kruskalmc(P21 ~ CAT_OCUP, data = respondentes)
kruskalmc(P21 ~ rol, data = respondentes)
kruskalmc(P21 ~ INF, data = respondentes)
kruskalmc(P21 ~ sexo_catOcup, data = respondentes)
