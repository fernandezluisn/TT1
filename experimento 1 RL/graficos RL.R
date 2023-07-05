remove(list=ls())
gc()

library(here)

setwd(here())

asalariados_parc <- read.csv("asalariados_parc_v2.csv")
asalariados_tot <- read.csv("asalariados_tot_v2.csv")

patrones_parc <- read.csv("patrones_parc_v2.csv")
patrones_Tot <- read.csv("patrones_Tot_v2.csv")

conteo <- read.csv("conteo_v2.csv")
table(conteo$imp)

total<-as.numeric(t(patrones_Tot[1,3:32]))
parcial<-as.numeric(t(patrones_parc[1,3:32]))


summary(total)
summary(parcial)

boxp2<-data.frame(cbind(total,
                        parcial))
par(mfrow = c(2, 2))

boxplot(boxp2, main="ECM patrones", col = c("blue", "red"),outline=FALSE)


# EMA
total<-as.numeric(t(patrones_Tot[3,3:32]))
parcial<-as.numeric(t(patrones_parc[3,3:32]))
summary(total)
summary(parcial)

boxp2<-data.frame(cbind(total,
                        parcial))

boxplot(boxp2, main="EMA patrones", col = c("blue", "red"),outline=FALSE)

#bias  (actual - predicted).

total<-as.numeric(t(patrones_Tot[4,3:32]))
parcial<-as.numeric(t(patrones_parc[4,3:32]))
summary(total)
summary(parcial)

boxp2<-data.frame(cbind(total,
                        parcial))

boxplot(boxp2, main="sesgo patrones", col = c("blue", "red"),outline=FALSE)

abline(h=0,col="green", lwd = 2,
       lty = 2)

#varianza
# var(base$P21_final)-var(base$P21)
total<-as.numeric(t(patrones_Tot[5,3:32]))
parcial<-as.numeric(t(patrones_parc[5,3:32]))
summary(total)
summary(parcial)

boxp2<-data.frame(cbind(total,
                        parcial))

boxplot(boxp2, main="varianza patrones", col = c("blue", "red"),outline=FALSE)

abline(h=0,col="red", lwd = 2,
       lty = 2)

#### asalariados ####
total<-as.numeric(t(asalariados_tot[1,3:32]))
parcial<-as.numeric(t(asalariados_parc[1,3:32]))


summary(total)
summary(parcial)

boxp2<-data.frame(cbind(total,
                        parcial))

boxplot(boxp2, main="ECM asalariados", col = c("blue", "red"),outline=FALSE)


# EMA
total<-as.numeric(t(asalariados_tot[3,3:32]))
parcial<-as.numeric(t(asalariados_parc[3,3:32]))
summary(total)
summary(parcial)

boxp2<-data.frame(cbind(total,
                        parcial))

boxplot(boxp2, main="EMA asalariados", col = c("blue", "red"),outline=FALSE)

#bias  (actual - predicted).

total<-as.numeric(t(asalariados_tot[4,3:32]))
parcial<-as.numeric(t(asalariados_parc[4,3:32]))
summary(total)
summary(parcial)

boxp2<-data.frame(cbind(total,
                        parcial))

boxplot(boxp2, main="sesgo asalariados", col = c("blue", "red"),outline=FALSE)

abline(h=0,col="green", lwd = 2,
       lty = 2)

#varianza
# var(base$P21_final)-var(base$P21)
total<-as.numeric(t(asalariados_tot[5,3:32]))
parcial<-as.numeric(t(asalariados_parc[5,3:32]))
summary(total)
summary(parcial)

boxp2<-data.frame(cbind(total,
                        parcial))

boxplot(boxp2, main="varianza asalariados", col = c("blue", "red"),outline=FALSE)

abline(h=0,col="red", lwd = 2,
       lty = 2)

