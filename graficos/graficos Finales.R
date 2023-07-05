remove(list=ls())
gc()

library(here)

setwd(here())

asalariados_parc_RL <- read.csv("../experimento 1 RL/asalariados_parc_v2.csv")
asalariados_tot_RL <- read.csv("../experimento 1 RL/asalariados_tot_v2.csv")

patrones_parc_RL <- read.csv("../experimento 1 RL/patrones_parc_v2.csv")
patrones_Tot_RL <- read.csv("../experimento 1 RL/patrones_Tot_v2.csv")

asalariados_parc_AA <- read.csv("../experimento 1/asalariados_parc_v2.csv")
asalariados_tot_AA <- read.csv("../experimento 1/asalariados_tot_v2.csv")

patrones_parc_AA <- read.csv("../experimento 1/patrones_parc_v2.csv")
patrones_Tot_AA <- read.csv("../experimento 1/patrones_Tot_v2.csv")



total_RL<-as.numeric(t(patrones_Tot_RL[1,3:32]))
parcial_RL<-as.numeric(t(patrones_parc_RL[1,3:32]))
total_AA<-as.numeric(t(patrones_Tot_AA[1,3:32]))
parcial_AA<-as.numeric(t(patrones_parc_AA[1,3:32]))



boxp2<-data.frame(cbind(total_RL,
                        parcial_RL,
                        total_AA,
                        parcial_AA))

par(mfrow = c(1, 2))
boxplot(boxp2, main="ECM patrones", col = c("blue", "blue4", "red", "red4"),outline=FALSE)


# EMA
total_RL<-as.numeric(t(patrones_Tot_RL[3,3:32]))
parcial_RL<-as.numeric(t(patrones_parc_RL[3,3:32]))
total_AA<-as.numeric(t(patrones_Tot_AA[3,3:32]))
parcial_AA<-as.numeric(t(patrones_parc_AA[3,3:32]))

boxp2<-data.frame(cbind(total_RL,
                        parcial_RL,
                        total_AA,
                        parcial_AA))

boxplot(boxp2, main="EMA patrones", col = c("blue", "blue4", "red", "red4"),outline=FALSE)

#bias  (actual - predicted).

total_RL<-as.numeric(t(patrones_Tot_RL[4,3:32]))
parcial_RL<-as.numeric(t(patrones_parc_RL[4,3:32]))
total_AA<-as.numeric(t(patrones_Tot_AA[4,3:32]))
parcial_AA<-as.numeric(t(patrones_parc_AA[4,3:32]))

boxp2<-data.frame(cbind(total_RL,
                        parcial_RL,
                        total_AA,
                        parcial_AA))

boxplot(boxp2, main="sesgo patrones", col = c("blue", "blue4", "red", "red4"),outline=FALSE)

abline(h=0,col="green", lwd = 2,
       lty = 2)

#varianza
# var(base$P21_final)-var(base$P21)
total_RL<-as.numeric(t(patrones_Tot_RL[5,3:32]))
parcial_RL<-as.numeric(t(patrones_parc_RL[5,3:32]))
total_AA<-as.numeric(t(patrones_Tot_AA[5,3:32]))
parcial_AA<-as.numeric(t(patrones_parc_AA[5,3:32]))

boxp2<-data.frame(cbind(total_RL,
                        parcial_RL,
                        total_AA,
                        parcial_AA))

boxplot(boxp2, main="varianza patrones", col = c("blue", "blue4", "red", "red4"),outline=FALSE)

abline(h=0,col="red", lwd = 2,
       lty = 2)

#### asalariados ####
total_RL<-as.numeric(t(asalariados_tot_RL[1,3:32]))
parcial_RL<-as.numeric(t(asalariados_parc_RL[1,3:32]))
total_AA<-as.numeric(t(asalariados_tot_AA[1,3:32]))
parcial_AA<-as.numeric(t(asalariados_parc_AA[1,3:32]))

boxp2<-data.frame(cbind(total_RL,
                        parcial_RL,
                        total_AA,
                        parcial_AA))


boxplot(boxp2, main="ECM asalariados", col = c("blue", "blue4", "red", "red4"),outline=FALSE)


# EMA
total_RL<-as.numeric(t(asalariados_tot_RL[3,3:32]))
parcial_RL<-as.numeric(t(asalariados_parc_RL[3,3:32]))
total_AA<-as.numeric(t(asalariados_tot_AA[3,3:32]))
parcial_AA<-as.numeric(t(asalariados_parc_AA[3,3:32]))

boxp2<-data.frame(cbind(total_RL,
                        parcial_RL,
                        total_AA,
                        parcial_AA))

boxplot(boxp2, main="EMA asalariados", col = c("blue", "blue4", "red", "red4"),outline=FALSE)

#bias  (actual - predicted).

total_RL<-as.numeric(t(asalariados_tot_RL[4,3:32]))
parcial_RL<-as.numeric(t(asalariados_parc_RL[4,3:32]))
total_AA<-as.numeric(t(asalariados_tot_AA[4,3:32]))
parcial_AA<-as.numeric(t(asalariados_parc_AA[4,3:32]))

summary(total_AA)
summary(total_RL)

boxp2<-data.frame(cbind(total_RL,
                        parcial_RL,
                        total_AA,
                        parcial_AA))
boxplot(boxp2, main="sesgo asalariados", col = c("blue", "blue4", "red", "red4"),outline=FALSE)

abline(h=0,col="green", lwd = 2,
       lty = 2)

#varianza
# var(base$P21_final)-var(base$P21)
total_RL<-as.numeric(t(asalariados_tot_RL[5,3:32]))
parcial_RL<-as.numeric(t(asalariados_parc_RL[5,3:32]))
total_AA<-as.numeric(t(asalariados_tot_AA[5,3:32]))
parcial_AA<-as.numeric(t(asalariados_parc_AA[5,3:32]))

summary(total_AA)
summary(total_RL)

boxp2<-data.frame(cbind(total_RL,
                        parcial_RL,
                        total_AA,
                        parcial_AA))

boxplot(boxp2, main="varianza asalariados", col = c("blue", "blue4", "red", "red4"),outline=FALSE)

abline(h=0,col="red", lwd = 2,
       lty = 2)

