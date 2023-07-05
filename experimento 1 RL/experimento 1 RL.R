remove(list=ls())
gc()

library(dplyr)
library(here)

library(ltm)


library(tidyverse)
library(tidymodels)
library(gridExtra)

library(Metrics)

library(robustbase)

here()
setwd(here())
source("../funciones/funciones.r")



usu_individual_T422.txt <- read.csv2("../EPH_usu_4to_Trim_2022_txt/usu_individual_T422.txt.txt", sep=";", 
                                    colClasses = c(PP04D_COD = "character"))

#### parte 1 ####
usu_individual_T422.txt %>% filter(ESTADO==1 & P21>0)->ocupados

table(ocupados$CAT_OCUP)

remove(usu_individual_T422.txt)
auxiliares(ocupados)->ocupados

#### importancia de variables ####
class(ocupados$PP3E_TOT)



# aclarar ocupados
ocupados[is.na(ocupados$JERARQUIA),]$JERARQUIA<-9
ocupados[is.na(ocupados$PP3E_TOT),]$PP3E_TOT<-mean(ocupados$PP3E_TOT, na.rm = T)

ocupados$Y <- log(ocupados$P21)
m1 <- lm( Y ~ AGLOMERADO + INF + CH06+car1 + PP3E_TOT + PP04C, data=ocupados,
          weights = PONDERA )


plot(m1)
tidy_1 <- tidy(m1, conf.int = TRUE)
# Para finalizar con el análisis de este primer módelo, 
# se observa el test de significatividad global. 
# Este test nos muestra que si bien el modelo es significativo ya que su p-valor es cercano a 0
glance(m1)

# asalariados
ocupados %>% filter(CAT_OCUP==3)->asal
asal$CAT_OCUP<-NULL
mA <- lm( Y ~ AGLOMERADO + INF + CH06+car1 + PP3E_TOT + PP04C, data=asal,
          weights = PONDERA )


plot(mA)
tidy_1 <- tidy(mA, conf.int = TRUE)
# Para finalizar con el análisis de este primer módelo, 
# se observa el test de significatividad global. 
# Este test nos muestra que si bien el modelo es significativo ya que su p-valor es cercano a 0
glance(mA)


# patrones
ocupados %>% filter(CAT_OCUP==1)->pat
pat$CAT_OCUP<-NULL
mP <- lm( Y ~ AGLOMERADO + PP03D + CH06+PP05C_2 + PP3E_TOT + PP04C, data=pat,
          weights = PONDERA )


plot(mP)
tidy_1 <- tidy(mP, conf.int = TRUE)
# Para finalizar con el análisis de este primer módelo, 
# se observa el test de significatividad global. 
# Este test nos muestra que si bien el modelo es significativo ya que su p-valor es cercano a 0
glance(mP)
 



#### experimento ####
remove(list=ls())
gc()
usu_individual_T422.txt <- read.csv2("../EPH_usu_4to_Trim_2022_txt/usu_individual_T422.txt.txt", sep=";", 
                                     colClasses = c(PP04D_COD = "character"))
ocupados2<-usu_individual_T422.txt %>% filter(ESTADO==1)
source("../funciones/funciones.r")
auxiliares(ocupados2)->ocupados2

hacerTabla<-function(base, tabla_final=NULL){
  sesgo<-c("Bias", "Standard",Metrics::bias( base$P21,base$P21_final))
  varianza<-c("Varianza", "Muestral", var(base$P21)-var(base$P21_final))
  
  if(is.null(tabla_final)){
    
    tabla_final<- rbind(metrics(base,truth=P21, estimate=P21_final),sesgo,varianza)
    
  }else{
    
    tablaActual<- rbind(metrics(base,truth=P21, estimate=P21_final),sesgo,varianza)
    
    tablaActual[, paste0("prueba_",n)]<-tablaActual[,".estimate"]
    tablaActual[,".estimate"]<-NULL
    tabla_final<- left_join(tabla_final,tablaActual, by=c(".metric",".estimator"))
    
  }
  
  return(tabla_final)
}

calculoP21Final<-function(base, modelo){
  
  error_standard <- sigma(modelo)/10
  base$error <- rnorm(nrow(base),0,error_standard) 
  base$error <- ifelse(base$error>3*error_standard, 3*error_standard,base$error)
  base$error <- ifelse(base$error< -3*error_standard, -3*error_standard,base$error)
  base$P21_con_error<-base$P21_predicho+base$error
  base$P21_final<-exp(as.numeric(base$P21_con_error))
  return(base)
}


ni=ncol(asalariados_tot)-1
for (n in ni:30) {
  print(n)
  
  
  ponerNulos(ocupados2)->baseEjercicio
  baseEjercicio[is.na(baseEjercicio$JERARQUIA),]$JERARQUIA<-9
  baseEjercicio[is.na(baseEjercicio$PP3E_TOT),]$PP3E_TOT<-mean(baseEjercicio$PP3E_TOT, na.rm = T)
  
  if(n==1){
    conteo<-baseEjercicio[,c("CODUSU", "NRO_HOGAR", "COMPONENTE", "imp")]
  }else{
    conteo$imp<-conteo$imp+baseEjercicio$imp
  }
  
  baseTrain<-baseEjercicio %>% filter(!is.na(P21_i)) 
  baseTrain$Y <- log(baseTrain$P21_i)
  ##sin jerarquía
  basePredict<-baseEjercicio %>% filter(is.na(P21_i))
  
  baseTrain %>% filter(CAT_OCUP==1)->patronesTrain
  basePredict %>% filter(CAT_OCUP==1)->patronesPred
  
  baseTrain %>% filter(CAT_OCUP==3)->asalariadosTrain
  basePredict %>% filter(CAT_OCUP==3)->asalariadosPred
  
  
  m1 <- lmrob( Y ~ AGLOMERADO + INF + CH06+car1 + PP3E_TOT + PP04C, data=baseTrain,
            weights = PONDERA, fast.s.large.n = Inf )
  
  mA <- lmrob( Y ~ AGLOMERADO + INF + CH06+car1 + PP3E_TOT + PP04C, data=asalariadosTrain,
            weights = PONDERA, fast.s.large.n = Inf )
  
  mP <- lmrob( Y ~ AGLOMERADO + PP03D + CH06+PP05C_2 + PP3E_TOT + PP04C, data=patronesTrain,
            weights = PONDERA, fast.s.large.n = Inf )
  
  

  
  predict(m1,basePredict)->basePredict$P21_predicho
  basePredict %>% filter(CAT_OCUP==1)->patronesPredTot
  basePredict %>% filter(CAT_OCUP==3)->asalariadosPredTot
  
  predict(mA,asalariadosPred)->asalariadosPred$P21_predicho
  predict(mP,patronesPred)->patronesPred$P21_predicho
  
  calculoP21Final(asalariadosPred,mA)->asalariadosPred
  calculoP21Final(patronesPred,mP)->patronesPred
  calculoP21Final(patronesPredTot,m1)->patronesPredTot
  calculoP21Final(asalariadosPredTot,m1)->asalariadosPredTot
  
  sigma(mP)
  sigma(mA)
  sigma(m1)
  
  if(n==1){
    
    hacerTabla(patronesPred)->patrones_parc
    hacerTabla(patronesPredTot)->patrones_Tot
    hacerTabla(asalariadosPred)->asalariados_parc
    hacerTabla(asalariadosPredTot)->asalariados_tot
    
    
    
  }else{
    
    hacerTabla(patronesPred,patrones_parc)->patrones_parc
    hacerTabla(patronesPredTot,patrones_Tot)->patrones_Tot
    hacerTabla(asalariadosPred,asalariados_parc)->asalariados_parc
    hacerTabla(asalariadosPredTot,asalariados_tot)->asalariados_tot
    
    
    
  }
  
  
  write.csv(patrones_parc, "patrones_parc_v2.csv", row.names = F)
  write.csv(patrones_Tot, "patrones_Tot_v2.csv", row.names = F)
  write.csv(asalariados_parc, "asalariados_parc_v2.csv", row.names = F)
  write.csv(asalariados_tot, "asalariados_tot_v2.csv", row.names = F)
  write.csv(conteo, "conteo_v2.csv", row.names = F)
  
  
  
}

total<-as.numeric(t(patrones_Tot[1,3:10]))
parcial<-as.numeric(t(patrones_parc[1,3:10]))
summary(total)
summary(parcial)

boxp2<-data.frame(cbind(total,
                        parcial))

boxplot(boxp2)
