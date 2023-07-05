remove(list=ls())
gc()
options(scipen=999)
library(dplyr)
library(here)

library(ltm)


library(tidyverse)
library(tidymodels)
library(gridExtra)

library(Metrics)
library(randomForest)
library(reldist)
library(parallel)

here()
setwd(here())
source("../funciones/funciones.r")

hacerTabla<-function(base, tabla_final=NULL){
  sesgo<-c("Bias", "Standard",Metrics::bias( base$P21_final,base$P21))
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

usu_individual_T422.txt <- read.csv2("../EPH_usu_4to_Trim_2022_txt/usu_individual_T422.txt.txt", sep=";", 
                                    colClasses = c(PP04D_COD = "character"))

#### parte 1 ####
usu_individual_T422.txt %>% filter(ESTADO==1 & P21>0)->ocupados

table(ocupados$CAT_OCUP)

remove(usu_individual_T422.txt)
auxiliares(ocupados)->ocupados

#### importancia de variables ####
class(ocupados$PP3E_TOT)

baseActual2<-ocupados[,c("P21","INF", "CH06","CH04","rol", "rama.eph", "NIVEL_ED", "car1","car2", 
                         "AGLOMERADO", "PP07H", "PP07A", "PP07E", "CH03", "CH15", "PP03D","TECNO", 
                         "PP3E_TOT", "CH11", "CH08", "CH09", "PP04C", "CH16", "PP04B1",
                         "PP05C_1", "PP05C_2", "PP05C_3", "REGION", "JERARQUIA", "CAT_OCUP")]
sum(is.na(ocupados$PP04B_COD))
apply(is.na(baseActual2), 2, sum)



# aclarar esto
baseActual2[is.na(baseActual2$JERARQUIA),]$JERARQUIA<-9
baseActual2[is.na(baseActual2$PP3E_TOT),]$PP3E_TOT<-mean(baseActual2$PP3E_TOT, na.rm = T)


table(baseActual2$JERARQUIA)
cl <- makeCluster(4)
i <- proc.time()
m1 <- randomForest(
  formula = P21 ~ .,
  data    = baseActual2,
  mtry=23, min.node.size=10, ncpus = 4
)
f<-proc.time()
print(f-i)/60
stopCluster(cl)
varImpPlot(m1, sort=TRUE, n.var=min(30, nrow(m1$importance)),
           type=NULL, class=NULL, scale=TRUE, 
           main=deparse(substitute(m1))) 


# asalariados
baseActual2 %>% filter(CAT_OCUP==3)->asal
asal$CAT_OCUP<-NULL
cl <- makeCluster(4)
i <- proc.time()
m1 <- randomForest(
  formula = P21 ~ .,
  data    = asal,
  mtry=23, min.node.size=10, ncpus = 4
)
f<-proc.time()
print(f-i)/60
stopCluster(cl)
varImpPlot(m1, sort=TRUE, n.var=min(30, nrow(m1$importance)),
           type=NULL, class=NULL, scale=TRUE, 
           main=deparse(substitute(m1))) 

# patrones
baseActual2 %>% filter(CAT_OCUP==1)->pat
pat$CAT_OCUP<-NULL
cl <- makeCluster(4)
i <- proc.time()
m1 <- randomForest(
  formula = P21 ~ .,
  data    = pat,
  mtry=23, min.node.size=10, ncpus = 4
)
f<-proc.time()
print(f-i)/60
stopCluster(cl)
varImpPlot(m1, sort=TRUE, n.var=min(30, nrow(m1$importance)),
           type=NULL, class=NULL, scale=TRUE, 
           main=deparse(substitute(m1))) 

# CP
baseActual2 %>% filter(CAT_OCUP==2)->cp
cp$CAT_OCUP<-NULL
cl <- makeCluster(4)
i <- proc.time()
m1 <- randomForest(
  formula = P21 ~ .,
  data    = cp,
  mtry=23, min.node.size=10, ncpus = 4
)
f<-proc.time()
print(f-i)/60
stopCluster(cl)
varImpPlot(m1, sort=TRUE, n.var=min(30, nrow(m1$importance)),
           type=NULL, class=NULL, scale=TRUE, 
           main=deparse(substitute(m1))) 

#### experimento ####
ocupados2<-usu_individual_T422.txt %>% filter(ESTADO==1)
auxiliares(ocupados2)->ocupados2


asalariados_parc <- read.csv("asalariados_parc_v2.csv")
asalariados_tot <- read.csv("asalariados_tot_v2.csv")
conteo <- read.csv("conteo_v2.csv")
patrones_parc <- read.csv("patrones_parc_v2.csv")
patrones_Tot <- read.csv("patrones_Tot_v2.csv")
ni<-ncol(patrones_Tot)-1


for (n in ni:30) {
  print(n)
  i <- proc.time()
  
  ponerNulos(ocupados2)->baseEjercicio
  baseEjercicio[is.na(baseEjercicio$JERARQUIA),]$JERARQUIA<-9
  baseEjercicio[is.na(baseEjercicio$PP3E_TOT),]$PP3E_TOT<-mean(baseEjercicio$PP3E_TOT, na.rm = T)
  
  if(n==1){
    conteo<-baseEjercicio[,c("CODUSU", "NRO_HOGAR", "COMPONENTE", "imp")]
  }else{
    conteo$imp<-conteo$imp+baseEjercicio$imp
  }
  
  baseTrain<-baseEjercicio %>% filter(!is.na(P21_i)) 
  basePredict<-baseEjercicio %>% filter(is.na(P21_i))
  
  baseTrain %>% filter(CAT_OCUP==1)->patronesTrain
  basePredict %>% filter(CAT_OCUP==1)->patronesPred
  
  baseTrain %>% filter(CAT_OCUP==3)->asalariadosTrain
  basePredict %>% filter(CAT_OCUP==3)->asalariadosPred
  
  baseTrain<-baseTrain[,c("P21_i","INF", "CH06","CH04","rol", "rama.eph", "NIVEL_ED", "car1","car2", 
                           "AGLOMERADO", "PP07H", "PP07A", "PP07E", "CH03", "CH15", "PP03D","TECNO", 
                           "PP3E_TOT", "CH11", "CH08", "CH09", "PP04C", "CH16", "PP04B1",
                           "PP05C_1", "PP05C_2", "PP05C_3", "REGION", "JERARQUIA", "CAT_OCUP")]
  
  patronesTrain<-patronesTrain[,c("P21_i","INF", "CH06","CH04","rol", "rama.eph", "NIVEL_ED", "car1","car2", 
                           "AGLOMERADO", "PP07H", "PP07A", "PP07E", "CH03", "CH15", "PP03D","TECNO", 
                           "PP3E_TOT", "CH11", "CH08", "CH09", "PP04C", "CH16", "PP04B1",
                           "PP05C_1", "PP05C_2", "PP05C_3", "REGION", "JERARQUIA", "CAT_OCUP")]
  
  asalariadosTrain<-asalariadosTrain[,c("P21_i","INF", "CH06","CH04","rol", "rama.eph", "NIVEL_ED", "car1","car2", 
                           "AGLOMERADO", "PP07H", "PP07A", "PP07E", "CH03", "CH15", "PP03D","TECNO", 
                           "PP3E_TOT", "CH11", "CH08", "CH09", "PP04C", "CH16", "PP04B1",
                           "PP05C_1", "PP05C_2", "PP05C_3", "REGION", "JERARQUIA", "CAT_OCUP")]
  
  m1 <- randomForest(
    formula = P21_i ~ .,
    data    = baseTrain,
    mtry=23, min.node.size=10
  )
  
  mA <- randomForest(
    formula = P21_i ~ .,
    data    = asalariadosTrain,
    mtry=23, min.node.size=10
  )
  
  mP <- randomForest(
    formula = P21_i ~ .,
    data    = patronesTrain,
    mtry=23, min.node.size=10
  )
  
  predict(m1,basePredict)->basePredict$P21_final
  basePredict %>% filter(CAT_OCUP==1)->patronesPredTot
  basePredict %>% filter(CAT_OCUP==3)->asalariadosPredTot
  
  predict(mA,asalariadosPred)->asalariadosPred$P21_final
  predict(mP,patronesPred)->patronesPred$P21_final
  f <- proc.time()
  
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
