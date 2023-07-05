remove(list=ls())
gc()

library(dplyr)
library(here)

library(ltm)
library(car)


library(tidyverse)
library(tidymodels)
library(gridExtra)

library(robustbase)

library(randomForest)
library(reldist)


here()
setwd(here())
source("../funciones/funciones.r")

calculoP21Final<-function(base, modelo){
  
  error_standard <- sigma(modelo)/10
  base$error <- rnorm(nrow(base),0,error_standard) 
  base$error <- ifelse(base$error>3*error_standard, 3*error_standard,base$error)
  base$error <- ifelse(base$error< -3*error_standard, -3*error_standard,base$error)
  base$P21_con_error<-base$P21_predicho+base$error
  base$P21_final<-exp(as.numeric(base$P21_con_error))
  return(base)
}



usu_individual_T422.txt <- read.csv2("../EPH_usu_4to_Trim_2022_txt/usu_individual_T422.txt.txt", sep=";", 
                                    colClasses = c(PP04D_COD = "character"))

#### parte 1 ####
usu_individual_T422.txt %>% filter(ESTADO==1 & P21>0)->ocupados

giniTot2<-gini(ocupados$P21, ocupados$PONDERA)

table(ocupados$CAT_OCUP)

auxiliares(ocupados)->ocupados


#### experimento ####
ocupados2<-usu_individual_T422.txt %>% filter(ESTADO==1)
auxiliares(ocupados2)->ocupados2

resumen <- read.csv("resumen.csv")


if(is.null(resumen)){
  ni=1
}else{
  ni=nrow(resumen)+1
}
for(n in ni:30){
  print(n)
  i <- proc.time()
  
  ponerNulos(ocupados2)->baseEjercicio
  baseEjercicio[is.na(baseEjercicio$JERARQUIA),]$JERARQUIA<-9
  baseEjercicio[is.na(baseEjercicio$PP3E_TOT),]$PP3E_TOT<-mean(baseEjercicio$PP3E_TOT, na.rm = T)
  
  
  
  baseTrain<-baseEjercicio %>% filter(!is.na(P21_i)) 
  basePredict<-baseEjercicio %>% filter(is.na(P21_i))
  
  
  ##AA
  baseTrain<-baseTrain[,c("P21_i","INF", "CH06","CH04","rol", "rama.eph", "NIVEL_ED", "car1","car2", 
                           "AGLOMERADO", "PP07H", "PP07A", "PP07E", "CH03", "CH15", "PP03D","TECNO", 
                           "PP3E_TOT", "CH11", "CH08", "CH09", "PP04C", "CH16", "PP04B1",
                           "PP05C_1", "PP05C_2", "PP05C_3", "REGION", "JERARQUIA", "CAT_OCUP")]
  
  
  
  
  m1 <- randomForest(
    formula = P21_i ~ .,
    data    = baseTrain,
    mtry=23, min.node.size=10
  )
  
  print((i-proc.time())/60)
  
  predict(m1,basePredict)->basePredict$P21_AA
  
  anti_join(baseEjercicio, basePredict)->difer
  difer$P21_AA<-difer$P21_i
  rbind(difer, basePredict)->baseAA
  giniAA<-gini(baseAA$P21_AA, baseAA$PONDERA)
  
  ##RB
  baseTrain<-baseEjercicio %>% filter(!is.na(P21_i)) 
  baseTrain$Y<-log(baseTrain$P21_i)
  m1 <- lmrob( Y ~ AGLOMERADO + INF + CH06+car1 + PP3E_TOT + PP04C, data=baseTrain,
               weights = PONDERA, fast.s.large.n = Inf )
  predict(m1,basePredict)->basePredict$P21_predicho
  calculoP21Final(basePredict,m1)->basePredict
  
  
  difer$P21_final<-difer$P21_i
  basePredict$P21_con_error<-NULL
  basePredict$P21_predicho<-NULL
  
  basePredict$error<-NULL
  rbind(difer, basePredict)->baseRL
  
  giniRL<-gini(baseRL$P21_final, baseRL$PONDERA)
  
  
  if(n==1){

    resumen<-as.data.frame(rbind(c(giniAA,giniRL)))



  }else{

    
    as.data.frame(rbind(resumen, c(giniAA,giniRL)))->resumen

    write.csv(resumen, "resumen.csv", row.names = F)
    

  }
  
  
  
  
  
  
  
}

#write.csv2(baseRL, row.names = F, "base_ejemplo.csv")


resumen$dif1<-resumen$V1-giniTot2
resumen$dif2<-resumen$V2-giniTot2
vif(m1)
#grafico de densidad
baseRL %>% filter(imp==1)->imputados
plot(density(log(imputados$P21_AA)), col="red", main="densidad log P21 imputada")
lines(density(log(imputados$P21)))
lines(density(log(imputados$P21_final)), col="blue")

legend("topleft",legend = c("Declarado", "AA", "RLMR"), fill = c("black", "red", "blue"))

# grafico resumen
resumen <- read.csv("resumen.csv")
names(resumen)<-c("AA", "RLMR")
giniTot2<-gini(ocupados$P21, ocupados$PONDERA)


boxplot(resumen, col = c("red", "blue"), main= "resultados coeficiente de gini")
abline(h=giniTot2,col="green", lwd = 2,
       lty = 2)
