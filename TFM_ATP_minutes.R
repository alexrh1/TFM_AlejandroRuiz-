#Carga de librerías

library(sas7bdat)
library(nnet)
library(h2o)
library(dummies)
library(MASS)
library(reshape)
library(caret)
library(glmnet)
library(lmSupport)
library(plyr)
library(dplyr)
library(pROC)
library(randomForest)
library(gbm)
library(xgboost)
library(e1071)
library(RColorBrewer)
library(ggplot2)
library(corrplot)
library(readxl)
library(openxlsx)

#Carga de funciones

source('F:/TFM/Funciones/cruzada gbm continua.R')
source('F:/TFM/Funciones/cruzada rf continua.R')
source('F:/TFM/Funciones/cruzada xgboost continua.R')
source('F:/TFM/Funciones/cruzadas avnnet y lin.R')
source('F:/TFM/Funciones/cruzadas ensamblado continuas fuente.R')

## Funcion R2
Rsq<-function(modelo,varObj,atptransf){
  testpredicted<-predict(modelo, atptransf)
  testReal<-atptransf[,varObj]
  sse <- sum((testpredicted - testReal) ^ 2)
  sst <- sum((testReal - mean(testReal)) ^ 2)
  1 - sse/sst
}

RsqLASSO<-function(modelo,varObj,atptransf){
  testpredicted<-predict(modelo, model.matrix(as.formula(paste0(varObj,"~.")), data=atptransf)[,-1],s="lambda.1se")
  testReal<-atptransf[,varObj]
  sse <- sum((testpredicted - testReal) ^ 2)
  sst <- sum((testReal - mean(testReal)) ^ 2)
  1 - sse/sst
}

## Lectura archivo con las transformaciones
atptransf<-read.sas7bdat('F:/TFM/DATOS/trans_redes_train.sas7bdat') 
summary(atptransf) #Verificar que las variables se han leido correctamente
#Lectura archivo de datos normal 
datos<-read.sas7bdat('F:/TFM/DATOS/datosatp_depurados_train')
summary(datos)
#Lectura de archivo con variables seleccionadas en %randomselect
atp<-read.sas7bdat("F:/Machine Learning/Practica 1/redes_transf.sas7bdat")
dput(names(atp))

#Convertir variables nominales a factores
atptransf$tourney_level<-factor(atptransf$tourney_level)
atptransf$month<-factor(atptransf$month)
atptransf$round<-factor(atptransf$round)
atptransf$best_of<-factor(atptransf$best_of)
atptransf$G_tourney_name<-factor(atptransf$G_tourney_name)
datos$tourney_level<-factor(datos$tourney_level)
datos$month<-factor(datos$month)
datos$round<-factor(datos$round)
datos$best_of<-factor(datos$best_of)
datos$G_tourney_name<-factor(datos$G_tourney_name)

#*************************************************
#**************Regresión Lineal****************
#**************************************************

## Particion de datos
set.seed(2611)
partitionIndex <- createDataPartition(atptransf$minutes, p=0.8, list=FALSE)
data_train <- atptransf[partitionIndex,]
data_test <- atptransf[-partitionIndex,]

modeloPreliminar<-lm(minutes~., data=data_train) 
summary(modeloPreliminar)
Rsq(modeloPreliminar,"minutes",data_train) #0.27357
Rsq(modeloPreliminar,"minutes",data_test) #0.246346

# Nos fijamos en la importancia de las variables. Podemos sacar un grafico que muestra lo que se pierde en R2 en train al quitarlas del modelo
modelEffectSizes(modeloPreliminar)
barplot(sort(modelEffectSizes(modeloPreliminar)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)")


## Seleccion de variables sin INTERACCIONES
#aic
null<-lm(minutes~1, data=data_train)
full<-lm(minutes~., data=data_train)
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")
modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward")
coef(modeloStepAIC) #Asi veo que variables han quedado en el modelo
coef(modeloBackAIC)
# los 2 son iguales

#los evaluamos
Rsq(modeloStepAIC,"minutes",data_test) #0.2469749
Rsq(modeloBackAIC,"minutes",data_test) #0.2468793

#bic
modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)))
modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",k=log(nrow(data_train)))
coef(modeloStepBIC) #Asi veo que variables han quedado en el modelo
coef(modeloBackBIC)
# los 2 son distintos

#los evaluamos
Rsq(modeloStepBIC,"minutes",data_test) #0.2428843
Rsq(modeloBackBIC,"minutes",data_test) #0.2428843

## Regresion LASSO
y <- as.double(as.matrix(data_train[, 5])) # 5 es la columna de la Variable Objetivo
x<-model.matrix(minutes~., data=data_train)[,-1]

set.seed(1712)
cv.lasso <- cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso, s=cv.lasso$lambda.1se)

modeloLASSO<-glmnet(x,y,alpha=1,lambda = cv.lasso$lambda.1se)

RsqLASSO(cv.lasso,"minutes",data_test) #0.2051908

## Interacciones
formulaInteracciones<-function(data,posicion){
  listaFactores<-c()
  lista<-paste(names(data)[posicion],'~')
  nombres<-names(data)
  for (i in (1:length(nombres))[-posicion]){
    lista<-paste(lista,nombres[i],'+')
    if (class(data[,i])=="factor"){
      listaFactores<-c(listaFactores,i)
      for (j in ((1:length(nombres))[-c(posicion,listaFactores)])){
        lista<-paste(lista,nombres[i],':',nombres[j],'+')
      }
    }
  }
  lista<-substr(lista, 1, nchar(lista)-1)
  lista
}

formInt<-formulaInteracciones(atptransf,5)

full2<-lm(formInt, data=data_train)
#aic
modeloStepIntAIC<-step(null, scope=list(lower=null, upper=full2), direction="both")
modeloBackIntAIC<-step(full2, scope=list(lower=null, upper=full2), direction="backward")#MENSAJE DE ERROR, INFINITO PARA ESTE MODELO

Rsq(modeloStepIntAIC,"minutes",data_test)
Rsq(modeloBackIntAIC,"minutes",data_test) #Son diferentes tambien

#bic
modeloStepIntBIC<-step(null, scope=list(lower=null, upper=full2), direction="both",k=log(nrow(data_train)))
modeloBackIntBIC<-step(full2, scope=list(lower=null, upper=full2), direction="backward",k=log(nrow(data_train)))
Rsq(modeloStepIntBIC,"minutes",data_test)
Rsq(modeloBackIntBIC,"minutes",data_test) #Son diferentes tambi???n

## Validacion cruzada repetida
total<-c()
modelos<-list(modeloPreliminar,modeloStepAIC,modeloBackAIC,modeloStepBIC,modeloBackBIC,modeloStepIntAIC,modeloStepIntBIC)
formulaModelos<-sapply(modelos,formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(formulaModelos[[i]]), data = data_train,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  total<-rbind(total,data.frame(Rsquared=vcr$resample[,2],modelo=rep(paste("Modelo",i),
                                                                     nrow(vcr$resample))))
}
#falta el lasso
set.seed(1712)
vcr<-train(minutes~., data = data_train,
           method = "glmnet",
           tuneGrid=expand.grid(.alpha=1,.lambda=cv.lasso$lambda.1se),
           trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                    returnResamp="all")
)
total<-rbind(total,data.frame(Rsquared=vcr$resample[,2],modelo=rep('LASSO',
                                                                   nrow(vcr$resample))))

boxplot(Rsquared~modelo,data=total, col="dodgerblue") #6, 7 y 9 son los mejores
axis(3, at=1:length(modelos), labels=sapply(modelos,function(x) length(coef(x))), cex.axis=1)
aggregate(Rsquared~modelo, data = total, mean)
aggregate(Rsquared~modelo, data = total, sd)

#Analizamos un poco el modelo ganador: 
Rsq(modeloBackBIC,"minutes",data_train)
Rsq(modeloBackBIC,"minutes",data_test)

Rsq(modeloStepIntBIC,"minutes",data_train)
Rsq(modeloStepIntBIC,"minutes",data_test)

summary(modeloBackBIC)
summary(modeloStepBIC)


## Si el modelo ganador no tiene interacciones, nos podemos fijar en la importancia de las variables. 
modelEffectSizes(modeloPreliminar)
barplot(sort(modelEffectSizes(modeloPreliminar)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)")

## Validacion cruzada repetida para que muestre RASE
total<-c()
modelos<-list(modeloPreliminar,modeloStepAIC,modeloBackAIC,modeloStepBIC,modeloBackBIC,modeloStepIntAIC,modeloStepIntBIC)
formulaModelos<-sapply(modelos,formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(formulaModelos[[i]]), data = data_train,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  total<-rbind(total,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo",i),
                                                         nrow(vcr$resample))))
}

#falta el lasso
set.seed(1712)
vcr<-train(minutes~., data = data_train,
           method = "glmnet",
           tuneGrid=expand.grid(.alpha=1,.lambda=cv.lasso$lambda.1se),
           trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                    returnResamp="all")
)
total<-rbind(total,cbind(vcr$resample[,1:2],modelo=rep('LASSO',
                                                       nrow(vcr$resample))))

boxplot(RMSE~modelo,data=total, col="dodgerblue", main="Validación cruzada repetida para regresión lineal") 
axis(3, at=1:length(modelos), labels=sapply(modelos,function(x) length(coef(x))), cex.axis=1)
boxplot(Rsquared~modelo,data=total,col="dodgerblue", main="Validación cruzada repetida para regresión lineal")
axis(3, at=1:length(modelos), labels=sapply(modelos,function(x) length(coef(x))), cex.axis=0.5)

#*******************************************************
#K-NN PARA LA PREDICCIÓN DE VARIABLE CONTINUA
#******************************************************
## Partición de datos

set.seed(12345)
partitionIndex <- createDataPartition(datos$minutes, p=0.8, list=FALSE)
data_train <- datos[partitionIndex,]
data_test <- datos[-partitionIndex,]

knnvcr <- train(minutes~ ., data = data.frame(minutes=data_train$minutes,model.matrix(minutes~., data=data_train)[,-1]),
                preProcess = c("center","scale"),
                method = "knn", tuneGrid = expand.grid(k = c(25)),
                trControl = trainControl(method="repeatedcv", number=4, repeats=20,
                                         returnResamp="all")
)

boxplot(RMSE~k,data=knnvcr$resample,main="R-Square",col="dodgerblue")
aggregate(RMSE~k, data = knnvcr$resample, mean)
aggregate(RMSE~k, data = knnvcr$resample, sd) 


#************************************
#*********REDES NEURONALES***********
#***********************************
dput(names(atp))

# c("tourney_level", "month", "best_of", "round", "minutes", "min_rank_points", 
#"G_tourney_name", "IMP_REP_dif_rank", "IMP_REP_dif_rank_points", 
#"IMP_REP_max_rank", "IMP_REP_max_rank_points", "IMP_REP_min_age", 
#"IMP_REP_min_rank", "aleat", "LOG_IMP_REP_dif_rank", "SQR_IMP_REP_dif_rank_points", 
#"LOG_IMP_REP_max_rank", "PWR_IMP_REP_min_age", "LOG_IMP_REP_min_rank"
#)

continuasredes<-c("min_rank_points", "IMP_REP_min_rank", "LOG_IMP_REP_min_rank", "IMP_REP_min_age")
categoricasredes<-c("tourney_level", "month","best_of","round","G_tourney_name")

# a)Eliminar las observaciones con missing en alguna variable

atp2<-na.omit(atp,(!is.na(atp)))

# b)pasar las categoricas a dummies

atp3<- dummy.data.frame(atp2, categoricasredes, sep = ".")

# c)estandarizar las variables continuas

# Calculo medias y dtipica de datos y estandarizo (solo las continuas)

means <-apply(atp[,continuasredes],2,mean) 
sds<-sapply(atp[,continuasredes],sd) 

# Estandarizo solo las continuas y uno con las categoricas

atpbis<-scale(atp[,continuasredes], center = means, scale = sds)
numerocont<-which(colnames(atp)%in%continuasredes)
atpbis<-cbind(atpbis,atp3[,-numerocont])

dput(names(atpbis))

c("min_rank_points", "IMP_REP_min_rank", "LOG_IMP_REP_min_rank", "IMP_REP_min_age", "aleat",
  "tourney_level.A","tourney_level.G","tourney_level.M","tourney_level.F","tourney_level.D",
  "best_of.3", "best_of.5", "round.BR" ,"round.SF","round.F","round.QF",
  "round.RR","round.R128","round.R64","round.R32","round.R16","G_tourney_name.0",
  "G_tourney_name.0","G_tourney_name.1","G_tourney_name.2","G_tourney_name.3","G_tourney_name.4")

# ***************************
# TUNING CON CARET
# ***************************

set.seed(12346)

# Validacion cruzada repetida
control<-trainControl(method = "repeatedcv",number=4,repeats=5,savePredictions = "all") 



# nnet: parametros
#     Number of Hidden Units (size, numeric)
#     Weight Decay (decay, numeric)


nnetgrid <-  expand.grid(size=c(3,5,10,15,18,21),decay=c(0.01,0.1,0.001))

rednnet<- train(minutes~IMP_REP_min_rank+LOG_IMP_REP_min_rank+IMP_REP_min_age
                +aleat+tourney_level.A+tourney_level.G+tourney_level.M+tourney_level.F
                +best_of.3+round.BR+round.RR+round.R16+round.SF+round.R128+round.F+round.R64
                +round.QF+G_tourney_name.0+G_tourney_name.1+G_tourney_name.2+G_tourney_name.3
                ,data=atpbis,
                method="nnet",linout = TRUE,maxit=100,trControl=control,tuneGrid=nnetgrid)

rednnet


# avNNet: parametros
# Number of Hidden Units (size, numeric)
# Weight Decay (decay, numeric)
# Bagging (bag, logical)

avnnetgrid <-expand.grid(size=c(3,5,10,15,18,21),decay=c(0.01,0.1,0.001),bag=FALSE)

redavnnet<- train(minutes~IMP_REP_min_rank+LOG_IMP_REP_min_rank+IMP_REP_min_age
                  +aleat+tourney_level.A+tourney_level.G+tourney_level.M+tourney_level.F
                  +best_of.3+round.BR+round.RR+round.R16+round.SF+round.R128+round.F+round.R64
                  +round.QF+G_tourney_name.0+G_tourney_name.1+G_tourney_name.2+G_tourney_name.3
                  ,data=atpbis,
                  method="avNNet",linout = TRUE,maxit=100,trControl=control,repeats=5,tuneGrid=avnnetgrid)

redavnnet
#*******************************************************************************
#*******************PREPARACION DE DATOS PARA LOS DEMÁS ALGORITMOS**************
#*******************************************************************************

#Archivos de datos: Entrada normal de variables 
datos
dput(names(datos))


continuas <- c("IMP_REP_dif_rank", "IMP_REP_dif_rank_points", 
               "IMP_REP_max_rank", "IMP_REP_max_rank_points", "IMP_REP_min_age", 
               "IMP_REP_min_rank")
categoricas <- c("tourney_level", "month", "best_of", "round","G_tourney_name")

# a)Eliminar las observaciones con missing en alguna variable
datos<-as.data.frame(datos)
datos2<-na.omit(datos,(!is.na(datos)))

# b)pasar las categÃ³ricas a dummies

datos3<- dummy.data.frame(datos2, categoricas, sep = ".")

# c)estandarizar las variables continuas

# Calculo medias y dtipica de datos y estandarizo (solo las continuas)

means <-apply(atp[,continuas],2,mean) 
sds<-sapply(atp[,continuas],sd) 

# Estandarizo solo las continuas y uno con las categoricas

datosbis<-scale(datos[,continuas], center = means, scale = sds)
numerocont<-which(colnames(datos)%in%continuas)
datosbis<-cbind(datosbis,datos3[,-numerocont])




# ***************************************************************
# Random Forest: TUNNING
# mtry: Number of variable is randomly collected to be sampled at each split time.
# ntree: Number of branches will grow after each time split.
# Sampsize:
# Node size:
# ***************************************************************
datosbis$IMP_REP_dif_rank <- NULL
datosbis$IMP_REP_dif_rank_points <- NULL
datosbis$IMP_REP_max_rank <- NULL
datosbis$IMP_REP_max_rank_points <- NULL
datosbis$IMP_REP_min_age <- NULL
datosbis$IMP_REP_min_rank <- NULL
datosbis$aleat <- NULL
datos$aleat <- NULL



set.seed(12345)
rfgrid<-expand.grid(mtry=c(3,4,5,6,7,8,9,10,11,12,13)) #si selecciona 13 sería bagging

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 


rf<- train(y=datos$minutes, x= datos[,c(1:4,6:13)],
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=1000,nodesize=10,replace=TRUE,
           importance=TRUE)  
rf


#El mejor resultado se da con mtry =4. Ahora probaremos a muestrear por si mejoran los resultados

rfgrid1<-expand.grid(mtry=c(11))

rf1<- train((minutes)~.,data=datos,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=1000,sampsize=9000,nodesize=10,replace=TRUE,
           importance=TRUE)

#Estudio de Early Stopping

rfbis<-randomForest(minutes~.,
                    data=datos,
                    mtry=4,ntree=3000,nodesize=20,replace=TRUE)

rfbis$results
plot(rfbis$mse,main="Estudio de Early Stopping para RF", col="dodgerblue")


#***************************************************
#VALIDACIÓN CRUZADA REPETIDA PARA RANDOM FOREST
#***************************************************
#Variables guardadas para poner en cruzada 

vardep <- "minutes"
listconti <- c("IMP_REP_dif_rank", "IMP_REP_dif_rank_points", 
              "IMP_REP_max_rank", "IMP_REP_max_rank_points", "IMP_REP_min_age", 
              "IMP_REP_min_rank")
listclass <- c("tourney_level", "month", "best_of", "round","G_tourney_name")

grupos<-4
sinicio<-12345
repe<-5

#Aunque parece mejor cuanto menos árboles, comprobamos en repeticiones variando también tamaño de hoja 

medias1<-cruzadarf(data=datos, vardep=vardep,
                   listconti=listconti, 
                   listclass=listclass, grupos=grupos,sinicio=sinicio,repe=repe, 
                   nodesize=10,replace=TRUE,ntree=500,mtry=4)
medias1$modelo="RF1"

medias2<-cruzadarf(data=datos, vardep=vardep,
                   listconti=listconti, 
                   listclass=listclass, grupos=grupos,sinicio=sinicio,repe=repe, 
                   nodesize=30,replace=TRUE,ntree=500,mtry=4)
medias2$modelo="RF2"

medias3<-cruzadarf(data=datos, vardep=vardep,
                   listconti=listconti, 
                   listclass=listclass, grupos=grupos,sinicio=sinicio,repe=repe, 
                   nodesize=40,replace=TRUE,ntree=400,mtry=4)
medias3$modelo="RF3"


medias4<-cruzadarf(data=datos, vardep=vardep,
                   listconti=listconti, 
                   listclass=listclass, grupos=grupos,sinicio=sinicio,repe=repe, 
                   nodesize=50,replace=TRUE,ntree=200,mtry=4)
medias4$modelo="RF4"

medias21<-cruzadarf(data=datos, vardep=vardep,
                   listconti=listconti, 
                   listclass=listclass, grupos=grupos,sinicio=sinicio,repe=repe, 
                   nodesize=60,replace=TRUE,ntree=100,mtry=4)
medias21$modelo="RF5"

unionRF <- rbind(medias1,medias2,medias3,medias4,medias21)
par(mar=1.5)
par("mar")
#Me llevo el archivo a Excel, calculo el RMSE y vuelvo a importar 
#boxplot(data=unionRF,error~modelo, col="dodgerblue")
comp_rf<- read_excel("F:/TFM/Random Forest/Resultados.xlsx")
boxplot(data=comp_rf,RMSE~modelo, col="dodgerblue", main="Validación cruzada repetida para Random Forest")

#Importancia de las variables 

rfgrid1<-expand.grid(mtry=c(4))

rf1<- train(y=datos$minutes, x= datos[,c(1:4,6:13)],
            method="rf",trControl=control,tuneGrid=rfgrid,
            linout = FALSE,ntree=200,nodesize=50,replace=TRUE,
            importance=TRUE)

final<-rf1$finalModel
final
tablarf<-as.data.frame(importance(final))
tablarf<-tablarf[order(-tablarf$IncNodePurity),]
tablarf

par(mar=c(10, 4, 4, 2),cex.axis=0.7,las=2)
barplot(tablarf$IncNodePurity,names.arg=rownames(tablarf),
        col="#2E9AFE",
        main = "Random Forest: Importancia de las variables")
#******************************************************************
#TUNEADO DE GRADIENT BOOSTING CON CARET
# Caret permite tunear estos parámetros básicos:
#  
# 	shrinkage (parámetro v de regularización, mide la velocidad de ajuste, a menor v, más lento y necesita más iteraciones, pero es más fino en el ajuste)
# 	n.minobsinnode: tamaño máximo de nodos finales (el principal parámetro que mide la complejidad)
# 	n.trees=el número de iteraciones (árboles)
# 	interaction.depth (2 para árboles binarios)
#   bag.fraction --> utilizaremos 1, e ir reduciendo
#**************************************************************************************

gbmgrid<-expand.grid(shrinkage=c(0.1,0.05,0.03,0.01,0.001), 
         n.minobsinnode=c(10,20,40,60), n.trees=c(100,500,1000,5000), 
         interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all", classProbs=TRUE)

gbm<- train(y=datos$minutes, x= datosbis,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="gaussian", bag.fraction=1,verbose=FALSE)
gbm

plot(gbm, main = "Gradient Boosting")

#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were n.trees = 500, interaction.depth = 2,
#shrinkage = 0.05 and n.minobsinnode = 5.

#Posibilidad de remuestreo. La idea es fijar algunos parámetros para ver como funciona 
#en funcion de las iteraciones 

gbmgridm<-expand.grid(shrinkage=c(0.1),
                      n.minobsinnode=c(15),
                      n.trees=c(3000),
                      interaction.depth=c(2))


gbmm<- train(minutes~.,data=datosbis,
             method="gbm",trControl=control,tuneGrid=gbmgridm,
             distribution="gaussian", bag.fraction=0.6,verbose=FALSE)

gbmm
#No lo mejora

#ESTUDIO DE EARLY STOPPING 

gbmgrides<-expand.grid(shrinkage=c(0.03),
                       n.minobsinnode=c(60),
                       n.trees=c(10,30,50,100, 500, 1000, 2000),
                       interaction.depth=c(2))


gbmes<- train(minutes~.,data=datos,
              method="gbm",trControl=control,tuneGrid=gbmgrides,
              distribution="gaussian", bag.fraction=1,verbose=FALSE)

gbmes


plot(gbmes,main="Gradient Boosting: Early Stopping")

#Cuantos menos árboles, mejor. En 500 alcanza el mínimo. 


#Realmente, solo parace importante la variable best_of

#***************************************************************
#VALIDACION CRUZADA REPETIDA PARA GRADIENT BOOSTING
#**************************************************************

#Se ponen pocos árboles, ya que cuanto más complejo no sa ha observado mejoría, y se varía shrinkage y obs x hoja

medias5<-cruzadagbm(data=datos, vardep=vardep,
                    listconti=listconti, listclass=listclass, 
                    grupos=grupos,sinicio=sinicio,repe=repe, 
                    n.minobsinnode=60,shrinkage=0.05,n.trees=500,interaction.depth=2)
medias5$modelo="GBM1"

medias6<-cruzadagbm(data=datos, vardep=vardep,
                    listconti=listconti, listclass=listclass, 
                    grupos=grupos,sinicio=sinicio,repe=repe, 
                    n.minobsinnode=80,shrinkage=0.03,n.trees=500,interaction.depth=2)
medias6$modelo="GBM2"

medias7<-cruzadagbm(data=datos, vardep=vardep,
                    listconti=listconti, listclass=listclass, 
                    grupos=grupos,sinicio=sinicio,repe=repe, 
                    n.minobsinnode=80,shrinkage=0.03,n.trees=300,interaction.depth=2)
medias7$modelo="GBM3"

medias8<-cruzadagbm(data=datos, vardep=vardep,
                    listconti=listconti, listclass=listclass, 
                    grupos=grupos,sinicio=sinicio,repe=repe, 
                    n.minobsinnode=90,shrinkage=0.03,n.trees=200,interaction.depth=2)
medias8$modelo="GBM4"

medias9<-cruzadagbm(data=datos, vardep=vardep,
                    listconti=listconti, listclass=listclass, 
                    grupos=grupos,sinicio=sinicio,repe=repe, 
                    n.minobsinnode=100,shrinkage=0.01,n.trees=100,interaction.depth=2)
medias9$modelo="GBM5"

medias10<-cruzadagbm(data=datos, vardep=vardep,
                    listconti=listconti, listclass=listclass, 
                    grupos=grupos,sinicio=sinicio,repe=repe, 
                    n.minobsinnode=150,shrinkage=0.01,n.trees=100,interaction.depth=2)
medias10$modelo="GBM6"


unionGBM <- rbind(medias5, medias6, medias7, medias8, medias9, medias10)
#boxplot(data=unionGBM, error~modelo, col="dodgerblue") #ganador el cuatro por ser más simple

#Para recuperar el RMSE, me llevo a Excel los resultados y construyo nuevo boxplot

comp_gbm <- read_excel ("F:/TFM/Gradient Boosting/Resultados RCV.xlsx")
boxplot(data=comp_gbm, ASE~modelo, col="dodgerblue", main="Validación cruzada repetida: Gradient Boosting")
boxplot(data=comp_gbm, RMSE~modelo, col="dodgerblue", main="Validación cruzada repetida: Gradient Boosting")
comp_gbm2<-read_excel("F:/TFM/Gradient Boosting/Resultados2.xlsx")
boxplot(data=comp_gbm2, RMSE~modelo, col="dodgerblue", main="Validación cruzada repetida: Gradient Boosting")
#Importancia de variables del modelo ganador 
gbmgrid<-expand.grid(shrinkage=c(0.03),
                       n.minobsinnode=c(90),
                       n.trees=c(200),
                       interaction.depth=c(2))
gbmganador<- train(y=datos$minutes, x= datos[,c(1:4,6:13)],
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="gaussian", bag.fraction=1,verbose=FALSE)
varImp(gbmganador)
plot(varImp(gbmganador),main = "Gradient Boosting: Importancia de variables",col="dodgerblue")
summary(gbmganador)
#****************************************************************************
# TUNEADO DE XGBOOST CON CARET
# nrounds (# Boosting Iterations)
# max_depth (Max Tree Depth)
# eta (Shrinkage)
# gamma (Minimum Loss Reduction)
# colsample_bytree (Subsample Ratio of Columns)
# min_child_weight (Minimum Sum of Instance Weight)
# subsample (Subsample Percentage) mostrear observaciones antes de tunearlo
#*****************************************************************************

mode(datosatp) = "numeric"
set.seed(12345)
control<-trainControl(method = "cv",
                      number=4,
                      savePredictions = "all") 

xgbmgrid <-expand.grid( min_child_weight=20,
                        eta=c(0.1,0.05,0.03,0.01,0.2,0.3),
                        nrounds=c(100,300,500,1000,2000,4000,5000),
                        max_depth=6,
                        gamma=0,
                        colsample_bytree=1,
                        subsample=1)

xgbm<- train(y=datos$minutes, x= datos[,c(1:4,6:13)],
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,objective = "reg:linear",verbose=FALSE,
             alpha=1,lambda=0)
xgbm
xgbm$results

#Tuning parameter 'subsample' was held constant at a value of 1
#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were nrounds = 5000, max_depth = 6, eta =
#0.001, gamma = 0, colsample_bytree = 1, min_child_weight = 20 and subsample = 1.

plot(xgbm, main="XGBoost: Early Stopping")

#Estudio de Early Stopping y fijacion de parámetros

set.seed(12345)

xgbmgrides <- expand.grid(min_child_weight=20,
                        eta=0.01,
                        nrounds=500,
                        max_depth=6,
                        gamma=c(0,0.001,0.01,0.05,0.1,0.5,1),
                        colsample_bytree=c(1),
                        subsample=c(1))
control<-trainControl(method = "cv",number=4,savePredictions = "all")

xgbmes<- train(minutes~.,data=datos,
              method="xgbTree",trControl=control,
              tuneGrid=xgbmgrides,objective = "reg:linear",verbose=FALSE,
              alpha=1,lambda=0)

plot(xgbmes)

xgbmes
xgbm

plot(xgbm, main="XGBoost: Early Stopping")

#Importancia de variables 
varImp(xgbm)
plot(varImp(xgbm),main="XGBoost: Importancia de variables", col="dodgerblue")
summary(xgbm)


#****************************************************************
#VALIDACIÓN CRUZADA REPERIDA PARA XGBOOST
#***************************************************************

medias11<-cruzadaxgbm(data=datos,
                     vardep=vardep,listconti=listconti,
                     listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                     min_child_weight=20,eta=0.01,nrounds=500,max_depth=6,
                     gamma=0,colsample_bytree=1,subsample=1)
medias11$modelo="XGBM1"

medias12<-cruzadaxgbm(data=datos,
                      vardep=vardep,listconti=listconti,
                      listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                      min_child_weight=40,eta=0.01,nrounds=500,max_depth=6,
                      gamma=0,colsample_bytree=1,subsample=1)
medias12$modelo="XGBM2"

medias13<-cruzadaxgbm(data=datos,
                      vardep=vardep,listconti=listconti,
                      listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                      min_child_weight=50,eta=0.03,nrounds=300,max_depth=6,
                      gamma=0,colsample_bytree=1,subsample=1)
medias13$modelo="XGBM3"

medias14<-cruzadaxgbm(data=datos,
                      vardep=vardep,listconti=listconti,
                      listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                      min_child_weight=70,eta=0.03,nrounds=200,max_depth=6,
                      gamma=0,colsample_bytree=1,subsample=1)
medias14$modelo="XGBM4"

medias15<-cruzadaxgbm(data=datos,
                      vardep=vardep,listconti=listconti,
                      listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                      min_child_weight=80,eta=0.01,nrounds=200,max_depth=6,
                      gamma=0,colsample_bytree=1,subsample=1)
medias15$modelo="XGBM5"

medias16<-cruzadaxgbm(data=datos,
                      vardep=vardep,listconti=listconti,
                      listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                      min_child_weight=90,eta=0.03,nrounds=100,max_depth=6,
                      gamma=0,colsample_bytree=1,subsample=1)
medias16$modelo="XGBM6"

unionxgbm <- rbind(medias11, medias12, medias13, medias14, medias15, medias16)
#Nuevamente, me llevo los datos a Excel, opera para obtener el RMSE y vuelvo a importar 
comp_xgbm <- read_excel("F:/TFM/XGBoost/Resultados.xlsx")
boxplot(data=comp_xgbm,RMSE~modelo, main="Validación cruzada repetida para XGBoost", col ="dodgerblue")
#Importancia de variables del modelo ganador 
xgbmgrid <- expand.grid(min_child_weight=90,
                          eta=0.03,
                          nrounds=200,
                          max_depth=6,
                          gamma=c(0),
                          colsample_bytree=c(1),
                          subsample=c(1))
xgbmganador<- train(minutes~.,data=datos,
               method="xgbTree",trControl=control,
               tuneGrid=xgbmgrides,objective = "reg:linear",verbose=FALSE,
               alpha=1,lambda=0)
varImp(xgbmganador)
plot(varImp(xgbmganador),main="XGBoost: Importancia de variables", col="dodgerblue")
summary(xgbmganador)
#******************************************************
#******COMPARACIÓN DE LOS MEJORES MODELOS OBTENIDOS****
#******************************************************
#******************************************************
#NOTA: SE VUELVE A HACER VALIDACIÓN CRUZADA REPETIDA, 
#ESTA VEZ 20 VECES DE LOS MEJORES MODELOS DE CADA UNA DE LAS TECNICAS. 
#ESTOS RESULTADOS SE GUARDAN EN EXCEL, SE JUNTAN CON LA REGRESION 
#Y SE HACE UN NUEVO BOXPLOT COMPARANDO TODOS
#*******************************************************
repe2<-20

#Redes neuronales 

continuasredes<-c("min_rank_points", "IMP_REP_min_rank", "LOG_IMP_REP_min_rank", "IMP_REP_min_age")
categoricasredes<-categoricas<-c("tourney_level", "month","best_of","round","G_tourney_name")

medias17<-cruzadaavnnet(data=atp,
                       vardep=vardep,listconti=continuasredes,
                       listclass=categoricasredes,grupos=grupos,sinicio=sinicio,repe=repe2,
                       size=c(5),decay=c(0.01),repeticiones=5,itera=100)
medias17$modelo="avNNet"

#Random Forest 

medias18<-cruzadarf(data=datos, vardep=vardep,
                   listconti=listconti, 
                   listclass=listclass, grupos=grupos,sinicio=sinicio,repe=repe2, 
                   nodesize=50,replace=TRUE,ntree=200,mtry=4)
medias18$modelo="Random Forest"

#Gradien Boosting 

medias19<-cruzadagbm(data=datos, vardep=vardep,
                    listconti=listconti, listclass=listclass, 
                    grupos=grupos,sinicio=sinicio,repe=repe2, 
                    n.minobsinnode=90,shrinkage=0.01,n.trees=200,interaction.depth=2)
medias19$modelo="Gradient Boosting"

#XGBoost

medias20<-cruzadaxgbm(data=datos,
                      vardep=vardep,listconti=listconti,
                      listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe2,
                      min_child_weight=70,eta=0.03,nrounds=200,max_depth=6,
                      gamma=0,colsample_bytree=1,subsample=1)
medias20$modelo="XGBoost"

unionfinal<-rbind(medias18,medias19,medias20)

#Me llevo a Excel los resultados y los junto con la regresión 

#Recupero el Excel y hago boxplot final 

comparacion_final <- read_excel("F:/TFM/Resultados/Mejores_Modelos.xlsx")
comparacion_final
boxplot(data=comparacion_final,ASE~modelo,main="Comparación de los mejores modelos",
        col="dodgerblue")
boxplot(data=comparacion_final,RMSE~modelo,main="Comparación de los mejores modelos",
        col="dodgerblue")

#******************************************************
#***************ENSAMBLADO DE MODELOS******************
#******************************************************

#Preparacion de los archivos para ensamblar 

#Regresion linear 

mediaslin<-cruzadalin(data=datos,
                    vardep=vardep,listconti=listconti,
                    listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe)
mediaslinbis<-as.data.frame(mediaslin[1])
mediaslinbis$modelo<-"regression"
predi1<-as.data.frame(mediaslin[2])
predi1$reg<-predi1$pred

#Red neuronal

mediasred<-cruzadaavnnet(data=atp,
                        vardep=vardep,listconti=continuasredes,
                        listclass=categoricasredes,grupos=grupos,sinicio=sinicio,repe=repe2,
                        size=c(5),decay=c(0.01),repeticiones=5,itera=100)

mediasredbis<-as.data.frame(mediasred[1])
mediasredbis$modelo<-"avnnet"
predi2<-as.data.frame(mediasred[2])
predi2$avnnet<-predi2$pred

#Random Forest 

mediasrf<-cruzadarf(data=datos, vardep=vardep,
                   listconti=listconti, 
                   listclass=listclass, grupos=grupos,sinicio=sinicio,repe=repe, 
                   nodesize=50,replace=TRUE,ntree=200,mtry=4)

mediasrfbis<-as.data.frame(mediasrf[1])
mediasrfbis$modelo<-"rf"
predi3<-as.data.frame(mediasrf[2])
predi3$rf<-predi3$error

#Gradient Boosting 
mediasgbm<-cruzadagbm(data=datos,
                    vardep=vardep,listconti=listconti,
                    listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                    n.minobsinnode=90,shrinkage=0.01,n.trees=200,interaction.depth=2)

mediasgbmbis<-as.data.frame(mediasgbm[1])
mediasgbmbis$modelo<-"gbm"
predi4<-as.data.frame(mediasgbm[2])
predi4$gbm<-predi4$error

#XGBoost 

mediasxgbm<-cruzadaxgbm(data=datos,
                     vardep=vardep,listconti=listconti,
                     listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                     min_child_weight=70,eta=0.03,nrounds=200,max_depth=6,
                     gamma=0,colsample_bytree=1,subsample=1)

mediasxgbmbis<-as.data.frame(mediasxgbm[1])
mediasxgbmbis$modelo<-"xgbm"
predi5<-as.data.frame(mediasxgbm[2])
predi5$xgbm<-predi5$error

#k-nn

mediasknn <- read_xlsx("F:/TFM/Resultados/Resultado k-NN.xlsx")
mediasknnbis <- as.data.frame(mediasknn[1])
mediasknn$modelo <-"knn"
predi6<-as.data.frame(mediasknn[2])
predi6$knn<-predi6$error

#Antes de la construccion de los ensamblados, se unen los arhivos
#y nos los llevamos a Excel para operar en términos de RMSE. 

unipredi <-cbind(predi1, predi2, predi3, predi4, predi5)

unipredi <- read_excel('F:/TFM/Resultados/Predicciones ensamblado.xlsx')

#Construcción de los modelos ensamblados 

unipredi$predi7<-(unipredi$avnnet+unipredi$reg)/2
unipredi$predi8<-(unipredi$rf+unipredi$knn)/2
unipredi$predi9<-(unipredi$rf+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi10<-(unipredi$reg+unipredi$knn+unipredi$rf)/3
unipredi$predi11<-(unipredi$knn+unipredi$avnnet+unipredi$xgbm)/3
unipredi$predi12<-(unipredi$reg+unipredi$avnnet+unipredi$rf)/3
unipredi$predi13<-(unipredi$gbm+unipredi$rf+unipredi$xgbm+unipredi$knn)/4

repeticiones<-nlevels(factor(unipredi$Rep))
unipredi$Rep<-as.factor(unipredi$Rep)
unipredi$Rep<-as.numeric(unipredi$Rep)

#Lectura del ensamblado definitivo 

ensemble <- read.xlsx('F:/TFM/Resultados/Ensamblado definitivo.xlsx')
boxplot(data=ensemble, RMSE~modelo, col='dodgerblue', main='Ensamblado de modelos')

