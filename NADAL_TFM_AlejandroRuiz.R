#Carga librerias
library(caret)
library(sas7bdat)
library(pROC)
library(glmnet)

install.packages('e1071', dependencies=TRUE)

#Funcion tasa de acierto y otras medidas
compararLog<-function(modelo,dd,nombreVar,evento){
  probs <-predict(modelo, newdata=dd,type="response")
  cm<-confusionMatrix(data=factor(ifelse(probs>0.5,1,0)), dd[,nombreVar],positive=evento)
  c(cm$overall[1],cm$byClass[1:2])
}

compararLogLASSO<-function(modelo,dd,nombreVar,evento){
  probs<-predict(modelo, model.matrix(as.formula(paste0(nombreVar,"~.")), data=dd)[,-1],s="lambda.1se",type="response")
  cm<-confusionMatrix(data=factor(ifelse(probs>0.5,1,0)), dd[,nombreVar],positive=evento)
  c(cm$overall[1],cm$byClass[1:2])
}

#Para evaluar el pseudo-R2 en regr. log???stica en cualquier conjunto de datosNadal
pseudoR2<-function(modelo,dd,nombreVar){
  pred.out.link <- predict(modelo, dd, type = "response")
  mod.out.null <- glm(as.formula(paste(nombreVar,"~1")), family = binomial, data = dd)
  pred.out.linkN <- predict(mod.out.null, dd, type = "response")
  1-sum((dd[,nombreVar]==1)*log(pred.out.link)+log(1 -pred.out.link)*(1-(dd[,nombreVar]==1)))/
    sum((dd[,nombreVar]==1)*log(pred.out.linkN)+log(1 -pred.out.linkN)*(1-(dd[,nombreVar]==1)))
}

pseudoR2LASSO<-function(modelo,dd,nombreVar){
  pred.out.link <- predict(modelo, model.matrix(as.formula(paste0(nombreVar,"~.")), data=dd)[,-1],s="lambda.1se",type="response")
  mod.out.null <- glm(as.formula(paste(nombreVar,"~1")), family = binomial, data = dd)
  pred.out.linkN <- predict(mod.out.null, dd, type = "response")
  1-sum((dd[,nombreVar]==1)*log(pred.out.link)+log(1 -pred.out.link)*(1-(dd[,nombreVar]==1)))/
    sum((dd[,nombreVar]==1)*log(pred.out.linkN)+log(1 -pred.out.linkN)*(1-(dd[,nombreVar]==1)))
}

#Gr???fico con la importancia de las variables en regr. log???stica
impVariablesLog<-function(modelo,nombreVar,dd=data_train){
  null<-glm(as.formula(paste(nombreVar,"~1")),data=dd,family=binomial)
  aux2<-capture.output(aux<-step(modelo, scope=list(lower=null, upper=modelo), direction="backward",k=0,steps=1))
  aux3<-read.table(textConnection(aux2[grep("-",aux2)]))[,c(2,5)]
  aux3$V5<-(aux3$V5-modelo$deviance)/modelo$null.deviance
  barplot(aux3$V5,names.arg = aux3$V2,las=2,horiz=T,main="Importancia de las variables (Pseudo-R2)")
}

#Lectura de datosNadal
datosNadal<-read.sas7bdat('F:/TFM/DATOS/datosdepurados_nadal_train.sas7bdat') # Poner la ruta
summary(datosNadal)

#borrar variable X_WARN
datosNadal$X_WARN_ <- NULL
summary(datosNadal)

#variables binarias
datosNadal$win_or_lose<-factor(datosNadal$win_or_lose)
datosNadal$G_round<-factor(datosNadal$G_round)
datosNadal$G_tourney_name<-factor(datosNadal$G_tourney_name)
#Vemos el reparto de 0s y 1s
prop.table(table(datosNadal$win_or_lose))

## Partici???n de datosNadal
set.seed(3011)
partitionIndex <- createDataPartition(datosNadal$win_or_lose, p=0.8, list=FALSE)
data_train <- datosNadal[partitionIndex,]
data_test <- datosNadal[-partitionIndex,]

#Modelo inicial sin transf
modeloInicial<-glm(win_or_lose~.,data_train[,],family=binomial)
summary(modeloInicial)
compararLog(modeloInicial,data_train,"win_or_lose",'1')
compararLog(modeloInicial,data_test,"win_or_lose",'1')
pseudoR2(modeloInicial,data_train,"win_or_lose") #0.88
pseudoR2(modeloInicial,data_test,"win_or_lose") #0.23

#Importancia de las variables
par(mar=c(5.1, 11, 4.1, 2.1))#ajustar el segundo valor si no cabe el nombre de las variables
impVariablesLog(modeloInicial,"win_or_lose")

#ROC
(curvaRocTrain<-roc(data_train$win_or_lose, predict(modeloInicial,data_train,type = "response"), direction="<"))
(curvaRocTest<-roc(data_test$win_or_lose, predict(modeloInicial,data_test,type = "response"), direction="<"))
plot(curvaRocTrain)
plot(curvaRocTest,add=T,col=2)

#Selecci???n manuaal
modeloManual<-glm(win_or_lose~dif_bpFaced+G_round+IMP_REP_dif_1stWon+REP_rf_svpt+REP_op_SvGms,data_train,family=binomial)
summary(modeloManual)
compararLog(modeloManual,data_train,"win_or_lose",'1') #pierde en especificidad con el anterior 
compararLog(modeloManual,data_test,"win_or_lose",'1') #gana en todo con respecto al train??
pseudoR2(modeloManual,data_train,"win_or_lose")
pseudoR2(modeloManual,data_test,"win_or_lose") #mayor en test que en train

#Importancia de las variables
impVariablesLog(modeloManual,"win_or_lose")

#ROC
(curvaRocTrain<-roc(data_train$win_or_lose, predict(modeloManual,data_train,type = "response"), direction="<"))
(curvaRocTest<-roc(data_test$win_or_lose, predict(modeloManual,data_test,type = "response"), direction="<"))
plot(curvaRocTrain)
plot(curvaRocTest,add=T,col=2)

modeloManual$rank

#Seleccion variables
#aic
null<-glm(win_or_lose~1,data=data_train,family=binomial)
full<-glm(win_or_lose~.,data=data_train,family=binomial)
modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward")
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")

roc(data_test$win_or_lose, predict(modeloBackAIC,data_test,type = "response"), direction="<")
roc(data_test$win_or_lose, predict(modeloStepAIC,data_test,type = "response"), direction="<")
compararLog(modeloBackAIC,data_test,"win_or_lose",'1')
compararLog(modeloStepAIC,data_test,"win_or_lose",'1')
pseudoR2(modeloBackAIC,data_test,"win_or_lose") #0.31
pseudoR2(modeloStepAIC,data_test,"win_or_lose") #0.67

modeloBackAIC$rank

#bic
modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",k=log(nrow(data_train)))
modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)))

roc(data_test$win_or_lose, predict(modeloBackBIC,data_test,type = "response"), direction="<")
roc(data_test$win_or_lose, predict(modeloStepBIC,data_test,type = "response"), direction="<")
compararLog(modeloBackBIC,data_test,"win_or_lose",'1')
compararLog(modeloStepBIC,data_test,"win_or_lose",'1')
pseudoR2(modeloBackBIC,data_test,"win_or_lose") #0,71
pseudoR2(modeloStepBIC,data_test,"win_or_lose") #0,71
#los dos modelos son iguales 

modeloStepBIC$rank

## Regresi???n LASSO
y <- as.double(as.matrix(data_train[, 7])) # 7 es la columna de la Variable Objetivo
x<-model.matrix(win_or_lose~., data=data_train)[,-1]

set.seed(1712)
cv.lasso <- cv.glmnet(x,y,family = "binomial", type.measure="auc")#class para MISC
plot(cv.lasso)
coef(cv.lasso, s=cv.lasso$lambda.1se)

#modificamos los datosNadal test para obtener su AUC y usamos otra funci???n para el resto de estad???sticos
x_test<-model.matrix(win_or_lose~., data=data_test)[,-1]
roc(data_test$win_or_lose, as.numeric(predict(cv.lasso,x_test,type = "response",s="lambda.1se")), direction="<")
compararLogLASSO(cv.lasso,data_test,"win_or_lose",'1')

pseudoR2LASSO(cv.lasso,data_train,"win_or_lose")
pseudoR2LASSO(cv.lasso,data_test,"win_or_lose")

#Interacciones
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

formInt<-formulaInteracciones(data_train,7)
fullInt<-glm(formInt, data=data_train, family=binomial)
#no hacemos back porque no se puede ajustar el modelo completo
#AIC
modeloForwIntAIC<-step(null, scope=list(lower=null, upper=fullInt), direction="forward")
modeloStepIntAIC<-step(null, scope=list(lower=null, upper=fullInt), direction="both")
roc(data_test$win_or_lose, predict(modeloForwIntAIC,data_test,type = "response"), direction="<")
roc(data_test$win_or_lose, predict(modeloStepIntAIC,data_test,type = "response"), direction="<")
compararLog(modeloForwIntAIC,data_test,"win_or_lose",'1')
compararLog(modeloStepIntAIC,data_test,"win_or_lose",'1')
pseudoR2(modeloForwIntAIC,data_test,"win_or_lose") #0,67
pseudoR2(modeloStepIntAIC,data_test,"win_or_lose") #0,67
#BIC
modeloForwIntBIC<-step(null, scope=list(lower=null, upper=fullInt), direction="forward",k=log(nrow(data_train)))
modeloStepIntBIC<-step(null, scope=list(lower=null, upper=fullInt), direction="both",k=log(nrow(data_train)))
roc(data_test$win_or_lose, predict(modeloForwIntBIC,data_test,type = "response"), direction="<")
roc(data_test$win_or_lose, predict(modeloStepIntBIC,data_test,type = "response"), direction="<")
compararLog(modeloForwIntBIC,data_test,"win_or_lose",'1')
compararLog(modeloStepIntBIC,data_test,"win_or_lose",'1')
pseudoR2(modeloForwIntBIC,data_test,"win_or_lose") #0,71
pseudoR2(modeloStepIntBIC,data_test,"win_or_lose") #0.71

#Validaci???n cruzada repetida
## Validaci???n cruzada repetida
total<-c()
modelos<-list(modeloInicial,modeloManual,modeloStepAIC,modeloStepBIC,modeloStepIntAIC,modeloStepIntBIC)
formulaModelos<-sapply(modelos,formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(formulaModelos[[i]]), data = data_train,
             method = "glm",family="binomial",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  auxVarObj<-data_train$win_or_lose
  data_train$win_or_lose<-make.names(data_train$win_or_lose) #formateo la variable objetivo para que funcione el codigo
  set.seed(1712)
  vcr_roc <- train(as.formula(formulaModelos[[i]]), data = data_train,
                   method = "glm", family="binomial", metric = "ROC",
                   trControl = trainControl(method = "repeatedcv",
                                            number = 5, repeats = 20,
                                            summaryFunction=twoClassSummary,
                                            classProbs=TRUE,
                                            returnResamp="all"
                   )
  )
  data_train$win_or_lose<-auxVarObj #recupero la variable objetivo en su formato
  total<-rbind(total,data.frame(accuracy=vcr$resample[,1],roc=vcr_roc$resample[,1],modelo=rep(paste("Modelo",i),
                                                                                              nrow(vcr$resample))))
}
#falta el lasso
set.seed(1712)
vcr<-train(win_or_lose~., data = data_train,
           method = "glmnet",family="binomial",
           tuneGrid=expand.grid(.alpha=1,.lambda=cv.lasso$lambda.1se),
           trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                    returnResamp="all")
)
auxVarObj<-data_train$win_or_lose
data_train$win_or_lose<-make.names(data_train$win_or_lose) #formateo la variable objetivo para que funcione el codigo
set.seed(1712)
vcr_roc <- train(win_or_lose~., data = data_train,
                 method = "glmnet", family="binomial", metric = "ROC",
                 tuneGrid=expand.grid(.alpha=1,.lambda=cv.lasso$lambda.1se),
                 trControl = trainControl(method = "repeatedcv",
                                          number = 5, repeats = 20,
                                          summaryFunction=twoClassSummary,
                                          classProbs=TRUE,
                                          returnResamp="all"
                 )
)
data_train$win_or_lose<-auxVarObj #recupero la variable objetivo en su formato
total<-rbind(total,data.frame(accuracy=vcr$resample[,1],roc=vcr_roc$resample[,1],modelo=rep("LASSO",nrow(vcr$resample))))

par(mar=c(5.1,5.1 , 6, 2.1)) #ajusto el margen superior
boxplot(accuracy~modelo,data=total,main="Tasa de acierto")
axis(3, at=1:length(modelos), labels=sapply(modelos,function(x) length(coef(x))), cex.axis=1)
boxplot(roc~modelo,data=total,main="???rea bajo la curva ROC")
axis(3, at=1:length(modelos), labels=sapply(modelos,function(x) length(coef(x))), cex.axis=1)
aggregate(roc~modelo, data = total, mean) 
aggregate(roc~modelo, data = total, sd)

#vemos la f???rmula y las variables m???s importantes
formula(modeloStepBIC) #solo 2 variables??
par(mar=c(5.1, 7, 4.1, 2.1))#ajustar el segundo valor si no cabe el nombre de las variables
impVariablesLog(modeloStepBIC,"win_or_lose")
compararLog(modeloStepBIC,data_test,"win_or_lose",'1')

#B???squeda del mejor punto de corte para el ganador
test_roc<-roc(data_test$win_or_lose, predict(modeloStepBIC,data_test,type = "response"), direction="<")
plot(test_roc,print.thres="best") #punto de corte maximiza youden
plot(test_roc,print.thres=c(0.065,0.5)) #comparo con 0.5 el que he observado antes
#se pueden a???adir otros puntos de corte que se desee comparar
    