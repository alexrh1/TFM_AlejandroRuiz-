#PREDICCIÓN DE LA DURACIÓN DE UN PARTIDO DE TENIS 
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
#Carga del fichero de datos a predecir 

atp_predi <- read.xlsx('F:/TFM/DATOS/Datos2019_SCORE.xlsx')
dput(names(atp_predi))

atp_predi$best_of<-factor(atp_predi$best_of)
atp_predi$G_tourney_name<-factor(atp_predi$G_tourney_name)

#Lista de continuas y categóricas 

continuaspredi<-c("dif_age", "dif_rank", "dif_rank_points", 
                  "min_age", "min_rank", "max_rank", "min_rank_points", 
                  "max_rank_points","minutes")
categoricaspredi<-c("tourney_name","month","round")

#Estandarización de variables continuas 
means <-apply(atp_predi[,continuaspredi],2,mean) 
sds<-sapply(atp_predi[,continuaspredi],sd) 
atp_predibis<-scale(atp_predi[,continuaspredi], center = means, scale = sds)
numerocont<-which(colnames(atp_predi)%in%continuaspredi)
atp_predibis<-cbind(atp_predibis,atp_predi[,-numerocont])
atp_predibis<- subset(atp_predibis, select = -minutes)

prediction <-predict(rf, atp_predibis)
plot(prediction)
atp_predi$minutesPrediction<-prediction

write.xlsx(atp_predi,file='F://TFM//Predicciones//Minutes_Predicciones.xlsx')
