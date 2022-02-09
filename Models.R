#Importation des librairies
library(rpart)
library(rpart.plot)
library(caret)
library(tidyverse)
library(ROSE)
library(dplyr)
library(questionr)
library(GGally)
library(pROC)
library(xgboost)
#Lecture de la base de donn?es
data <-read.csv("data.csv", sep=';')
head(data)
str(data)
dim(data)
names(data)
#Traitement des donn?es 
sum(is.na(data)) 
data <-na.omit(data)
data<-select(data,-c(X, test_date))
str(data)

#Recodage des donn?es 
data$toux=as.factor(data$toux)
data$toux <- fct_collapse(data$toux,
                          "Non" = c("0"),
                          "Oui" = c("1"))

data$maux_de_gorge=as.factor(data$maux_de_gorge)
data$maux_de_gorge <- fct_collapse(data$maux_de_gorge,
                                   "Non" = c("0"),
                                   "Oui" = c("1"))

data$essoufflement=as.factor(data$essoufflement)
data$essoufflement <- fct_collapse(data$essoufflement,
                                   "Non" = c("0"),
                                   "Oui" = c("I"))

data$maux_de_tete=as.factor(data$maux_de_tete)
data$maux_de_tete <- fct_collapse(data$maux_de_tete,
                                  "Non" = c("0"),
                                  "Oui" = c("1"))

data$sexe=as.factor(data$sexe)
data$sexe <- fct_collapse(data$sexe,
                          "Femme" = c("F"),
                          "Homme" = c("M"))

data$test_indication=as.factor(data$test_indication)

data$fievre=as.factor(data$fievre)
data$fievre <- fct_collapse(data$fievre,  
                            "Non" = c("0"),
                            "Oui, Trop" = c("f"),
                            "Oui, un peu"=c("f++"))

data$gout=as.factor(data$gout)
data$gout <- fct_collapse(data$gout,
                          "Non" = c("0"),
                          "Oui" = c("1"))

data$age=as.factor(data$age)
data$age <- fct_collapse(data$age,
                         "[21 - 39]" = c("21", "22","23","24","25","26","27","28","29", "30","31", "32","33","34","35","36","37","38","39"),
                         "[40 - 59]" = c("40", "41", "42","43","44","45","46","47","48","49", "50","51", "52","53","54","55","56","57","58","59"))

data$corona_result=as.factor(data$corona_result)

summary(data)
str(data)
#Train and Test
split = sample(2,nrow(data), replace = TRUE, prob = c(0.8, 0.2))
training_data = data[split==1, ]
test_data = data[split==2, ]
dim(training_data)
dim(test_data)
table(training_data$corona_result)
prop.table(table(training_data$corona_result))
barplot(prop.table(table(training_data$corona_result)),
        col = rainbow(2),
        ylim=c(0,1),
        main="corona_result distribution")

over<-ovun.sample(corona_result~., data=training_data, method ="over", N= 2789010 )$data
table(over$corona_result)
barplot(prop.table(table(over$corona_result)),
        col = rainbow(2),
        ylim=c(0,1),
        main="corona_result distribution after oversampling")

#Régression logistique
LR<-glm(corona_result ~., data = over,family = "binomial")
summary(LR)
AIC(LR)

#Sélection des variables par la méthode de stepwise en se basant sur le critère AIC
LR_both=step(LR,direction="both")
summary(LR_both)

#Le modèle a gardé toutes les variables, donc je vais travailler par la suite avec LR

#Odds Ratios
odds.ratio(LR)
#Représentation graphique des Odds Ratios 
ggcoef_model(LR, exponentiate = TRUE)

#pseudo R (McFadden)
mod<-glm(corona_result ~  1, data = over , family = "binomial" )
pseudo_r =1-logLik(LR)/logLik(mod)
pseudo_r

#test de vraisemblance 
TRV=2*(logLik(LR)-logLik(mod))
TRV
qchisq(0.95,13)

#Prédictions
predts=predict ( LR, test_data, type ='response')
predts
#Validation du mod?le

#Courbe de ROC
roc(test_data$corona_result, predts ,plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="blue", lwd=2, print.auc=TRUE, main ="Logistic Regression")
p = ifelse(predts> 0.5 , 1, 0)
p
#Matrice de confusion
Matrice_de_confusion = table(Predicted = p, Real = test_data$corona_result)
Matrice_de_confusion

#Mesures de performance du mod?le
precision=Matrice_de_confusion[2,2]/( Matrice_de_confusion[2,2]+Matrice_de_confusion[2,1])
precision

Rappel=Matrice_de_confusion[2,2]/( Matrice_de_confusion[1,2]+Matrice_de_confusion[2,2])
Rappel #sensitivity

specificity=Matrice_de_confusion[1,1]/( Matrice_de_confusion[1,1]+Matrice_de_confusion[2,1])
specificity 

FP_rate=1-specificity
FP_rate

F_mesure=2*((precision*Rappel)/(precision+Rappel))
F_mesure

accuracy = sum(diag(Matrice_de_confusion) / sum(Matrice_de_confusion))
accuracy

auc(test_data$corona_result, predts)

#XGBoost
X_train = data.matrix(over[,-10])
y_train = over[,10]
X_test = data.matrix(test_data[,-10])
y_test = test_data[,10]
xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
xgboost_test = xgb.DMatrix(data=X_test, label=y_test)
model <- xgboost(data = xgboost_train, max.depth=3,nrounds=50)
summary(model)
importance_matrix =xgb.importance(colnames(xgboost_train), model = model)
importance_matrix 
xgb.plot.importance(importance_matrix[1:9,])
pred_test = predict(model, xgboost_test)
pred_test
pred_y = as.factor((levels(y_test))[round(pred_test)])
print(pred_y)
conf_mat = table(Predicted = pred_y, Real = y_test)
conf_mat
#Mesures de performance du modèle
precision_xgb=conf_mat[2,2]/( conf_mat[2,2]+conf_mat[2,1])
precision_xgb

Rappel_xgb=conf_mat[2,2]/( conf_mat[1,2]+conf_mat[2,2])
Rappel_xgb #sensitivity

specificity_xgb=conf_mat[1,1]/( conf_mat[1,1]+conf_mat[2,1])
specificity_xgb 

FP_rate_xgb=1-specificity_xgb
FP_rate_xgb

F_mesure_xgb=2*((precision_xgb*Rappel_xgb)/(precision_xgb+Rappel_xgb))
F_mesure_xgb

accuracy_xgb = sum(diag(conf_mat) / sum(conf_mat))
accuracy_xgb

roc(test_data$corona_result, pred_test ,plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="blue", lwd=2, print.auc=TRUE, main="XGBoost")
auc(test_data$corona_result, pred_test)

#Decision Tree Classifier

dtree <- rpart(corona_result ~., data = over, method = "class")
dtree
rpart.plot(dtree, extra = 106)
pred<-predict(dtree, test_data)
pred[,2]
predicted <-predict(dtree, test_data, type = 'class')
table_mat = table(Predicted = predicted, Real = test_data$corona_result)
table_mat
#Mesures de performance du modèle
precision_dtree=table_mat[2,2]/( table_mat[2,2]+table_mat[2,1])
precision_dtree

Rappel_dtree=table_mat[2,2]/( table_mat[1,2]+table_mat[2,2])
Rappel_dtree #sensitivity

specificity_dtree=table_mat[1,1]/( table_mat[1,1]+table_mat[2,1])
specificity_dtree 

FP_rate_dtree=1-specificity_dtree
FP_rate_dtree

F_mesure_dtree=2*((precision_dtree*Rappel_dtree)/(precision_dtree+Rappel_dtree))
F_mesure_dtree

accuracy_dtree = sum(diag(table_mat) / sum(table_mat))
accuracy_dtree

roc(test_data$corona_result, pred[,2] ,plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="blue", lwd=2, print.auc=TRUE, main="Decision tree")
auc(test_data$corona_result, pred[,2])

