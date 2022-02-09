library(dplyr)
library(tidyverse)
library(ROSE)
#lecture de la base de données
data <-read.csv("data.csv", sep=';') 
data <-na.omit(data)
data<-select(data,-c(X, test_date))
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
                            "Oui, Un peu"=c("f++"))
data$gout=as.factor(data$gout)
data$gout <- fct_collapse(data$gout,
                          "Non" = c("0"),
                          "Oui" = c("1"))
data$corona_result=as.factor(data$corona_result)
data$age=as.factor(data$age)
data$age <- fct_collapse(data$age,
                         "[21 - 39]" = c("21", "22","23","24","25","26","27","28","29", "30","31", "32","33","34","35","36","37","38","39"),
                         "[40 - 59]" = c("40", "41", "42","43","44","45","46","47","48","49", "50","51", "52","53","54","55","56","57","58","59"))

set.seed(123)
#Train and Test
split = sample(2,nrow(data), replace = TRUE, prob = c(0.8, 0.2))
training_data = data[split==1, ]
test_data = data[split==2, ]
over<-ovun.sample(corona_result~., data=training_data, method ="over", N= 2789010 )$data
LR<-glm(corona_result ~., data = over,family = "binomial")


