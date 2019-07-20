library("readxl")
library("dplyr")
library(corpcor)
library("Hotelling")
library("openxlsx")
library(MASS)
library(nortest)
library(ggplot2)
library(lawstat)
library(stats)
library(dplyr)
library(mvnormtest)
library(biotools)
library(dplyr)
library(reshape2)
library(knitr)
library(heplots)
library(factoextra)
library(klaR) 

autos = read_excel("C:/Users/chait/Desktop/Facultad/aid-practicas/Parcial/vehiculos.xls")
View(autos)

autos = data.frame(autos)[,-1]
autos.means = colMeans(autos[,-4])
autos.means

autos$grave = factor(autos$grave)
autos.si.mean = colMeans(autos %>% filter(grave=='si') %>% select(-grave))
autos.si.mean
autos.no.mean = colMeans(autos %>% filter(grave=='no') %>% select(-grave))
autos.no.mean

fit = hotelling.test( .~ grave,  data=autos)
fit


mshapiro.test(t(autos[,-4]))
boxM(data = autos[,-4], grouping=autos[,4])


autos.lda <- lda(grave~antigüedad+edad.conductor+potencia,autos)
autos.lda

partimat(grave~antigüedad+edad.conductor+potencia, data = autos, method = "lda", prec = 200,image.colors = c("darkgoldenrod1", "snow2"), col.mean = "firebrick")

prediccionesl <- predict(object = autos.lda, newdata = autos[, -4]) 
table(autos$grave, prediccionesl$class, dnn = c("Clase real", "Clase predicha"))
trainig_error <- mean(autos$grave != prediccionesl$class) * 100
trainig_error

autos.qda <- qda(grave~antigüedad+edad.conductor+potencia,autos)
autos.qda

partimat(grave~antigüedad+edad.conductor+potencia, data = autos, method = "qda", prec = 200,image.colors = c("darkgoldenrod1", "snow2"), col.mean = "firebrick")

prediccionesq <- predict(object = autos.qda, newdata = autos[, -4]) 
table(autos$grave, prediccionesq$class, dnn = c("Clase real", "Clase predicha"))
trainig_errorq <- mean(autos$grave != prediccionesq$class) * 100
trainig_errorq
