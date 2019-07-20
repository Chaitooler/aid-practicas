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

beer = read_excel("C:/Users/chait/Desktop/Facultad/aid-practicas/Parcial/beer.xlsx")
View(beer)

ggplot(beer) + geom_boxplot(aes(x = marca, y = valor, group=marca)) + theme_bw()

beer.df = data.frame(beer)
beer.df$marca = factor(beer.df$marca)
beer.df$valor = as.numeric(beer.df$valor)


summary(beer.df)
shapiro.test(beer.df[beer.df$marca =='A',"valor"])
shapiro.test(beer.df[beer.df$marca =='B',"valor"])
shapiro.test(beer.df[beer.df$marca =='C',"valor"])
shapiro.test(beer.df[beer.df$marca =='D',"valor"])
shapiro.test(beer.df[beer.df$marca =='E',"valor"])

bartlett.test(valor~marca, data=beer.df)
levene.test(beer.df$valor, beer.df$marca)

beer.aov <- aov(valor~marca, data=beer.df)
summary(beer.aov)
model.tables(beer.aov, "means")

tukey.test <- TukeyHSD(beer.aov, conf.level=0.95)
summary(tukey.test) 
tukey.test
