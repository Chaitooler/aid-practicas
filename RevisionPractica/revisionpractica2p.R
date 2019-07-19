### ARchivo de Calcio

install.packages('xlsx')
library(xlsx)

datacalcio <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)

library(car)
library(stats)

##Hoteling  - Primero, Normalidad multivariada por grupo, Luego MDBox para decir si es lineal o cuadratico

