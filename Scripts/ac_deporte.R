library(ca)	# Paquete para an�lisis de correspondencias
library(FactoMineR) # Paquete con m�todos de an�lisis exploratorio de datos
library(factoextra) # Paquete para an�lisis multivariado de datos
library(ggplot2) # Paquete para confeccionar dibujos

# Armamos la base de datos

noprac=c(31,22)
hasta3=c(38,10)
masde3=c(40,6)
deporte=as.matrix(rbind(noprac,hasta3,masde3))
colnames(deporte)=c("Ausencia de depresi�n","Presencia de depresi�n")
rownames(deporte)=c("Sin pr�ctica","Hasta 3 veces por semana",
                    "M�s de 3 veces por semana")

Xsq=chisq.test(deporte)
Xsq$expected


