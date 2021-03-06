library(ggplot2) # Paquete para confeccionar dibujos
library(devtools) # Colecci�nn de herramientas de desarrollo para paquetes
install_github("vqv/ggbiplot") # Instala paquete desde GitHub
library(ggbiplot) # Paquete para visualizaci�n de componentes principales
library(readxl) # Permite leer archivos xlsx

nad=read_excel("C:/.../nadadores.xlsx")
# Importa la base con la cual se va a trabajar

nadadores=data.frame(nad[,2:5])
nad.pca.cov=prcomp(nadadores, center = TRUE, scale. = FALSE)
# Realiza el an�lisis de componentes principales
nad.pca.cor=prcomp(nadadores, center = TRUE, scale. = TRUE) 
# Realiza el an�lisis de componentes principales para las variables estandarizadas
summary(nad.pca.cor)
summary(nad.pca.cov) 
# Realiza un resumen de las variabilidades explicadas por las componentes principales
      
ggscreeplot(nad.pca.cov, type = c('pev', 'cev')) +
  xlab('N�mero de componentes principales') +
  ylab('Proporci�n de la variabilidad explicada') +
  geom_line(colour='royalblue') +
  geom_point(colour='royalblue')
# Produce un gr�fico de sedimentaci�n



