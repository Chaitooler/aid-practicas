library(ca)	# Paquete para an�lisis de correspondencias
library(FactoMineR) # Paquete con m�todos de an�lisis exploratorio de datos
library(factoextra) # Paquete para an�lisis multivariado de datos
library(ggplot2) # Paquete para confeccionar dibujos

# Armamos la base de datos
atento=c(64,57,57,72,36,21)
leve=c(94,94,105,141,97,51)
moderado=c(58,54,65,77,54,34)
disperso=c(46,40,60,94,78,51)
base=rbind(atento,leve,moderado,disperso)
colnames(base)=c("A","B","C","D","E","F")
rownames(base)=c("Atento", "S�nt. leves","S�nt. moderados","Disperso")
      
atencion.ac=CA(base, graph=FALSE) # Realiza el analisis de correspondencias
get_ca_row(atencion.ac) # Muestra lo que se guarda de las filas
get_ca_col(atencion.ac) # Muestra lo que se guarda de las columnas

fviz_contrib(atencion.ac, choice="row", axes=1,
             fill="royalblue", color = "black") +
  theme_gray() +
  theme(axis.text.x = element_text(angle=0)) +
  xlab('Nivel de atenci�n') +
  ylab('Contribuciones (%)') +
  ggtitle('') 
# Grafica las categor�as de las filas

fviz_contrib(atencion.ac, choice="col", axes=1, 
             fill="royalblue", color = "black") +
  theme_gray() +
  theme(axis.text.x = element_text(angle=0)) +
  xlab('Nivel cultural') +
  ylab('Contribuciones (%)') +
  ggtitle('') 
# Grafica las categor�as de las columnas

fviz_ca_row(atencion.ac, repel=TRUE, col.row="royalblue") +
  theme_gray() +
  xlab('Dimensi�n 1 (95.4%)') +
  ylab('Dimensi�n 2 (2.9%)') +
  ggtitle('') 
# Grafica los puntos fila

fviz_ca_col(atencion.ac, repel=TRUE, col.col="indianred") +
  theme_gray() +
  xlab('Dimensi�n 1 (95.4%)') +
  ylab('Dimensi�n 2 (2.9%)') +
  ggtitle('') 
# Grafica los puntos columna

fviz_ca_biplot(atencion.ac, repel=TRUE, col.row="royalblue", 
               col.col="indianred") +
  theme_gray() +
  xlab('Dimensi�n 1 (95.4%)') +
  ylab('Dimensi�n 2 (2.9%)') +
  ggtitle('') 
# Realiza el biplot sim�trico

# Aplicamos ahora el paquete ca
atencion_ac=ca(base, graph = FALSE) # Realiza el an�lisis de correspondencias
summary(atencion_ac)
atencion_ac$rowcoord # Arroja las coordenadas del biplot de las filas
atencion_ac$colcoord # Arroja las coordenadas del biplot de las columnas
                        
