library(readxl) # Permite leer archivos xlsx
library(FactoMineR) # Paquete con m�todos de an�lisis exploratorio de datos
library(factoextra) # Paquete para an�lisis multivariado de datos
library(ade4) # Paquete con herramientas para an�lisis multivariado de datos
library(anacor)	# Paquete para an�lisis de correspondencias simple y can�nico

empresa=read_excel("C:/.../empresa.xlsx")
# Importa la base con la cual se va a trabajar

G�nero=factor(empresa$G�nero)
Antig�edad=factor(empresa$Antig�edad)
Ingresos=factor(empresa$Ingresos)
Categor�a=factor(empresa$Categor�a)
base=data.frame(G�nero,Ingresos,Categor�a)
# Armamos la base de datos con las variables como factores

empresa.acm=MCA(base, quali.sup=1, graph=F)
# Realiza el analisis de correspondencias m�ltiple

fviz_contrib(empresa.acm, choice="var", axes=1,
             fill="royalblue", color = "black") +
  theme_gray() +
  xlab('') +
  ylab('Contribuciones (%)') +
  ggtitle('') 
# Grafica las contribuciones de las variables

fviz_contrib(empresa.acm, choice="ind", axes=1, top=5,
             fill="royalblue", color = "black") +
  theme_gray() +
  xlab('') +
  ylab('Contribuciones (%)') +
  ggtitle('') 
# Grafica las contribuciones de los individuos

fviz_mca_var(empresa.acm, repel = TRUE, col.var="royalblue") +
  theme_gray() +
  xlab('Dimensi�n 1 (38.9%)') +
  ylab('Dimensi�n 2 (33.3%)') +
  ggtitle('') 
# Realiza el biplot sim�trico

fviz_mca_ind(empresa.acm, habillage=G�nero, addEllipses=TRUE, 
             repel=TRUE, legend.title = "G�nero") +
  theme_gray() +
  xlab('Dimensi�n 1 (38.9%)') +
  ylab('Dimensi�n 2 (33.3%)') +
  ggtitle('') +
  scale_color_brewer(palette="Paired") 
# Realiza un agrupamiento por g�nero

acm.disjonctif(base)
# Calcula la matriz disyuntiva
burtTable(base)
# Calcula la matriz de Burt

acm.empresa=dudi.acm(base, scannf = FALSE)
summary(acm.empresa)
# Calcula las inercias
round(acm.empresa$c1,3)
# Calcula las coordenadas para representar 

