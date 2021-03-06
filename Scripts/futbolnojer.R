library(cluster) # Incluye m�todos para el an�lisis de clusters
library(factoextra) 
# Permite extraer y visualizar resultados de an�lisis de datos multivariados
library(ggplot2) # Paquete para confeccionar dibujos
library(readxl) # Permite leer archivos xlsx

futbol=read_excel("C:/.../futbol.xlsx")
# Importa la base con la cual se va a trabajar

futbol=data.frame(futbol) # Arregla los datos
df=na.omit(futbol) # Elimina los registros con datos faltantes
df=scale(futbol[,-1]) # Estandariza las variables
rownames(df)=futbol[,1] # Pone nombre a las filas

set.seed(123) # Fija una semilla
kmfutbol=kmeans(df, centers=4, nstart=10)
# Aplica k-means con k=4

par(mar=c(5,5,1,2)) # Establece m�rgenes
clusplot(df, kmfutbol$cluster, main=NULL, color=TRUE, shade=TRUE, labels=4, 
         lines=0, plotchar=FALSE)
# Dibuja la clasificaci�n
clusplot(df, kmfutbol$cluster, main=NULL, color=TRUE, shade=FALSE, labels=2, 
         lines=0, plotchar=FALSE, cex.txt=0.6, col.txt="black", cex=0)
# Dibuja la clasificaci�n con etiquetas

fviz_cluster(kmfutbol, data=df, geom="text", labelsize=8, repel=TRUE) + 
  scale_color_brewer(palette = "Set1") +
  ggtitle('') 
# Dibuja la clasificaci�n personalizada

Cluster=kmfutbol$cluster
datos=data.frame(cbind(futbol,Cluster))
# Guarda el grupo de pertenencia de cada pa�s

ggplot(datos, aes(x=GC,y=GF)) +
  geom_point(aes(colour=factor(Cluster))) +
  scale_color_brewer(palette = "Set1") +
  xlab("Goles en contra") +
  ylab("Goles a favor") +
  labs(color='Cluster') 
# Grafica relaci�n de goles por grupo 

ggplot(datos, aes(x=GC,y=GF)) +
  geom_point(aes(colour=factor(Cluster))) +
  geom_text(aes(label=Pa�s), size=3, hjust=0, vjust=0) +
  scale_color_brewer(palette = "Set1") +
  xlab("Goles en contra") +
  ylab("Goles a favor") +
  labs(color='Cluster') 
# Grafica relaci�n de goles por pa�s




