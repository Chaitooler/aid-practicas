library(readxl) # Permite leer archivos xlsx
library(ggplot2) # Paquete para confeccionar dibujos
library(ggrepel) # Paquete que manipula etiquetas para gr�ficos
library(plotrix) # Paquete para gr�ficos requerido para la librer�a smacof
library(smacof) # Paquete para MDS basado en la minimizaci�n del stress

cities=read_excel("C:/.../ciudades.xlsx")
# Importa la base con la cual se va a trabajar
ciudades=data.frame(cities[,2:3])

D=dist(ciudades) # Calcula las distancias eucl�deas entre las filas
MCD_D=cmdscale(D, eig=TRUE, k=2)
# Realiza el MCD de una matriz de datos con k dimensiones de representaci�n
x=MCD_D$points[,1] # Guarda las abscisas de los puntos
y=MCD_D$points[,2] # Guarda las ordenadas de los puntos

# Preparamos base de datos para el gr�fico
data=cbind(-x,-y)
datos=data.frame(data)
colnames(datos)=c("Latitud","Longitud")
rownames(datos)=c("Buenos Aires","C�rdoba","Rosario","Mendoza","Tucum�n",
                  "Salta","Santa Fe","San Juan","Resistencia",
                  "Santiago del Estero","Corrientes","Posadas", 
                  "San Salvador de Jujuy","Bah�a Blanca","Paran�",
                  "Neuqu�n")

ggplot(datos, aes(x=Latitud, y=Longitud))+
  geom_point(colour="royalblue") +
  geom_text_repel(aes(label=rownames(datos))) +
  theme_gray() 
# Realiza un gr�fico de puntos 

MCD.D= smacofSym(D, ndim=2) # Realiza una escala multidimensional
MCD.D$stress # Calula el stress del ajuste
  

      
        
              