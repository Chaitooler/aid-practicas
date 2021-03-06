library(corrplot) # Paquete para representaciones gr�ficas de matrices
library(readxl) # Permite leer archivos xlsx

IMCinfantil=read_excel("C:/.../IMCinfantil.xlsx")
# Importa la base con la cual se va a trabajar
attach(IMCinfantil) # Se pone la base en la memoria

base.ni�os=data.frame(EDAD,PESO,TALLA,IMC,CC) 
# Arma una sub-base con las variables num�ricas de IMCinfantil
base.ni�os$CC=max(base.ni�os$CC)-base.ni�os$CC 
# Cambia la variable para que correlacione en forma negativa con las restantes
M=cor(base.ni�os) # Calcula la matriz de correlaci�n
corrplot.mixed(M, lower="number", upper="shade", addshade="all")
# Produce un correlograma
