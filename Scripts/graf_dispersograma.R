library(readxl) # Permite leer archivos xlsx

IMCinfantil=read_excel("C:/.../IMCinfantil.xlsx")
# Importa la base con la cual se va a trabajar
attach(IMCinfantil) # Se pone la base en la memoria

SEX=4*(SEXO=="F")+5*(SEXO=="M")
base.ni�os=data.frame(EDAD, PESO, TALLA, IMC, CC) 
# Arma una sub-base con variables num�ricas
pairs(base.ni�os, pch=19, cex=0.8, 
      col=SEX)
# Produce un diagrama de dispersi�n de a pares