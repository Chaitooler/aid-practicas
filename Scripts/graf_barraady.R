library(readxl) # Permite leer archivos xlsx
library(ggplot2) # Paquete para confeccionar dibujos

IMCinfantil=read_excel("C:/.../IMCinfantil.xlsx")
# Importa la base con la cual se va a trabajar
attach(IMCinfantil) # Se pone la base en la memoria

datos=data.frame(table(SEXO, CatPeso)) # Arregla los datos 

ggplot(data=datos, aes(x=CatPeso, y=Freq, fill=SEXO)) +
  geom_bar(stat="identity", colour="blue", position="dodge") +
  coord_flip() +
  scale_fill_brewer(palette="Paired") +
  xlab("Categor�a de peso") +
  ylab("")
# Produce un diagrama de barras superpuestas
  

