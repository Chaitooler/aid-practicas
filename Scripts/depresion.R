B=as.table(rbind(c(1,4), c(7,2)))
# Guarda los datos
dimnames(B)=list(Sexo=c('Mujer','Hombre'), S�ntomas=c('Presente','Ausente'))
# Establece las categor�as de estudio

Xsq=chisq.test(B) # Realiza el test Chi cuadrado
Xsq$expected # Calcula las frecuencias esperadas 

fisher.test(B) # Realiza el test de Fisher
