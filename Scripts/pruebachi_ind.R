D=as.table(rbind(c(8,12,20), c(18,15,7)))
# Guarda los datos
dimnames(D)=list(Violencia=c('Poca','Mucha'),
                 Grupo.et�reo=c('Joven','Adulto',' Mayor'))
# Establece las categor�as de estudio

Xsq=chisq.test(D) # Realiza el test Chi cuadrado
Xsq$expected # Calcula las frecuencias esperadas  
