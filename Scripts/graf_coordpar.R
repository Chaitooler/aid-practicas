library(ggplot2) # Paquete para confeccionar dibujos
library(GGally) # Paquete que extiende funciones de ggplot2

ggparcoord(data=iris, columns=1:4, mapping=aes(color=as.factor(Species))) +
  scale_color_discrete("Especies", labels=levels(iris$Species)) +
  xlab("") +
  ylab("") +
  scale_x_discrete(limit=c("Sepal.Length", "Sepal.Width", "Petal.Length", 
                             "Petal.Width"),
                   labels=c("Longitud del s�palo", "Ancho del s�palo",
                            "Longitud del p�talo", "Ancho del p�talo"))
# Produce diagrama de coordenadas paralelas  
  

