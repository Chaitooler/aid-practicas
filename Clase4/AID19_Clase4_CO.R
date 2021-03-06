
#### Ejemplo de cuentas

set.seed(2018)#para fijar la semilla
datos <- data.frame(X1 = rnorm(10,5,2), X2 = rnorm(10,3,3)) 
datos

datos_centrados <- datos 
datos_centrados$X1 <- datos$X1 - mean(datos$X1) 
datos_centrados$X2 <- datos$X2 - mean(datos$X2) 
datos_centrados

matriz_cov <- cov(datos_centrados) 
matriz_cov

eigen <- eigen(matriz_cov) 
eigen$values#(autovalores)

eigen$vectors#(autovectores)

# Producto matricial 

M<-as.matrix(datos_centrados,nrow=10,ncol=2)
pc_scores <-M%*%eigen$vectors
colnames(pc_scores) <- c("PC1", "PC2") 

pc_scores

datos_recuperados <-pc_scores %*% t(eigen$vectors) #t(eigen$vectors)=solve(eigen$vectors) pues los
#autovectores son ortogonales entres sí, ya que son autovectores de una matriz simétrica.

datos_recuperados[, 1] <- datos_recuperados[, 1] + mean(datos$X1) 
datos_recuperados[, 2] <- datos_recuperados[, 2] + mean(datos$X2) 
datos_recuperados

datos
###############################################

### Ejemplo de componentes principales

data("USArrests") 
head(USArrests)

dim(USArrests)#[1] 50  4

PrinComp <- prcomp(USArrests, scale = TRUE)# es lo mismo que prcomp(USArrests,center=TRUE, scale = TRUE) 
names(PrinComp)

PrinComp$center# media

PrinComp$scale# desvio

PrinComp$rotation# loadings (autovectores)

PrinComp$sdev# raiz cuadrada de los autovalores

head(PrinComp$x)# scores

dim(PrinComp$x)

biplot(x = PrinComp, scale = 0, cex = 0.8, col = c("blue4", "brown3"))

pca<-PrinComp
pca$rotation <- -PrinComp$rotation 
pca$x <- -PrinComp$x 
biplot(x = pca, scale = 0, cex = 0.8, col = c("blue4", "brown3"))

library(ggplot2) 
PrinComp$sdev^2

prop_varianza <- PrinComp$sdev^2/sum(PrinComp$sdev^2) 
prop_varianza

ggplot(data = data.frame(prop_varianza, pc = 1:4),aes(x = pc, y = prop_varianza)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  labs(x = "Componente principal", y = "Proporción de varianza explicada")

prop_varianza_acum <- cumsum(prop_varianza) 
prop_varianza_acum

ggplot(data = data.frame(prop_varianza_acum, pc = 1:4), aes(x = pc, y = prop_varianza_acum, group = 1)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(x = "Componente principal", y = "Proporción de varianza explicada acumulada")
