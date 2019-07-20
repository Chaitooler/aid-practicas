library("readxl")
library("dplyr")
library(corpcor)
library("Hotelling")
library("openxlsx")
library(MASS)
library(nortest)
library(ggplot2)
library(lawstat)
library(stats)
library(dplyr)
library(mvnormtest)
library(biotools)
library(dplyr)
library(reshape2)
library(knitr)
library(heplots)
library(factoextra)
library(klaR) 

leche = read_excel("C:/Users/chait/Desktop/Facultad/aid-practicas/Parcial/leches.xlsx")
View(leche)


mat_dist = dist(leche[, -1], method='euclidean')
mat_dist


hc_complete <- hclust(d = mat_dist, method = "complete") 
hc_average <- hclust(d = mat_dist, method = "average")
hc_single <- hclust(d = mat_dist, method = "single")
hc_ward <- hclust(d = mat_dist, method = "ward.D2")
cor(x = mat_dist, cophenetic(hc_complete))
cor(x = mat_dist, cophenetic(hc_average))
cor(x = mat_dist, cophenetic(hc_single))
cor(x = mat_dist, cophenetic(hc_ward))

plot(hc_average )
rect.hclust(hc_average, k=4, border="red")

grupos<-cutree(hc_average,k=4)
split(leche[,1],grupos)

##Graficamos
datos2 = leche
set.seed(101) 
hc_avg <- datos2[,-1] %>% scale() %>% dist(method = "euclidean") %>% 
  hclust(method = "average") 
fviz_dend(x = hc_avg, k = 4, cex = 0.6) + 
  geom_hline(yintercept = 5.5, linetype = "dashed")


fviz_cluster(object = list(data = leche[,-1], cluster = cutree(hc_avg, k = 4)), ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE) + 
  theme_bw()

grupos<-cutree(hc_avg,k=4)
split(leche[,1],grupos)



### Pruebo KMEANS
fviz_nbclust(x = datos2[,-1], FUNcluster = kmeans, method = "wss", 
             diss = dist(datos2, method = "euclidean")) + 
  geom_vline(xintercept = 4, linetype = 2)

set.seed(123)
km_clusters <- kmeans(x = datos2[,-1], centers = 4, nstart = 25)
names(km_clusters)
split(rownames(datos2),km_clusters$cluster)

fviz_cluster(object = km_clusters, data = datos2[,-1], show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) + 
  theme_bw() + 
  theme(legend.position = "none")



#### Caracteristicas medias

split(datos2[,1],km_clusters$cluster)


datos2$cluster <- grupos
datos2

datos2.c1means = colMeans(datos2 %>% filter(cluster==1) %>% select(-cluster) %>% select(-Mamífero))
datos2.c1means

datos2.c2means = colMeans(datos2 %>% filter(cluster==2) %>% select(-cluster) %>% select(-Mamífero))
datos2.c2means

datos2.c3means = colMeans(datos2 %>% filter(cluster==3) %>% select(-cluster) %>% select(-Mamífero))
datos2.c3means

datos2.c4means = colMeans(datos2 %>% filter(cluster==4) %>% select(-cluster) %>% select(-Mamífero))
datos2.c4means
