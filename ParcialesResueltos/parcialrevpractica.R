###Install
install.packages("mvnormtest")
install.packages("biotools")
install.packages("heplots")
install.packages("readxl")
install.packages("corpcor")
install.packages("Hotelling")
install.packages("MASS")
install.packages("nortest")
install.packages("ggplot2")
install.packages('lawstat')

###Include
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

city = read_excel("C:/Users/chait/Desktop/Facultad/aid-practicas/RevisionPractica/city.xlsx")
cancer = read_excel("C:/Users/chait/Desktop/Facultad/aid-practicas/RevisionPractica/cancer.xls")
calcio = read_excel("C:/Users/chait/Desktop/Facultad/aid-practicas/RevisionPractica/calcio.xls")

###############################################################
## EJERCICIO 1 - COMPARACION DE MEDIAS (ANOVA, O KRUSKALWALLIS)
###############################################################

### PASAR INFO A DATA FRAME!!
calciodf = data.frame(calcio)

## Calcio
## ShapiroWilk normalidad
calciodf$Lote <- as.numeric(calciodf$Lote)


## Graficas
ggplot(calciodf, aes(x = calcio)) + 
  geom_histogram(aes(y = ..density.., colour = Lote)) + 
  facet_grid(. ~ Lote) + theme_bw()

qqnorm(calciodf[calciodf$Lote == 1, "calcio"], xlab = "", ylab = "", main = "CAlcio en lotes") 
qqline(calciodf[calciodf$Lote == 1, "calcio"])

##Calcioxlote

ggplot(calciodf) + geom_boxplot(aes(x = Lote, y = calcio, group=Lote)) + theme_bw()

###############################################################
## CASO1: No rechazo normalidad ni homocedasticidad
###############################################################
###H0: SON NORMALES
shapiro.test(calciodf[calcio$Lote ==1,"calcio"])
shapiro.test(calciodf[calcio$Lote ==2,"calcio"])
shapiro.test(calciodf[calcio$Lote ==3,"calcio"])
shapiro.test(calciodf[calcio$Lote ==4,"calcio"])
shapiro.test(calciodf[calcio$Lote ==5,"calcio"])
## No hay evidencia para rechazar H0 -> Asumo normalidad

### 1. H0: MEDIAS SON IGUALES
## Uso Levene para homocedasticidad

##Anderson test for normality (Video)
ad.test(calciodf[calcio$Lote ==5,"calcio"])
##
### Probar si la varianza (homogeneidad de varianza)
##Bartlett
bartlett.test(calciodf$calcio, calciodf$Lote)
bartlett.test(calcio~Lote, data=calciodf)
## 
###Levene H0 misma var. Puedo elegir media o mediana, si los datos no son normales conviene la mediana
levene.test(calciodf$calcio, calciodf$Lote)
## No rechazo h0 de homocedasticidad : CUMPLEN AMBOS

##ANOVA : Usa las columnas de a 2 como el ejemplo
## Comparing population means (3 or mas). PAra 2 medias usamos t
## H0 : medias todas iguales
## H1 : Existe una media qu eno es igual
calciodf$Lote <- as.factor(calciodf$Lote)
calcios.aov <- aov(calcio~Lote, data=calciodf)
summary(calcios.aov)
model.tables(calcios.aov, "means")
### SE rechaza la hipotesis de igualdad de medias. (O NO)
##PAra 3 variables usar z~x+y+x:y

##TEST TUKEY (Hace un t para varios) - Extremos son los que rechazan, se verifica en el boxplot
tukey.test <- TukeyHSD(calcios.aov, conf.level=0.95)
summary(tukey.test) 
tukey.test

###############################################################
## CASO2: Rechazo Homocedasticidad (Levene)
###############################################################

### SI no se cumple Homoscedasticidad (Levene rechaza), usamos boxcox
boxcox(calcio~Lote, data=calciodf, plotit = TRUE)
aov.data2=aov(calcio^(-1)~Lote, data=calciodf)
summary(aov.data2)
## Verificamos supuestos de anova transformado
shapiro.test(residuals(aov.data2))
ad.test(residuals(aov.data2))
leveneTest(calcio^(-1)~Lote, data=calciodf)
### SI SE CUMPLEN ESOS 3, EL RESULTADO DE ANOVA NOS DICE LA POSTA DE LAS MEDIAS

## Se puede usar Fligner k (No parametrico)
fligner.test(calcio~Lote, data= calciodf)

###############################################################
## CASO3: Rechazo Normalidad u Homocedasticidad
###############################################################
##Uso Kruskal Wallis
##Esta prueba contrasta la hipótesis nula que establece que las k muestras independientes proceden de la misma población y, en particular, todas ellas tienen la misma posición central. La misma se basa en los rangos de las observaciones y no requiere el cumplimiento del supuesto de normalidad ni del supuesto de homocedasticidad.

##Visualizacion
ggplot(data = datos, mapping = aes(x = condicion, y = n_huevos, colour = condicion)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = datos, mapping = aes(x = n_huevos, colour = condicion)) + 
  geom_histogram() + theme_bw() + facet_grid(. ~ condicion) + 
  theme(legend.position = "none")# + stat_bin(binwidth=30)

##Vemos el test
kruskal.test(n_huevos ~ condicion, data = datos)
kruskalmc(datos$n_huevos ~ datos$condicion)

###############################################################
## CASOS RAROS: Muestras apareadas, test t con las diffs
###############################################################
diferencia <- datos$antes - datos$despues 
datos <- cbind(datos, diferencia) 
head(datos, 4)
t.test(x = datos$antes, y = datos$despues, alternative = "two.sided", mu = 0, paired = TRUE, conf.level = 0.95)




###############################################################
######### EJERCICIO 2 #########
###############################################################

## cancer
## a - vector medio total y por grupos
cancerdf = data.frame(cancer)
summary(cancerdf)
## Benigno y maligno son clases
mediascancer = colMeans(cancerdf)

mediaspos = colMeans(cancerdf %>% filter(Class==1))
mediasneg = colMeans(cancerdf %>% filter(Class==0))

## b - comparar medidas de a una con univariado (CON TODAS LAS VARIABLES, las que rechazan son las que mejor separan y uso solo esas para LDA o QDA)
can.dat = cancerdf
t.test(x = can.dat$Adhes[can.dat$Class == 0], y = can.dat$Adhes[can.dat$Class == 1], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = can.dat$BNucl[can.dat$Class == 0], y = can.dat$BNucl[can.dat$Class == 1], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value
t.test(x = can.dat$Chrom[can.dat$Class == 0], y = can.dat$Chrom[can.dat$Class == 1], alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)$p.value


#Saco IDS
cancerdf = cancerdf[, -1]

pairs(x = cancerdf[, c("Adhes", "BNucl", "Chrom", "Epith", "Mitos", "NNucl","Thick", "UShap", "USize")], col = c("firebrick", "green3")[cancerdf$Class], pch = 19)



fit = hotelling.test( .~ Class,  data=cancerdf)
fit
cancerdf.clean = cancerdf[,c(-1)]

## Normalidad multivariada - Rechaza
mshapiro.test(t(cancerdf.clean[,-1]))

## Igualdad de matrices de var y cov - Rechaza
boxM(data = cancerdf.clean[-1], grouping=cancerdf.clean[,1])

###############################################################
## LDA
###############################################################

cancer.lda <- lda(Class~Adhes+BNucl+Chrom+Epith+NNucl+Mitos+UShap+USize,cancerdf[,-1])
cancer.lda

datos_tidy <- melt(cancerdf[,-1], value.name="valor")
kable(datos_tidy %>% group_by(variable) %>% summarise(p_value_Shapiro.test=shapiro.test(valor)$p.value))


##Visualizacion
partimat(Class~Adhes+BNucl+Chrom+Epith+NNucl+Mitos+UShap+USize, data = cancerdf.clean, method = "lda", prec = 200,image.colors = c("darkgoldenrod1", "snow2", "skyblue2","green","red","yellow","pink"), col.mean = "firebrick")

### Tasa de error ingenua (LDA)
predicciones <- predict(object = cancer.lda, newdata = cancerdf.clean[, -1]) 
table(cancerdf.clean$Class, predicciones$class, dnn = c("Clase real", "Clase predicha"))
trainig_error <- mean(cancerdf.clean$Class != predicciones$class) * 100
trainig_error

###############################################################
## QDA (Bueno cuando no se cumple Homocedasticidad)
###############################################################

cancer.qda <- qda(Class~Adhes+BNucl+Chrom+Epith+NNucl+Mitos+UShap+USize, data = cancerdf.clean)
cancer.qda

## ERROR INGENUO (QDA)
prediccionesq <- predict(object = cancer.qda, newdata = cancerdf.clean[, -1]) 
table(cancerdf.clean$Class, prediccionesq$class, dnn = c("Clase real", "Clase predicha"))
trainig_error <- mean(cancerdf.clean$Class != prediccionesq$class) * 100
trainig_error

###############################################################
## Cross Validation y Heldout
###############################################################


## TEST BOX M para varianzas y covarianzas, no lo piden pero bueno
#boxM(Y = data[,2],data=cancerdf[,-1 -2], grouping = cancerdf[, 2])

###############################################################
############## CLUSTERING #############
###############################################################
View(city)

###############################################################
## JERARQUICO
###############################################################

mat_dist = dist(city[, -1], method='euclidean')
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
rect.hclust(hc_average, k=3, border="red")

grupos<-cutree(hc_average,k=3)
split(rownames(city),grupos)

#################


datos2 <- city 
set.seed(101) 
hc_completo <- datos2[,-1] %>% scale() %>% dist(method = "euclidean") %>% 
  hclust(method = "average") 
fviz_dend(x = hc_completo, k = 5, cex = 0.6) + 
  geom_hline(yintercept = 5.5, linetype = "dashed")


fviz_cluster(object = list(data = city[,-1], cluster = cutree(hc_completo, k = 5)), ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE) + 
  theme_bw()


###############################################################
## K MEANS
###############################################################

##Optimal clusters
rownames(datos2) <- datos2[,1] ## no se como hacer esto
fviz_nbclust(x = datos2[,-1], FUNcluster = kmeans, method = "wss", 
             diss = dist(datos2, method = "euclidean")) + 
  geom_vline(xintercept = 3, linetype = 2)

set.seed(123)
km_clusters <- kmeans(x = datos2[,-1], centers = 3, nstart = 25)
names(km_clusters)
split(rownames(datos2),km_clusters$cluster)

fviz_cluster(object = km_clusters, data = datos2[,-1], show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) + 
  theme_bw() + 
  theme(legend.position = "none")
