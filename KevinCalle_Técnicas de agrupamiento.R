#### Ejercicio de t�cnicas de agrupamiento ####
#### K-MEANS, CLUSTER JER�RQUICO ####

## ESTUDIANTE: Kevin Fabricio Calle Urgilez


#######################  1. K-MEANS  #######################

# Se importan las LIBRER�AS necesarias para este programa

library(cluster)
require(graphics)
library(fpc)

# Se Leen los datos
datos_clientes <- read.csv("/Users/Kevin/OneDrive - UNIR/01_Aprendizaje Autom�tico/Documentaci�n/b5235f49-Aprendizaje_Automatico_MIA__PER1_20182019-kevincalleu@gmail.com/Actividades/Tarea_4_T�cnicasDeAgrupamiento/Wholesale customers data.csv")
head(datos_clientes)
summary(datos_clientes)

# Se muestra la distribuci�n de los datos
table(datos_clientes$Channel)
table(datos_clientes$Region)

summary(datos_clientes$Channel)
summary(datos_clientes$Region)

# Segmentaci�n
# Se toma en cuenta las 5 variables que contienen informaci�n de
# la venta en unidades monetarias de los productos
# Se omiten las varibles Channel y Region porque son de IDENTIFICATION
products <- datos_clientes[3:8]
summary(products)

# Se normalizan los datos
products_norm <- as.data.frame(lapply(products, scale))
summary(products_norm)

# Se elige el n�mero de cluster adecuado comparando la suma del error
# cuadr�tico (SSE) por el n�mero de clusteres

mydata <- products
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main = "Prueba del codo SSE vs clusters")

# Se van a utilizar 5 clusters que es el parece ser el n�mero �ptimo
# obtenido en el codo SSE vs cl�steres
set.seed(1234)
clients_clusters <- kmeans(products_norm, 5)

# Se realiza el gr�fico de los cl�steres
clusplot(products_norm,
         clients_clusters$cluster, 
         color=TRUE,
         col.clus=c(1:3)[unique(clients_clusters$cluster)],
         shade=TRUE,
         labels=4, 
         lines=0, 
         main = "Bivariate Cluster Plot")

plotcluster(products_norm, clients_clusters$cluster, main = "Distribuci�n de los cl�steres")

# Se observa el n�mero de elementos en cada segmento
clients_clusters$size

# Se observan los centroides de los clusteres
clients_clusters$centers


################  2. CLUSTERING JER�RQUICO  ################

# Se halla la DISTANCIA entre las variables
distance <- dist(products_norm)
hiracluster <- hclust(distance, "ave")

# Se realiza el gr�fico del dendrograma
plot(hiracluster)
plot(hiracluster, hang = -1)

# Se poda el �rbol. Se van a crear 10 grupos.
memb <- cutree(hiracluster, k = 10)
cent <- NULL
for(k in 1:10){
  cent <- rbind(cent, colMeans(products_norm[memb == k, , drop = FALSE]))
}
poda_hiracluster <- hclust(dist(cent)^2, method = "cen", members = table(memb))

# Se realiza el gr�fico de los �rboles
opar <- par(mfrow = c(1, 2))
plot(hiracluster,  labels = FALSE, hang = -1, main = "Original Tree")
plot(poda_hiracluster, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
par(opar)
