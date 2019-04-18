#### Detección de Anomalías ####

## ESTUDIANTE: Kevin Fabricio Calle Urgilez

# Se importan las LIBRERÍAS necesarias para este programa

# Para MAC
# install.packages("http://download.r-forge.r-project.org/src/contrib/IsolationForest_0.0-26.tar.gz", repo=NULL, type="source")

# Para Windows
#install.packages("IsolationForest", repos="http://R-Forge.R-project.org")

# Cargamos la librería
library(IsolationForest)
library(cluster)

# Se Leen los datos
diagnostic <- read.csv("/Users/Kevin/OneDrive - UNIR/01_Aprendizaje Automático/Documentación/b5235f49-Aprendizaje_Automatico_MIA__PER1_20182019-kevincalleu@gmail.com/Actividades/Trabajo 3 Detección de anomalías/wdbc.data.txt")
head(diagnostic)
summary(diagnostic)

# Se muestra la distribución de los datos. B=benigno, M=maligno
table(diagnostic$M)

# Se realiza el gráfico de la variable diagnóstico frente a otra
# variable (8).
plot(diagnostic[,2], diagnostic[,8])

# Segmentación
# Se toma en cuenta las 30 variables que contienen información de
# los datos de entrada procesados
# Se omiten las varibles ID y Diagnostic porque son de IDENTIFICACIÓN
wdbc <- diagnostic[3:32]
summary(wdbc)

# Se normalizan los datos
wdbc_norm <- as.data.frame(lapply(wdbc, scale))
summary(wdbc_norm)

# Entrenamos un modelo de Isolation Forest
model <- IsolationTrees(wdbc_norm, rFactor = 0)
model

# Obtenemos el Score de anomalia
score <- AnomalyScore(wdbc_norm, model)

# Mostamos el score
score$outF
summary(score$outF)
table(score$outF) # Se observa que hay 10 valores anómalos y 558 valores normales.
# Los valores anómalos superan el 0.5 del porcentaje de anomalía.


# Clasificación de los datos con K-MEANS

# Se van a utilizar 2 clusters para clasificar los datos como Benigno o Maligno.
# Se establece una semilla para replicar los datos
set.seed(123)

# Se clasifican los datos en dos clases aplicando K-MEANS
wdbc_clusters <- kmeans(wdbc_norm, 2)

# Se realiza el gráfico de los clústeres
clusplot(wdbc_norm,
         wdbc_clusters$cluster, 
         color=TRUE,
         col.clus=c(1:3)[unique(wdbc_clusters$cluster)],
         shade=TRUE,
         labels=4, 
         lines=0, 
         main = "Bivariate Cluster Plot")

# Se observa el número de elementos en cada cluster
# Vemos que se clasifican B=380 y M=188 
distribution_cluster = wdbc_clusters$size
distribution_cluster

# Se muestra la distribución de los datos original
# B=357 y M=211 
table(diagnostic$M)

# Se calcula la Efectividad de la clasificación
# Se acerto un 93% de las muestras calsificadas como benignas
(357*100)/380

# Se acerto un 89% de las muestras como malignas
(188*100)/211
