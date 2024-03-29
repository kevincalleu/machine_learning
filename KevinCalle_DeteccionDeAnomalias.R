#### Detecci�n de Anomal�as ####

## ESTUDIANTE: Kevin Fabricio Calle Urgilez

# Se importan las LIBRER�AS necesarias para este programa

# Para MAC
# install.packages("http://download.r-forge.r-project.org/src/contrib/IsolationForest_0.0-26.tar.gz", repo=NULL, type="source")

# Para Windows
#install.packages("IsolationForest", repos="http://R-Forge.R-project.org")

# Cargamos la librer�a
library(IsolationForest)
library(cluster)

# Se Leen los datos
diagnostic <- read.csv("/Users/Kevin/OneDrive - UNIR/01_Aprendizaje Autom�tico/Documentaci�n/b5235f49-Aprendizaje_Automatico_MIA__PER1_20182019-kevincalleu@gmail.com/Actividades/Trabajo 3 Detecci�n de anomal�as/wdbc.data.txt")
head(diagnostic)
summary(diagnostic)

# Se muestra la distribuci�n de los datos. B=benigno, M=maligno
table(diagnostic$M)

# Se realiza el gr�fico de la variable diagn�stico frente a otra
# variable (8).
plot(diagnostic[,2], diagnostic[,8])

# Segmentaci�n
# Se toma en cuenta las 30 variables que contienen informaci�n de
# los datos de entrada procesados
# Se omiten las varibles ID y Diagnostic porque son de IDENTIFICACI�N
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
table(score$outF) # Se observa que hay 10 valores an�malos y 558 valores normales.
# Los valores an�malos superan el 0.5 del porcentaje de anomal�a.


# Clasificaci�n de los datos con K-MEANS

# Se van a utilizar 2 clusters para clasificar los datos como Benigno o Maligno.
# Se establece una semilla para replicar los datos
set.seed(123)

# Se clasifican los datos en dos clases aplicando K-MEANS
wdbc_clusters <- kmeans(wdbc_norm, 2)

# Se realiza el gr�fico de los cl�steres
clusplot(wdbc_norm,
         wdbc_clusters$cluster, 
         color=TRUE,
         col.clus=c(1:3)[unique(wdbc_clusters$cluster)],
         shade=TRUE,
         labels=4, 
         lines=0, 
         main = "Bivariate Cluster Plot")

# Se observa el n�mero de elementos en cada cluster
# Vemos que se clasifican B=380 y M=188 
distribution_cluster = wdbc_clusters$size
distribution_cluster

# Se muestra la distribuci�n de los datos original
# B=357 y M=211 
table(diagnostic$M)

# Se calcula la Efectividad de la clasificaci�n
# Se acerto un 93% de las muestras calsificadas como benignas
(357*100)/380

# Se acerto un 89% de las muestras como malignas
(188*100)/211
