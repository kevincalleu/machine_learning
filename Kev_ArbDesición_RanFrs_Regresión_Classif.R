#### Ejercicio de Árboles de decisión y Random Forest para regresión y clasificación

# Integrantes:
      # - Kevin Calle
      # - Javier Parra
      # - Sergio Santoyo
      # - Verónica Chimbo

# 1. ÁRBOLES DE DESICIÓN

############################################   REGRESIÓN  ##############################

# Se importan las librerías
library(randomForest)
library(gmodels)
library(tree)
library(randomForest)


# Se establece una semilla para reproducir los datos
set.seed(123)

# Cargamos los datos 
input.folder <- "/Users/Kevin/OneDrive - UNIR/01_Aprendizaje Automático/Documentación/b5235f49-Aprendizaje_Automatico_MIA__PER1_20182019-kevincalleu@gmail.com/Actividades/Laboratorio 2 Random Forest_arboles de desición/"
cnt.data <- read.csv(paste0(input.folder, "hour.csv"))

# Se observan las características de los datos
str(cnt.data)
summary(cnt.data)

# Se observa el formato del dataset tanto en el inicio como en el final
head(cnt.data)
tail(cnt.data)

# Se elimina la fecha que es de tipo factor y no aporta al sistema
data_necesaria <- cnt.data[ ,!colnames(cnt.data)=="dteday"]
str(data_necesaria)
summary(data_necesaria)


# Se crea una función para normalizar los datos del dataframe
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))  
}

# Se aplica la función de normalización en los datos del dataframe
data.norm <- as.data.frame(lapply(data_necesaria, normalize))
data.scale <- as.data.frame(scale(data_necesaria))
str(data.norm)

# Se crea un conjunto de train con el 80% de los datos
train <- sample(1:nrow(data.norm), nrow(data.norm)/1.25)
test <- data.norm[-train, ]

# Se entrena el modelo con árbol de decisión
tree.cnt_regresion <- tree(cnt ~ . , data.norm[train, ]) 

# Se muestran las características del modelo árbol de desición entrenado
summary(tree.cnt_regresion)

# Vemos que solo se han utilizado 2 variables: registered, casual

# Se realiza el gráfico del árbol de decisión
plot(tree.cnt_regresion)
text(tree.cnt_regresion, pretty = 0)

# usemos cvtree() para ver si podando el arbol se mejora
cv.cnt_regresion <- cv.tree(tree.cnt_regresion, K = 10)
plot(cv.cnt_regresion$size, cv.cnt_regresion$dev, type = 'b')

# vamos a podar el arbol
prune.cnt_regresion <- prune.tree(tree.cnt_regresion, best = 6)
plot(prune.cnt_regresion)
text(prune.cnt_regresion, pretty = 0)

# hagamos una prediccion con el árbol sin podar
pred_tree_regresion <- predict(tree.cnt_regresion, newdata = test)
valores_reales <- data.norm[-train, "cnt"]
plot(pred_tree_regresion, valores_reales, main = "arbol sin podar")
abline(0, 1)

# Predicción con árbol podado
pred_prune_tree_regresion <- predict(prune.cnt_regresion, newdata = data.norm[-train, ])
plot(pred_prune_tree_regresion, valores_reales, main = "arbol podado")
abline(0, 1)

# Métricas de evaluación modelo sin podar
MSE <- mean((pred_tree_regresion - valores_reales)^2)
MSE
RMSE <- sqrt(mean((pred_tree_regresion - valores_reales)^2))
RMSE

# Métricas de evaluación modelo podado
MSE <- mean((pred_prune_tree_regresion - valores_reales)^2)
MSE
RMSE <- sqrt(mean((pred_prune_tree_regresion - valores_reales)^2))
RMSE

############################################   CLASIFICACIÓN  ##############################

# Discretizamos la variable cnt. Se crea una nueva variable ventas 
# con valores discretos alta, baja en función de cnt. 
# Baja venta si cnt <= 20
# Alta venta si cnt > 20

ventas <- ifelse(data_necesaria$cnt <= 20, "Baja", "Alta")
ventas <- as.factor(ventas)
data_cnt_factor <- data.frame(data.norm, ventas)
data_cnt_factor <- data_cnt_factor[ ,!colnames(data_cnt_factor)=="cnt"]
summary(data_cnt_factor)


# Vamos a construir un arbol de clasificacion para predecir ventas
# utilizando todas las variables, excepto cnt
# El training error es del 0.06% (Misclassifcation error rate)
# Se va a entrenar el árbol con el 80% de los datos
tree_clasification <- tree(ventas ~ ., data_cnt_factor, subset = train)
summary(tree_clasification)

# Se realiza el gráfico del árbol de clasificación
plot(tree_clasification)
text(tree_clasification, pretty = 0)

# El indicador mas importante para las ventas parace ser los usuarios registrados y casuales.

# Si ejecutamos el nombre del arbol, R nos muestra cada una de las ramas del arbol
# junto con el criterio de split, el numero de observaciones en cada rama, la desviacion,
# la prediccion completa para la rama (yes o no) y la fraccion de observaciones que en esa
# rama toman valores baja o alta.
tree_clasification

# Vamos a evaluar el arbol con conjuntos de test que son el 20% de los datos

tree_pred_clasification <- predict(tree_clasification, test, type = "class")

# se visualiza la matriz de confusión del modelo
ventas_test <- ventas[-train]
table(tree_pred_clasification, ventas_test)
prop.table(table(tree_pred_clasification, ventas_test), margin = 2)

# Precision
precision <- (2870 + 583) / 3476
precision


# Veamos si podando el arbol se puede mejorar. 
# la funcion cv.tree() realiza cross-validation para determinar el nivel optimo
# de la complejidad del arbol
# con FUN=prune.misclass indicamos que el criterio a optimizar es el error
# en clasificacion. 
# Por defecto K == 10

cv_cnt_clasificacion <- cv.tree(tree_clasification, FUN = prune.misclass)
names(cv_cnt_clasificacion)
cv_cnt_clasificacion

# El atributo dev indica el cross-validation error. Vamos a pintar el 
# error rate como una funcion de size y k
par(mfrow = c(1,2))
plot(cv_cnt_clasificacion$size, cv_cnt_clasificacion$dev, type = "b")
plot(cv_cnt_clasificacion$k,    cv_cnt_clasificacion$dev, type = "b")

# Ahora aplicamos la funcion prune.misclass() para podar el arbol y 
# obtener aquel con 4 nodos

prune_cnt_clasificacion <- prune.misclass(tree_clasification, best = 4)

par(mfrow = c(1,1))
plot(prune_cnt_clasificacion)
text(prune_cnt_clasificacion, pretty = 0)

# Veamos el comportamiento de este arbol en el test data
pred_tree_prune_classification <- predict(prune_cnt_clasificacion, test, type = "class")
table(pred_tree_prune_classification, ventas_test)
prop.table(table(pred_tree_prune_classification, ventas_test), margin = 2)

precision <- (2846 + 592) / 3476
precision


# 1. RANDOM FOREST

############################################   REGRESIÓN  ##############################

# Se observa la data
str(data.norm)

# Se ordena de forma aleatoria la información
rf_cnt_rand  <- data.norm[order(runif(17379)),]
rf_cnt_train <- rf_cnt_rand[1:13903, ]
rf_cnt_test  <- rf_cnt_rand[13904:17379, ]

# Se crea el modelo random forest
# El OOB es un estimador razonable de error en test/validation
rf.model <- randomForest(x = rf_cnt_train[, -16],
                         y = rf_cnt_train$cnt,
                         data = rf_cnt_train, 
                         ntree = 500,
                         do.trace= T)

# Se observan las características del modelo random forest creado
rf.model
plot(rf.model)

# Podemos ver la importancia de las variables
varImpPlot(rf.model)

# hacemos una prediccion en test
rf.pred <- predict(rf.model, rf_cnt_test)
head(rf.pred)
head(rf_cnt_test)
# Se muestran las características del modelo árbol de desición entrenado
summary(rf.pred)

# Se realiza el gráfico del árbol de decisión
plot(rf.pred, rf_cnt_test$cnt, main = "Modelo Regresión con Random forest")
abline(0, 1)

# Métricas de evaluación modelo Random Forest
MSE <- mean((rf.pred - rf_cnt_test$cnt)^2)
MSE
RMSE <- sqrt(mean((rf.pred - rf_cnt_test$cnt)^2))
RMSE

############################################   CLASIFICACIÓN  ##############################

# Se observa la data
str(data_cnt_factor)

data_cnt_factor <- data_cnt_factor[ ,!colnames(data_cnt_factor)=="cnt"]

# Se ordena de forma aleatoria la información
rf_classification_cnt_rand  <- data_cnt_factor[order(runif(17379)),]
rf_classification_cnt_train <- rf_classification_cnt_rand[1:13903, ]
rf_classification_cnt_test  <- rf_classification_cnt_rand[13904:17379, ]

# Se crea el modelo random forest
# El OOB es un estimador razonable de error en test/validation
rf.model_classification <- randomForest(x = rf_classification_cnt_train[, -16],
                         y = rf_classification_cnt_train$ventas,
                         data = rf_classification_cnt_train, 
                         ntree = 500,
                         do.trace= T)

# Se observan las características del modelo random forest creado
rf.model_classification
plot(rf.model_classification)

# Podemos ver la importancia de las variables
varImpPlot(rf.model_classification)

# hacemos una prediccion en test
rf_classification_pred <- predict(rf.model_classification, rf_classification_cnt_test)
head(rf_classification_pred)
head(rf_classification_cnt_test)
# Se muestran las características del modelo árbol de desición entrenado
summary(rf_classification_pred)

# Se realiza la matriz de confusión
table(rf_classification_pred, rf_classification_cnt_test$ventas)
prop.table(table(rf_classification_pred, rf_classification_cnt_test$ventas), margin = 2)

# Se calcula la precisión del modelo.
precision <- (2855 + 613) / 3476
precision

