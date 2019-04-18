## install.package("MASS")
library(MASS)
## install.packages("readxl")
library(readxl)
## install.packagea("neuralnet")
library(neuralnet)
## install.packagea("e1071")
library(e1071)


# Leer el fihero de excel
data_read<-read_excel("/Users/Kevin/OneDrive - UNIR/01_Aprendizaje Automático/Documentación/b5235f49-Aprendizaje_Automatico_MIA__PER1_20182019-kevincalleu@gmail.com/Actividades/Tarea 2 Laboratorio/auto-mpg.xlsx",
                      col_names = FALSE,col_types = "numeric")
# Seleccionar solo las columnas numericas
data<-data_read[,1:8]

# Poner nombres a las columnas segun los datos
colnames(data) <- c("miles_per_gallon","cylinders","displacement",
                    "horsepower","weight","acceleration",
                    "model_year","origin")
# Resumen de los datos
str(data)
summary(data)

## Tratamiento de datos

# Eliminar registros NA
data <- na.omit(data)
str(data)
summary(data)

# Indices de correlacion
cor(data)

# Normalizacion de datos en rango de 0 a 1
fn_normalizar <- function (x) {
  return ( (x - min(x)) / (max(x) - min(x)))
}
# Desnormalizacion de datos
fn_desnormalizar <- function (n, max, min) {
  return ( n * (max - min)) + min
}

data.normalizada <- as.data.frame(lapply(data, fn_normalizar))
str(data)
head(data.normalizada$miles_per_gallon)

# Separar en conjuntos de entrenamiento y pruebas
set.seed(123)
size <- floor(0.75 * nrow(data.normalizada))
size
index <- sample(seq_len(nrow(data)), size = size)
index
data.test <- data.normalizada[-index, ]
data.train <- data.normalizada[index, ]

##################################
# Generar modelo de SVM         #
#################################

# Generamos el modelo
data.svm_model1 <- svm(miles_per_gallon ~., data.train)
# Generamos las predicciones
data.svm_predict1 <- predict(data.svm_model1, data.test[, 2:8])

# Calculamos RMSE y MSE
svm_rmse1 <- sqrt(mean((data.test$miles_per_gallon - data.svm_predict1)^2))
svm_rmse1
svm_mae1 <- mean(abs(data.test$miles_per_gallon - data.svm_predict1))
svm_mae1

# Mejora del modelo mediente tunning
# Tunning
data.svm_tune <- tune(svm, miles_per_gallon ~., data = data.train,
                 ranges = list(epsilon = seq(0, 0.1, 0.01), cost = 1+(0:10))
)
print(data.svm_tune)
# Escojemos el mejor modelo del tunning
data.svm_model2 <- data.svm_tune$best.model
# Realizamos las predicciones
data.svm_predict2 <- predict(data.svm_model2, data.test[, 2:8]) 

# Calculo de errores
svm_rmse2 <- sqrt(mean((data.test$miles_per_gallon - data.svm_predict2)^2))
svm_rmse2
svm_mae2 <-  mean(abs(data.test$miles_per_gallon - data.svm_predict2))
svm_mae2

# Dibujamos los resultados para ver cual se acerca mas a la linea
# este sera el que mejor rendimeinto tenga
plot(data.test$miles_per_gallon, data.svm_predict1, col='red', main='Real vs Predicciones',
     pch=18, cex=0.7)
points(data.test$miles_per_gallon, data.svm_predict2, col='blue',
       pch=18, cex=0.7)
abline(0, 1, lwd=2)
legend('bottomright', legend=c('SVM', 'SVM Tunned'), pch=18, col=c('red', 'blue'))


 ##################################
# Generar modelo de red neuronal #
#################################

# Generamos le modelo inicial con una capa oculta de 4 neuronas
# Generamos una semillas para repetir los numeros randomiscos de los pesos 
# y obtener los mismos resultados
set.seed(123)
data.ann_model1 <- neuralnet(miles_per_gallon ~ cylinders + displacement
                            + horsepower + weight + acceleration
                            + model_year + origin, data.train,
                             linear.output = T, hidden = c(4))

# Mostramos el modelo
plot(data.ann_model1)
# Realizamos las predicciones
data.ann_predict1 <- compute(data.ann_model1, data.test[,2:8])

# Calculo de errores
ann_rmse1 <- sqrt(mean((data.test$miles_per_gallon - data.ann_predict1$net.result)^2))
ann_rmse1
ann_mae1 <- mean(abs(data.test$miles_per_gallon - data.ann_predict1$net.result))
ann_mae1

# Generar modelo de red neuronal con mas capas para reducir el error
set.seed(123)
data.ann_model2 <- neuralnet(miles_per_gallon ~ cylinders + displacement
                             + horsepower + weight + acceleration
                             + model_year + origin, data.train,
                             linear.output = T, hidden = c(8, 8))
# Dibujamos el modelo
plot(data.ann_model2)

# Realizamos las predicciones
data.ann_predict2 <- compute(data.ann_model2, data.test[,2:8])

# Calculo de errores
ann_rmse2 <- sqrt(mean((data.test$miles_per_gallon - data.ann_predict2$net.result)^2))
ann_rmse2
ann_mae2 <- mean(abs(data.test$miles_per_gallon - data.ann_predict2$net.result))
ann_mae2

# Dibujamos los resultados para ver cual se acerca mas a la linea
# este sera el que mejor rendimeinto tenga

plot(data.test$miles_per_gallon, data.ann_predict1$net.result, col='red', main='Real vs Predicciones',
     pch=18, cex=0.7)
points(data.test$miles_per_gallon, data.ann_predict2$net.result, col='blue',
       pch=18, cex=0.7)
abline(0, 1, lwd=2)
legend('bottomright', legend=c('ANN h=c(4)', 'ANN h=c(8,8)'), pch=18, col=c('red', 'blue'))

# Mostrar errores
paste("SVM1 rmse", svm_rmse1, " ", fn_desnormalizar(svm_rmse1, max(data$miles_per_gallon), min(data$miles_per_gallon)))
paste("SVM1 mae", svm_mae1, " ", fn_desnormalizar(svm_mae1, max(data$miles_per_gallon), min(data$miles_per_gallon)))

paste("SVM2 rmse", svm_rmse2, " ", fn_desnormalizar(svm_rmse2, max(data$miles_per_gallon), min(data$miles_per_gallon)))
paste("SVM2 mae", svm_mae2, " ", fn_desnormalizar(svm_mae2, max(data$miles_per_gallon), min(data$miles_per_gallon)))

paste("ANN1 rmse", ann_rmse1, " ", fn_desnormalizar(ann_rmse1, max(data$miles_per_gallon), min(data$miles_per_gallon)))
paste("ANN1 mae", ann_mae1, " ", fn_desnormalizar(ann_mae1, max(data$miles_per_gallon), min(data$miles_per_gallon)))

paste("ANN2 rmse", ann_rmse2, " ", fn_desnormalizar(ann_rmse2, max(data$miles_per_gallon), min(data$miles_per_gallon)))
paste("ANN2 mae", ann_mae2, " ", fn_desnormalizar(ann_mae2, max(data$miles_per_gallon), min(data$miles_per_gallon)))
