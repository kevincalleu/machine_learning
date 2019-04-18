####EJEMPLO CLASIFICADOR NAIVE BAYES #####

# Se instalan los paquetes necesarios

#install.packages("tm")
library(tm)

#install.packages("e1071")
library(e1071)

#### 1. PREPARACIÓN DE LOS DATOS  ####

# Se leen los datos del archivo "house-votes-84.csv"
datos <- read.csv("/Users/Kevin/OneDrive - UNIR/01_Aprendizaje Automático/Documentación/b5235f49-Aprendizaje_Automatico_MIA__PER1_20182019-kevincalleu@gmail.com/Actividades/Tarea 1/house-votes-84.csv")

# Se ordenan las columnas de los datos
datos <- datos[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,1,17)]

# Se categorizan los datos
datos[datos == "-1"] <- factor("no")
datos[datos == "1"] <- factor("yes")

# Se observa el formato del dataset tanto en el inicio como en el final
head(datos)
tail(datos)

# Se convierte la columna party a tipo categórica
datos$party <- as.factor(datos$party)

# Se visualiza la distribución de cada clase
summary(datos$party)

# Se visualiza la distribución porcentual de cada clase
prop.table(table(datos_limpios$party))

# Se comprueba que no haya espacios en blanco y se eliminan estos en caso de existir
any(is.na(datos))
datos_limpios <- na.omit(datos)


#### 2. ENTRENAMIENTO Y PRUEBA DEL MODELO  ####

# Creación de datos de entrenamiento con el 70% de los datos
datos_train <- datos_limpios[1:304, ]

# Creación de datos de test con el 30% de los datos
datos_test <- datos_limpios[305:435, ]

#Se observa si se mantienen las proporciones en los conjuntos creados
prop.table(table(datos_train$party))
prop.table(table(datos_test$party))

# Entrenamos el modelo con los datos de datos_train
clasificador <- naiveBayes(datos_train, datos_train$party)

# Evaluamos el modelo con el conjunto datos_test
datos_predichos <- predict(clasificador, datos_test)

# Matriz de confusión 

table(datos_predichos, datos_test$party)
prop.table(table(datos_predichos, datos_test$party))

# Probabilidad para cada uno de los eventos
datos_predichos <- predict(clasificador, datos_test, type = "raw")

# Se grafican los hitogramas 
df_datos <- as.data.frame(datos_predichos)
hist(df_datos$democrat)
hist(df_datos$republican)
  
