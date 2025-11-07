# paso 1 Cargar datos historicos

Datos_Seguros <- read.csv("~/Seguros_2.csv", sep=";", stringsAsFactors = TRUE)

#Paso 2 codificar variable de acuerdo a naturaleza de las mismas
str(Datos_Seguros)

#En este caso todas las variables estan correcta

#Paso 3 eliminar variables de valor unico 
#No se eliminan variables

#Paso 4 Identificar si hay variables nulas si las hay debemos eliminarlas o inputarlas

sum(is.na(Datos_Seguros))


#Si tuvieramos que eliminarlos
Datos_Seguros <- na.omit(Datos_Seguros)



#Paso #5 dividir los datos historicos en entrenamiento (85) y pruebas (15)

tam<- dim(Datos_Seguros)[1]
tam
muestra <- sample(1:tam,round(tam*0.15))


Tabla_Entrenamiento <- Datos_Seguros[-muestra,]

Tabla_Pruebas <- Datos_Seguros[muestra,]



#Paso #6 ejecutar los modelos de machine learning

library(kknn)
library(nnet)
library(randomForest)



modelo_kknn <- train.kknn(data = Tabla_Entrenamiento, formula = Fraude~.,kmax=round(sqrt(tam)))
prediccion_kknn <- predict(modelo_kknn,Tabla_Pruebas[,-12])
length(prediccion_kknn)
  
#Bosques aleatorios

modelo_bosques <- randomForest(Fraude~., Tabla_Entrenamiento, importance=TRUE,ntree=15, mtry=6)
prediccion_bosques <- predict(modelo_bosques, Tabla_Pruebas[,-12])


length(prediccion_bosques)
  

#Redes Neuronales

modelo_redes <- nnet(Fraude~., data=Tabla_Entrenamiento, size=15)
prediccion_redes <- predict(modelo_redes, Tabla_Pruebas[,-12], type="class")
length(prediccion_redes)




#paso 7 evaluar los modelos

MC_KKNN <- table(Tabla_Pruebas[,12],prediccion_kknn)
MC_Bosques <- table(Tabla_Pruebas[,12],prediccion_bosques)
MC_Redes <- table(Tabla_Pruebas[,12],prediccion_redes)


precision_global_kknn <- sum(diag(MC_KKNN)/sum(MC_KKNN))
precision_global_bosques <- sum(diag(MC_Bosques)/sum(MC_Bosques))
precision_global_redes <- sum(diag(MC_Redes)/sum(MC_Redes)) 
  
  
  
precision_por_categoria_kknn <- diag(MC_KKNN)/rowSums(MC_KKNN)
precision_por_categoria_bosques <- diag(MC_Bosques)/rowSums(MC_Bosques)  
precision_por_categoria_redes <- diag(MC_Redes)/rowSums(MC_Redes)  
  
  
Datos_para_Predecir <- read.csv("C:/Users/brice/Downloads/SegurosNuevos_2.csv", sep=";", stringsAsFactors=TRUE)


Datos_para_Predecir$ID <- NULL


str(Tabla_Entrenamiento)
str(Datos_para_Predecir)


Datos_para_Predecir$Record <- factor(Datos_para_Predecir$Record, levels = levels(Tabla_Entrenamiento$Record))

Datos_para_Predecir$EstadoCivil <- factor(Datos_para_Predecir$EstadoCivil, 
                                          levels = levels(Tabla_Entrenamiento$EstadoCivil))

prediccion_final <- predict(modelo_bosques, Datos_para_Predecir[,-12])
length(prediccion_final)

Datos_para_Predecir$Fraude <- prediccion_final









  