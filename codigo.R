#instalar y cargar paquetes
instalar_carga <- function (packages)  {   
  
  # Determinamos los paquetes instalados
  for(package in packages){
    
    # si cada paquete esta instalado se carga
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    
    # Si el paquete no está instalado localmente, descárguelo y luego cárguelo
    else {
      install.packages(package, dependencies = TRUE)
      do.call("library", list(package))
    }
  } 
}


# carga de bibliotecas
libs <- c("ggplot2", "maps", "plotly", "plyr", "dplyr", "rworldmap","stringr","lubridate", "plotly", "reshape2", "magrittr", "ggthemes", "tidyr", "DT", "lubridate","RColorBrewer")
instalar_carga (libs)

#Métodos específicos de carga de bibliotecas
libs.methods <- c("C50", "lattice", "caret", "nnet", "e1071","Matrix", "foreach","glmnet","C50","randomForest","ipred","rpart")
instalar_carga (libs.methods)

#carga de datos
getwd()
setwd("D:\\maria")
data <- read.csv("D:\\maria/survey.csv")


#LIMPIEZA DE DATOS
# ELIMINAMOS DATOS NO IMPORTANTES PARA EL FIN
data <- data[ , !(names(data) %in% "state")]
data <- data[ , !(names(data) %in% "Timestamp")]
data <- data[ , !(names(data) %in% "comments")]
data <- data[ , !(names(data) %in% "self_employed")]


# Unificacionn de genero
data$Gender %<>%  str_to_lower()

male_str <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man", "cis male")
trans_str <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means" )
female_str <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail")

data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% male_str) "male" else x )
data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% female_str) "female" else x )
data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% trans_str) "trans" else x )
data %<>% filter(Gender != "a little about you")
data %<>% filter(Gender != "guy (-ish) ^_^")
data %<>% filter(Gender != "p")


# categorizacion de edad
data$Age<-cut(data$Age, c(-Inf,20,35,65,Inf))

# NA valora la detección y eliminación de la fila.
sapply(data, function(x) sum(is.na(x)))
data <- data[!is.na(data$work_interfere),]

# Guardar los datos originales con todas las variables importantes
data.origin <- data

#Comparación de variabilidad entre categorías de variables
#matriz de covarianza
for(i in 1:length(data)){
  aux <- prop.table(table(data$treatment, data[,i]), 1)*100 
  percent <- round(max(abs(aux[1,]-aux[2,])), digits = 2)
  
  if(percent > 10 & percent < 99){
    
    #preparacion de la data para l visualizacion
    aux <- prop.table(table(data$treatment, data[,i]), 1)*100 
    nom <- colnames(aux)
    type <- c(rep("Yes",ncol(aux)),rep("No",ncol(aux)))
    val <- append(aux[1,], aux[2,])
    data.aux<-data.frame(nom=nom,type=type ,val=val)
    
    # Usamos la libreria ggplot2 para la visualizacion
    g <- ggplot() + geom_bar(data=data.aux,aes(x=nom, y=val,fill=type),stat='identity',position='dodge')+
      coord_flip() +
      labs(
        x = "Importance",
        y = "",
        title = paste("Comparativa de salud mental sobre ", names(data[i]), sep=""),
        subtitle = paste("El más diferente es ", percent, "%", sep=""),
        caption = "\nDeterminar por matriz de covarianzas"
      ) %>% suppressWarnings()
    print(g)
  }
  
}
# Selección de variables con mayor variabilidad
data <- data.frame(gender= data$Gender,
                   family_history= data$family_history,
                   work_interfere= data$work_interfere,
                   benefits= data$benefits, 
                   care_options= data$care_options,
                   anonymity= data$anonymity,
                   treatment=data$treatment)

#Preparación de la función de regresión 
regresion <- treatment~
  gender+
  family_history+
  work_interfere+
  benefits+
  care_options+
  anonymity


#Ahorro de porcentaje de predicción

percent <- data.frame(methods=c("Arbol de clasificacion", "redes nuronales"), value=c(0,0))

# datos de prueba y testing
set.seed(101)
n <- nrow(data)
data.index <- sample(1:n , size=round(n*0.7))
train <- data[data.index,]
test <- data[-data.index,]

# ejecutando el modelo c5 por libreria
model <- C5.0( treatment ~ . , data = train)

# prediccioon
prediction <- predict(model,newdata=test)

# matriz de confusion
( mc <- table(prediction, test$treatment) )

# Porcentaje exitoso de clasificación
( percent$value[1] <- sum(diag(mc)) / sum(mc) * 100 )
#visualizacion del modelo
model <- C5.0( treatment ~ . , data = train)
plot(model)