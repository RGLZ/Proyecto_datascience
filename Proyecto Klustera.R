library (readr) #llamar a la librería readr
library (tidyverse) #llamar a la librería tidyverse
#Lectura y asignación del dataframe a e_dataframe
e_dataframe <- read.csv("~/Proyecto_datascience/e.csv")

#pregunta1 - Cúal es la sucursal que recibe más conexiones
por_sucursal <- group_by(e_dataframe, branch_office)
conexiones <- count(por_sucursal)
#conexionesx<-as.character(conexiones$branch_office)

#Pregunta 2# ¿Qué día de la semana tenemos más visitantes?
visitantes <- filter(e_dataframe,visitor=="true")
dia <- group_by(visitantes,day_of_week_tz)
respuesta2 <- count(dia)
arrange(respuesta2,n)

#Pregunta 3# ¿Cuál es el tiempo promedio de conexión de un visitante?
visitantes <- filter(e_dataframe,visitor=="true")
respuesta3 <- mean(visitantes$tiempodeses)/60

#Pregunta 4# ¿Cuantas personas por mes han realizado visitas?
visitantes <- filter(e_dataframe,visitor=="true")
mes <- group_by(visitantes,month_tz)
respuesta4 <- count(mes)
arrange(respuesta4,n)

#Pregunta 5# ¿A qué hora se registran más visitantes?
visitantes <- filter(e_dataframe,visitor=="true")
hora <- group_by(visitantes,hour_tz)
respuesta5 <- count(hora)%>%
  arrange(n)

#GRÁFICAS-----------------------------------------------------------------------
#ESTABLECER EL DIRECTORIO DE SALIDA DE LAS GRÁFICAS
dir <- "//Users/usuario/Proyecto_datascience/graficas/" # Gráficas

# Graficar datos
# GRAFICA RESPUESTA 1
gr <- ggplot(conexiones, aes(x= branch_office, y= n)) +
  geom_col()

gr <- ggplot(conexiones, aes(x= branch_office, y= n)) +
  geom_col(fill = 'blue3') +
  labs(title="Gráfica 1. Número de conexiones por sucursal", x="Número de sucursal", y="Número de conexiones", size = 5) +
  geom_text(aes(label=n,vjust = -1))
  #Guardar las gráfica
  ggsave(paste(dir, "1.png", sep="/"), plot=gr, width=12, height=12)

  
# GRAFICA RESPUESTA 2
gr <- ggplot(respuesta2, aes(x= day_of_week_tz, y= n)) +
  geom_col()

gr <- ggplot(respuesta2, aes(x= day_of_week_tz, y= n)) +
  geom_col(fill = 'green4') +
  labs(title="Grafica 2. Visitantes por día de semana", x="Día de la semana", y="Número de visitantes", size = 5) +
  geom_text(aes(label=n,vjust = -1))
  #Guardar las gráficas
  ggsave(paste(dir, "2.png", sep="/"), plot=gr, width=12, height=12)
  
# GRAFICA RESPUESTA 4
gr <- ggplot(respuesta4, aes(x= month_tz, y= n)) +
  geom_col()
gr <- ggplot(respuesta4, aes(x= month_tz, y= n)) +
  geom_col(fill = 'red4') +
  labs(title="Grafica 3. Visitantes por mes", x="Mes", y="Número de visitantes", size =5) +
  geom_text(aes(label=n,vjust = -1))
 #Guardar las gráficas
  ggsave(paste(dir, "4.png", sep="/"), plot=gr, width=12, height=12)
  
# GRAFICA RESPUESTA 5
gr <- ggplot(respuesta5, aes(x= hour_tz, y= n)) +
  geom_col()
gr <- ggplot(respuesta5, aes(x= hour_tz, y= n)) +
  geom_col(fill = 'yellow3') +
  labs(title="Grafica 4. Visitantes por hora del día", x="Hora", y="Número de visitantes", size =5) +
  geom_text(aes(label=n,vjust = -1))
  #Guardar las gráficas
  ggsave(paste(dir, "5.png", sep="/"), plot=gr, width=12, height=12)
  
  #MODELO DE PREDICCIÓN--------------------------------------------------------
#install.packages(c("C50", "gmodels"))
library(tidyverse)
library(gmodels) # Herramientas para evaluar árbol de decisión
library('C50') # Genera el árbol de decisión

#Leer datos
#Base de datos de 249556 registros
dir1 <- "~/Proyecto_datascience"
Klustera <- read.csv(paste(dir1, "e.csv", sep="/"), stringsAsFactors = FALSE)

# ver al estructura
str(Klustera)

# Generamos una tabla con los visitantes y no visitantes
table(Klustera$visitor)

#quitando las dos primeras variables no númericas. 
Klustera <- Klustera[-1]
str(Klustera)
KlusteraX <- Klustera[-1]
str(KlusteraX)

#convertir día de la semana a numérico
levels <- c('Monday', 'Tuesday', 'Wednesday','Thursday','Friday','Saturday','Sunday')
KlusteraX$day_of_week_tz <- match(KlusteraX$day_of_week_tz, levels)

#convertir visitor a categórico
# Generamos una tabla con los diagnósticos
table(KlusteraX$visitor)

# recodificamos la columna visitor como factor
# los algoritmos requieren que el valor "objetivo" (columna de respuestas) sea un factor 
KlusteraX$visitor <- factor(KlusteraX$visitor, levels = c("true", "false"), labels = c("Visitante", "NoVisitante"))

# Transformamos la tabla a porcentajes
round(prop.table(table(KlusteraX$visitor)) * 100, digits = 1)

## Entrenamiento------------------------------------------------
# separamos la DB en un set como entrenamiento y otro como prueba
nfilas <- nrow(KlusteraX) * .80
set.seed(123)
index <- sample(1:nrow(KlusteraX), nfilas) # 80%


#restando la columna de visitor
KlusteraX_menos_visitor<- select(KlusteraX, - visitor)

Klustera_train <- KlusteraX_menos_visitor[index,] # Obtener solo las muestras sin visitor
Klustera_test <- KlusteraX_menos_visitor[-index,] # Todo menos las muestras sin visitor

# Guardamos la clasificación de cada uno (Visitante o NoVisitante) de la primera columna
Klustera_train_labels <- KlusteraX[index,6]
Klustera_test_labels <- KlusteraX[-index,6]


# Generando el modelo
Klustera_model <- C5.0(Klustera_train, Klustera_train_labels)
Klustera_model
summary(Klustera_model)# 

## ------------- ------------- ------------- ------------- -------------
# Evaluamos el modelo
# Creamos un vector con las predicciones sobre nuestos datos de pruebas
Klustera_pred <- predict(Klustera_model, Klustera_test)

CrossTable(Klustera_test_labels, Klustera_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

## ------------- ------------- ------------- ------------- -------------
# boosts
# Generará el número de arboles que indiquemos en trails
#Generamos 10 árboles adicionales y nos selecciona el mejor
Klustera_boost10_model <- C5.0(Klustera_train, Klustera_train_labels,trials = 10)
Klustera_boost10_model
summary(Klustera_boost10_model)

# Evaluamos el modelo 2
# Creamos un vector con las predicciones sobre nuestos datos de pruebas
Klustera_boost_pred10 <- predict(Klustera_boost10_model, Klustera_test)

CrossTable(Klustera_test_labels, Klustera_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicción'))

#APLICAR EL MODELO EN LA TABLA V

V_dataframe <- read.csv("~/Proyecto_datascience/v.csv")

#convertir día de la semana a numérico
levels <- c('Monday', 'Tuesday', 'Wednesday','Thursday','Friday','Saturday','Sunday')
V_dataframe$day_of_week_tz <- match(V_dataframe$day_of_week_tz, levels)

# Creamos un vector con las predicciones sobre nuestos datos de pruebas
Klustera_pred_V <- predict(Klustera_boost10_model, V_dataframe)

#Unimos la tabla de visitor resultante con el resto de los datos de v.
V_dataframe<-V_dataframe %>%
  mutate(visitor=Klustera_pred_V)

#Guarda el dataset combinado 
write.csv(V_dataframe,"//Users/usuario/Proyecto_datascience/V_visitor.csv") 

