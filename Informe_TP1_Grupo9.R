rm(list = ls())
dev.off()


library(party)
library(stringr)
library(tidyverse)
library(PASWR)
library(readxl)
library(ggplot2)
library(rriskDistributions)
library(stats)
#Para setear el directorio de trabajo
setwd('C:/Users/Santiago/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/FACULBAD/Computacion Cientifica Actuarial/TP 1 2020 2c')
path = "D:/Desktop/FACULTAD/2020/2 do CUATRIMESTRE/COMPUTACION ACTUARIAL/TP 1"
setwd(path)
dir()

#Para importar dataset como aarchivo xlsx
Chocolates <- read_xlsx("flavors_of_cacao.xlsx") #Libreria readxl
str(Chocolates)
data.class(Chocolates)
View(Chocolates)
attach(Chocolates)


any(!complete.cases(Chocolates)) #Para saber si hay datos faltantes
Chocolates %>% map_lgl(.f = function(x){any(!is.na(x) & x == "")}) #Para saber si hay datos en blanco ("")
##PARA CAMBIAR LOS ESPACIOS EN BLANCO POR VALORES NA
Chocolates[] <- lapply(Chocolates[], trimws)
Chocolates[] <- lapply(Chocolates[], function(x) gsub("^\\s+|\\s+$", "", x))
Chocolates[Chocolates == ""] <- NA
map_dbl(Chocolates, .f = function(x){sum(is.na(x))}) #Para saber donde nos faltan datos


#Es importante completar los espacios vacios ("") con valores NA, debido a que
#R los interpreta como character y no como valores ausentes NA

#### 1)----
#Distribucion de las variables
#Cuando se crea un modelo, es muy importante estudiar la distribución 
# de la variable respuesta, ya que, a fin de cuentas, es lo que nos 
# interesa predecir.

#Grafica que nos muestra la distribucion de Rating, nuestra variable respuesta
#del paquete ggplot2

ggplot(data = Chocolates, aes(x = Rating, y = ..count.., fill = Rating)) +
  geom_bar() +
  scale_fill_manual(values = rep("gray50",13)) + #el nro 13 es por la cantidad de niveles 
  labs(title = "Rating") +
  theme_bw() +
  theme(legend.position = "bottom")
table(Chocolates$Rating) #Nos dice la cantidad de calificaciones iguales
prop.table(table(Chocolates$Rating)) %>% round(digits = 4) #Nos dice los porcentajes
                                                          #de cada calificacion

Rating <- as.numeric(Rating) #Para convertirlo en un vector numerico
#Analisis exploratorio de la variables rating y cocoa percent
EDA(Rating) #Libreria PASWR
fit.cont(Rating)    #Libreria rriskDistributions
#Rechaza todas las distribuciones - pero la toma como NORMAL
#Rating pareceria que se distribuye normalmente, por el grafico que arroja.

`Cocoa Percent` <- as.numeric(`Cocoa Percent`)
EDA(`Cocoa Percent`)
fit.cont(`Cocoa Percent`)   #Libreria rriskDistributions
#Rechaza todas las distribuciones - pero la toma como NORMAL
#Cocoa Percent pareceria que se distribuye 
#normalmente, por el grafico que arroja. Aunque las colas no son normales

#Como tratar los datos NA y por que
#Para no tomar en cuenta los valores NA de los distintos campos,
#a la hora de utilizar las variables se debe agregar el argumento
# na.rm=T, de ese modo vamos a obtener un resultado en vez de NA

#Momentos absolutos y centrados
Rating <- as.numeric(Rating) #Para convertirlo en un vector numerico
mean(Rating, na.rm = T) #Media
median(Rating,na.rm=T) #Mediana
min(Rating,na.rm=T) #Valor minimo
max(Rating,na.rm=T) #Valor maximo
quantile(Rating,na.rm=T) #Quantiles
var(Rating,na.rm=T) #Varianza
sd(Rating,na.rm=T)  #desvio estandar
IQR(Rating,na.rm=T) #rango intercuartil
mad(Rating,na.rm=T) #desvio absoluto medio

`Cocoa Percent` <- as.numeric(`Cocoa Percent`)
mean(`Cocoa Percent`, na.rm = T)
median(`Cocoa Percent`,na.rm=T) #Mediana
min(`Cocoa Percent`,na.rm=T) #Valor minimo
max(`Cocoa Percent`,na.rm=T) #Valor maximo
quantile(`Cocoa Percent`,na.rm=T) #Quantiles
var(`Cocoa Percent`,na.rm=T) #Varianza
sd(`Cocoa Percent`,na.rm=T)  #desvio estandar
IQR(`Cocoa Percent`,na.rm=T) #rango intercuartil
mad(`Cocoa Percent`,na.rm=T) #desvio absoluto medio

#Presencia de outliers
boxplot(Rating)  #NO SE VISUALIZAN MUCHOS OUTLIERS
boxplot(`Cocoa Percent`)  # SE VISUALIZAN MUCHOS OUTLIERS, ESO EXPLICA EL PESO EN LAS COLAS EN EL 
                          #GRAFICO Q-Q PLOT


#### 2)----
#Relaciones entre variables y calificacion de los chocolates

#PROCEDIMIENTO REALIZADO A PARTIR DE LA LINEA 206

#Indicar y comentar esas relaciones
#### 3)----
# a. ¿Dónde se producen los mejores granos de cacao?

(PaisesOrigen5Ptos <- Chocolates[which(Chocolates[,7]==5),"Broad Bean Origin"])
#De Venezuela provienen los granos de cacao que tienen rating 5

# b. ¿Qué paises producen las barras de cacao mejor con mejor calificación?

#PAISES PRODUCTORES DE BARRAS DE CHOCOLATE CON LA MEJOR CALIFICACION
(PaisesProductores5Ptos <- Chocolates[which(Chocolates[,7]==5),"Company Location"]) #PAISES PRODUCTORES DE LOS MEJORES CHOCOLATES
#1 Italy             
#2 Italy 

#PARA VER LAS NACIONALIDADES DE LAS COMPAÑIAS QUE PRODUCEN BARRAS CON CALIFICACION MAYOR A 4 Y SUS RESPECTIVOS ORIGENES DEL CACAO

P <- matrix(NA, nrow = length(Rating), ncol = 3) 
for (i in 1:length(Rating)){ 
  if(Rating[i] >= 4){
    P[i,1] <- Rating[i]
    P[i,2] <- `Company Location`[i]
    P[i,3] <- `Broad Bean Origin`[i]
  }
  else{
    P[i,1] <-  NA
    P[i,2] <- `Company Location`[i]
    P[i,3] <-  NA
  }
} 
P <- as.data.frame(P) #P originalmente es una matriz, con as.data.frame
# la convertimos en un data frame
P <- na.omit(P) #elimina las filas con NA valores, de ese modo "depura"
#el data frame y nos deja los paises y compañias y sus 
#determinadas calificaciones de 4 o mas

table(P$V3) #Para ver la cantidad de paises repetidos y determinar cuales
# son los productores del mejor cacao.

# c. ¿Qué relación hay entre el porcentaje de cacao en una barra y su calificación?
Chocolates$Rating <- as.numeric(Chocolates$Rating)
Chocolates$`Cocoa Percent` <- as.numeric(Chocolates$`Cocoa Percent`)
cor(Rating,`Cocoa Percent`)
plot(Rating,`Cocoa Percent`)


#CONSIGNA 2----

rm(list = ls())
dev.off()
install.packages("fastDummies") #Para obtener variables dummy punto 2.2
library(fastDummies)
library(readxl)
install.packages("caret") #paquete que se utiliza createDataPartition para 
#el punto 2.3 donde divimos 70/30 el dataset
library(caret)

#Para cargar los datos, ya asea archivo csv o xlsx
Chocolates.c <- read_xlsx("flavors_of_cacao.xlsx") #Libreria readxl  #DATASET ORIGINAL QUE SE VA A UTILIZAR PARA ANALISIS CONTINUO
Chocolates.d <- read_xlsx("flavors_of_cacao.xlsx") #Libreria readxl  #DATASET ORIGINAL QUE SE VA A UTILIZAR PARA ANALISIS DISCRETO
 ##PARA CAMBIAR LOS ESPACIOS EN BLANCO POR VALORES NA
Chocolates.c[] <- lapply(Chocolates.c[], trimws)
Chocolates.c[] <- lapply(Chocolates.c[], function(x) gsub("^\\s+|\\s+$", "", x))
Chocolates.c[Chocolates.c == ""] <- NA

Chocolates.d[] <- lapply(Chocolates.d[], trimws)
Chocolates.d[] <- lapply(Chocolates.d[], function(x) gsub("^\\s+|\\s+$", "", x))
Chocolates.d[Chocolates.d == ""] <- NA

#Para transformar la variable Rating y cocoa percent a numerica y poder operar sin complicaciones
Chocolates.c$Rating <- as.numeric(Chocolates.c$Rating) 
Chocolates.d$Rating <- as.numeric(Chocolates.d$Rating)
Chocolates.c$`Cocoa Percent` <- as.numeric(Chocolates.c$`Cocoa Percent`) 
Chocolates.d$`Cocoa Percent` <- as.numeric(Chocolates.d$`Cocoa Percent`)

attach(Chocolates.c)
attach(Chocolates.d)

#1)----
#Con este bucle for transformamos la variable Rating en una variable binaria,
# la cual indica 1 = 5 o 0 = <5
for (i in 1:length(Chocolates.d$Rating)){ 
  if(Chocolates.d$Rating[i] == 5){
    Chocolates.d[i,7] <- 1
  }
  else{
    Chocolates.d[i,7] <- 0
  }
} 

Lista_chocolates <- list(Chocolates.c,Chocolates.d)  #LISTA QUE CONTIENE LISTA CON EL DATASET ORIGINAL Y EL MODIFICADO


#2)----

#VARIABLE CONTINUA COMPAÑIAS

#Primero se crean columnas con los paises que mas producen chocolate, y los que menos producen se engloban en una categoria conjunta

rating.cia.usa <- matrix(NA, nrow = length(Rating), ncol = 1) #764 compañias
rating.cia.france <- matrix(NA, nrow = length(Rating), ncol = 1) #156 compañias
rating.cia.canada <- matrix(NA, nrow = length(Rating), ncol = 1) #125 compañias
rating.cia.uk <- matrix(NA, nrow = length(Rating), ncol = 1) #96 compañias
rating.cia.italy <- matrix(NA, nrow = length(Rating), ncol = 1) #63 compañias
rating.cia.ecuador <- matrix(NA, nrow = length(Rating), ncol = 1) #54 compañias
rating.cia.australia <- matrix(NA, nrow = length(Rating), ncol = 1) #49 compañias
rating.cia.belgium <- matrix(NA, nrow = length(Rating), ncol = 1) #40 compañias
rating.cia.Switzerland <- matrix(NA, nrow = length(Rating), ncol = 1) #38 compañias
rating.cia.germany <- matrix(NA, nrow = length(Rating), ncol = 1) #35 compañias
rating.cia.austria <- matrix(NA, nrow = length(Rating), ncol = 1) #26 compañias
rating.cia.spain <- matrix(NA, nrow = length(Rating), ncol = 1) #25 compañias
rating.cia.colombia <- matrix(NA, nrow = length(Rating), ncol = 1) #23 compañias
rating.cia.hungary <- matrix(NA, nrow = length(Rating), ncol = 1) #22 compañias
rating.cia.venezuela <- matrix(NA, nrow = length(Rating), ncol = 1) #20 compañias
rating.cia.otras <- matrix(NA, nrow = length(Rating), ncol = 1) # compañias

#Con este bucle le damos a la matriz de cada pais el valor de rating de los chocolates que produce
for(i in 1:nrow(Chocolates.c)){
  if(Chocolates.c$`Company Location`[i] == "U.S.A."){
    rating.cia.usa[i] <- Chocolates.c$Rating[i]
  } else if (Chocolates.c$`Company Location`[i] == "France"){
    rating.cia.france[i] <- Chocolates.c$Rating[i]
  } else if (Chocolates.c$`Company Location`[i] == "Canada"){
    rating.cia.canada[i] <- Chocolates.c$Rating[i]
  } else if (Chocolates.c$`Company Location`[i] == "U.K."){
    rating.cia.uk[i] <- Chocolates.c$Rating[i] 
  } else if (Chocolates.c$`Company Location`[i] == "Italy"){
    rating.cia.italy[i] <- Chocolates.c$Rating[i]
  } else if (Chocolates.c$`Company Location`[i] == "Ecuador"){
    rating.cia.ecuador[i] <- Chocolates.c$Rating[i]
  } else if (Chocolates.c$`Company Location`[i] == "Australia"){
    rating.cia.australia[i] <- Chocolates.c$Rating[i]
  } else if (Chocolates.c$`Company Location`[i] == "Belgium"){
    rating.cia.belgium[i] <- Chocolates.c$Rating[i]
  } else if (Chocolates.c$`Company Location`[i] == "Switzerland"){
    rating.cia.Switzerland[i] <- Chocolates.c$Rating[i]
  } else if (Chocolates.c$`Company Location`[i] == "Germany"){
    rating.cia.germany[i] <- Chocolates.c$Rating[i]
  } else if (Chocolates.c$`Company Location`[i] == "Austria"){
    rating.cia.austria[i] <- Chocolates.c$Rating[i]
  } else if (Chocolates.c$`Company Location`[i] == "Spain"){
    rating.cia.spain[i] <- Chocolates.c$Rating[i]
  } else if (Chocolates.c$`Company Location`[i] == "Colombia"){
    rating.cia.colombia[i] <- Chocolates.c$Rating[i]
  } else if (Chocolates.c$`Company Location`[i] == "Hungary"){
    rating.cia.hungary[i] <- Chocolates.c$Rating[i]
  } else if (Chocolates.c$`Company Location`[i] == "Venezuela"){
    rating.cia.venezuela[i] <- Chocolates.c$Rating[i]
  } else {
    rating.cia.otras[i] <- Chocolates.c$Rating[i]
  }
}


#Sacamos el promedio de rating de barras de chocolate producidas por cada pais
cia.location.mean <- matrix(NA, nrow = length(Rating), ncol = 1)
for(i in 1:nrow(Chocolates.c)){
  if(Chocolates.c$`Company Location`[i] == "U.S.A."){
    cia.location.mean[i] <- mean(rating.cia.usa, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "France"){
    cia.location.mean[i] <- mean(rating.cia.france, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "Canada"){
    cia.location.mean[i] <- mean(rating.cia.canada, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "U.K."){
    cia.location.mean[i] <- mean(rating.cia.uk, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "Italy"){
    cia.location.mean[i] <- mean(rating.cia.italy, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "Ecuador"){
    cia.location.mean[i] <- mean(rating.cia.ecuador, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "Australia"){
    cia.location.mean[i] <- mean(rating.cia.australia, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "Belgium"){
    cia.location.mean[i] <- mean(rating.cia.belgium, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "Switzerland"){
    cia.location.mean[i] <- mean(rating.cia.Switzerland, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "Germany"){
    cia.location.mean[i] <- mean(rating.cia.germany, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "Austria"){
    cia.location.mean[i] <- mean(rating.cia.austria, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "Spain"){
    cia.location.mean[i] <- mean(rating.cia.spain, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "Colombia"){
    cia.location.mean[i] <- mean(rating.cia.colombia, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "Hungary"){
    cia.location.mean[i] <- mean(rating.cia.hungary, na.rm=T)
  } else if (Chocolates.c$`Company Location`[i] == "Venezuela"){
    cia.location.mean[i] <- mean(rating.cia.venezuela, na.rm=T)
  } else {
    cia.location.mean[i] <- mean(rating.cia.otras, na.rm=T)
  }
}

View(cia.location.mean)


#Primero se crean columnas con los paises que mas producen granos de cacao, y los que menos producen se engloban en una categoria conjunta
# de no pertenecer se completa con valores NA
rating.bbo.venezuela <- matrix(NA, nrow = length(Rating), ncol = 1) #214 productores
rating.bbo.ecuador <- matrix(NA, nrow = length(Rating), ncol = 1) #193 productores
rating.bbo.peru <- matrix(NA, nrow = length(Rating), ncol = 1) #165 productores
rating.bbo.madagascar <- matrix(NA, nrow = length(Rating), ncol = 1) #145 productores
rating.bbo.dominican.r <- matrix(NA, nrow = length(Rating), ncol = 1) #141 productores
rating.bbo.nicaragua <- matrix(NA, nrow = length(Rating), ncol = 1) #60 productores
rating.bbo.brasil <- matrix(NA, nrow = length(Rating), ncol = 1) #58 productores
rating.bbo.bolivia <- matrix(NA, nrow = length(Rating), ncol = 1) #57 productores
rating.bbo.belize <- matrix(NA, nrow = length(Rating), ncol = 1) #49 productores
rating.bbo.papua <- matrix(NA, nrow = length(Rating), ncol = 1) #42 productores
rating.bbo.colombia <- matrix(NA, nrow = length(Rating), ncol = 1) #40 productores
rating.bbo.costa.rica <- matrix(NA, nrow = length(Rating), ncol = 1) #38 productores
rating.bbo.vietnam <- matrix(NA, nrow = length(Rating), ncol = 1) #38 productores
rating.bbo.tanzania <- matrix(NA, nrow = length(Rating), ncol = 1) #34 productores
rating.bbo.ghana <- matrix(NA, nrow = length(Rating), ncol = 1) #33 productores
rating.bbo.trinidad <- matrix(NA, nrow = length(Rating), ncol = 1) # 33 productores
rating.bbo.mexico <- matrix(NA, nrow = length(Rating), ncol = 1) #30 compañias
rating.bbo.otros <- matrix(NA, nrow = length(Rating), ncol = 1) #resto productores

#Con este bucle le damos a la matriz de cada pais el valor de rating de los granos que produce

for(i in 1:nrow(Chocolates.c)){
  if(isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Venezuela")){
    rating.bbo.venezuela[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Ecuador")){
    rating.bbo.ecuador[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Peru")){
    rating.bbo.peru[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Madagascar")){
    rating.bbo.madagascar[i] <- Chocolates.c$Rating[i] 
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Dominican Republic")){
    rating.bbo.dominican.r[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Nicaragua")){
    rating.bbo.nicaragua[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Brazil")){
    rating.bbo.brasil[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Bolivia")){
    rating.bbo.bolivia[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Belize")){
    rating.bbo.belize[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Papua New Guinea")){
    rating.bbo.papua[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Colombia")){
    rating.bbo.colombia[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Costa Rica")){
    rating.bbo.costa.rica[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Vietnam")){
    rating.bbo.vietnam[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Tanzania")){
    rating.bbo.tanzania[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Ghana")){
    rating.bbo.ghana[i] <- Chocolates.c$Rating[i]
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Trinidad")){
    rating.bbo.trinidad[i] <- Chocolates.c$Rating[i]
  }  else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Mexico")){
    rating.bbo.mexico [i] <- Chocolates.c$Rating[i]
  } else {
    rating.bbo.otros[i] <- Chocolates.c$Rating[i]
  }
}


#Creamos la variable que contiene los promedios de rating por cada pais productor
#se tomaron los paises que tienen 30 o mas producciones, basandonos en el
#teorema central de limite que nos dice que una buena estimacion es con 
#un n >= 30, los que no cumplen esta condicion se engloban con la categoria "otros"
broad.bean.origin.mean <- matrix(NA, nrow = length(Rating), ncol = 1)

for(i in 1:nrow(Chocolates.c)){
  if(isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Venezuela")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.venezuela, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Ecuador")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.ecuador, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Peru")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.peru, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Madagascar")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.madagascar, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Dominican Republic")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.dominican.r, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Nicaragua")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.nicaragua, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Brazil")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.brasil, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Bolivia")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.bolivia, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Belize")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.belize, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Papua New Guinea")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.papua, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Colombia")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.colombia, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Costa Rica")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.costa.rica, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Vietnam")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.vietnam, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Tanzania")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.tanzania, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Ghana")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.ghana, na.rm=T)
  } else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Trinidad")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.trinidad, na.rm=T)
  }  else if (isTRUE(Chocolates.c$`Broad Bean Origin`[i] == "Mexico")){
    broad.bean.origin.mean[i] <- mean(rating.bbo.mexico, na.rm=T)
  } else {
    broad.bean.origin.mean[i] <- mean(rating.bbo.otros , na.rm=T)
  }
}
View(broad.bean.origin.mean)

#agregamos al dataset las variables que contienen agrupados los paises productores
#y las compañias con sus respectivas medias de cada grupo
Chocolates.c <- cbind(Chocolates.c, broad.bean.origin.mean, cia.location.mean)

install.packages("dplyr")
library(dplyr)
#Se agrega la variable "categorias" con los distintos niveles de calificacion.

Chocolates.c <- Chocolates.c %>%
  mutate(Categorias = case_when(Rating < 2  ~ "implacentero",
                                Rating >= 2 & Rating < 3  ~ "decepcionante",
                                Rating >= 3 & Rating <= 3.5 ~ "satisfactorio",
                                Rating > 3.5 & Rating < 4 ~ "remarcable",
                                Rating >= 4 & Rating < 5 ~ "premium",
                                Rating == 5 ~ "elite"))


#Se agrega la variable "cateorias.n" 
Chocolates.c <- Chocolates.c %>%
  mutate(promedio.categorias = case_when(Categorias == "implacentero"  ~ mean(rating.implacentero, na.rm=T),
                                         Categorias == "decepcionante" ~ mean(rating.decepcionante, na.rm=T),
                                         Categorias == "satisfactorio" ~ mean(rating.satisfactorio, na.rm=T),
                                         Categorias == "remarcable" ~ mean(rating.remarcable, na.rm=T),
                                         Categorias == "premium" ~ mean(rating.premium, na.rm=T),
                                         Categorias == "elite" ~ mean(rating.elite, na.rm=T)))


#GENERAMOS VARIABLES "DUMMY" PARA LAS VARIABALES CUALITATIVAS EN EL ANALISIS DISCRETO

#dummy_cols, del paquete fastDummies nos genera 2 nuevas variables
#con valores 1 o 0 si cumple con la condicion de ser; 1 = rating 5
# 0 = rating < 5

Chocolates.d <- dummy_cols(Chocolates.d,  select_columns = c("Rating"))
Chocolates.d <- dummy_cols(Chocolates.d,  select_columns = c("Broad Bean Origin"))
Chocolates.d <- dummy_cols(Chocolates.d,  select_columns = c("Company Location"))

View(Chocolates.d)

#Distribucion de categorias de los chocolates
library(ggplot2)


ggplot(data = Chocolates.c, aes(x = Categorias, y = ..count.., fill = Categorias)) +
  geom_bar() +
  scale_fill_manual(values = rep("gray50",6)) + #el nro 6 es por la cantidad de niveles 
  labs(title = "Calificacion") +
  theme_bw() +
  theme(legend.position = "bottom")

#Armamos la lista que incluye el dataset con rating continuo (YA TRANSFORMADO) y discreto (YA TRANSFORMADO)
Lista_chocolates <- list(Chocolates.c,Chocolates.d)

#3) 
set.seed(1995) #set.seet se utiliza para guardar el resultado aleatorio
#de la division 70/30 que se produce a continuacion.
# dividimos el data set continuo 70/30
entrenamiento <- createDataPartition(y = Chocolates.c$Categorias, p = 0.7, list = FALSE, times = 1)
chocolates.c.entrenamiento <- Chocolates.c[entrenamiento, ]
chocolates.c.test  <- Chocolates.c[-entrenamiento, ]

#Es importante verificar que la distribución de la variable
#respuesta es similar en el conjunto de entrenamiento y en el de test.
prop.table(table(chocolates.c.entrenamiento$Categorias))
prop.table(table(chocolates.c.test$Categorias))


set.seed(1998)
# dividimos el data set discreto 70/30
entrenamiento <- createDataPartition(y = Chocolates.d$Rating, p = 0.7, list = FALSE, times = 1)
chocolates.d.entrenamiento <- Chocolates.d[entrenamiento, ]
chocolates.d.test  <- Chocolates.d[-entrenamiento, ]



#Se crea lista con las variables partidas 70% entrenamiento 30% test
Lista_chocolates7030 <- list(chocolates.c.test,chocolates.c.entrenamiento,
                             chocolates.d.test,chocolates.d.entrenamiento)

Lista_original_y_7030 <- list(Lista_chocolates,Lista_chocolates7030) #####

#4)

#MODELO LINEAL DE LA VARIABLE CONTINUA  ####

Modelo_lm=lm(chocolates.c.entrenamiento$Rating ~ chocolates.c.entrenamiento$`Cocoa Percent` + chocolates.c.entrenamiento$broad.bean.origin.mean + chocolates.c.entrenamiento$cia.location.mean)
summary(Modelo_lm)
coef(Modelo_lm)
plot(Modelo_lm)

View(chocolates.c.test)
confint(Modelo_lm)  #INTERVALOS DE CONFIANZA DE LOS COEFICIENTES

Rating_Continuo <- matrix(NA,ncol=3,nrow=(nrow(chocolates.c.test)))
for(i in 1:nrow(chocolates.c.test)){
  A <- coef(Modelo_lm)[1]+ coef(Modelo_lm)[2]*chocolates.c.test[i,5]+ coef(Modelo_lm)[3]*chocolates.c.test[i,10] + coef(Modelo_lm)[4]*chocolates.c.test[i,11]
  Rating_Continuo[i,1] <- A
}
for(i in 1:nrow(chocolates.c.test)){
  Rating_Continuo[i,2] <- chocolates.c.test[i,7]
}
for(i in 1:nrow(chocolates.c.test)){
  Rating_Continuo[i,3] <- abs(Rating_Continuo[i,2]-Rating_Continuo[i,1])
}

colnames(Rating_Continuo) <- c("Rating Estimado", "Rating Observado", "Diferencias absolutas")

Bondad_Ajuste_Continuo <- chisq.test(Rating_Continuo[,2],Rating_Continuo[,1])
Bondad_Ajuste_Continuo

View(Rating_Continuo)

#MODELO LOGISTICO CON LA VARIABLE DISCRETA ####

x1 <- chocolates.d.entrenamiento$`Broad Bean Origin_Venezuela`
x2 <- chocolates.d.entrenamiento$`Broad Bean Origin_Ecuador`
x3 <- chocolates.d.entrenamiento$`Broad Bean Origin_Peru`
x4 <- chocolates.d.entrenamiento$`Broad Bean Origin_Madagascar`
x5 <- chocolates.d.entrenamiento$`Broad Bean Origin_Dominican Republic`
x6 <- chocolates.d.entrenamiento$`Broad Bean Origin_Nicaragua`
x7 <- chocolates.d.entrenamiento$`Broad Bean Origin_Brazil`
x8 <- chocolates.d.entrenamiento$`Broad Bean Origin_Bolivia`
x9 <- chocolates.d.entrenamiento$`Broad Bean Origin_Belize`
x10 <- chocolates.d.entrenamiento$`Broad Bean Origin_Papua New Guinea`
x11 <- chocolates.d.entrenamiento$`Broad Bean Origin_Colombia`
x12 <- chocolates.d.entrenamiento$`Broad Bean Origin_Costa Rica`
x13 <- chocolates.d.entrenamiento$`Broad Bean Origin_Vietnam`
x14 <- chocolates.d.entrenamiento$`Broad Bean Origin_Tanzania`
x15 <- chocolates.d.entrenamiento$`Broad Bean Origin_Ghana`
x16 <- chocolates.d.entrenamiento$`Broad Bean Origin_Trinidad`
x17 <- chocolates.d.entrenamiento$`Broad Bean Origin_Mexico`
x18 <- chocolates.d.entrenamiento$`Company Location_U.S.A.`
x19 <- chocolates.d.entrenamiento$`Company Location_France`
x20 <- chocolates.d.entrenamiento$`Company Location_Canada`
x21 <- chocolates.d.entrenamiento$`Company Location_U.K.`
x22 <- chocolates.d.entrenamiento$`Company Location_Italy`
x23 <- chocolates.d.entrenamiento$`Company Location_Ecuador`
x24 <- chocolates.d.entrenamiento$`Company Location_Australia`
x25 <- chocolates.d.entrenamiento$`Company Location_Belgium`
x26 <- chocolates.d.entrenamiento$`Company Location_Switzerland`
x27 <- chocolates.d.entrenamiento$`Company Location_Germany`
x28 <- chocolates.d.entrenamiento$`Company Location_Austria`
x29 <- chocolates.d.entrenamiento$`Company Location_Spain`
x30 <- chocolates.d.entrenamiento$`Company Location_Colombia`
x31 <- chocolates.d.entrenamiento$`Company Location_Hungary`
x32 <- chocolates.d.entrenamiento$`Company Location_Venezuela`


modelo_logistico <- glm(chocolates.d.entrenamiento$Rating ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 + x31 + x32, data = chocolates.d.entrenamiento, family = "binomial")
summary(modelo_logistico)
coef(modelo_logistico)
plot(modelo_logistico)

Rating_Discreto <- matrix(NA,ncol=3,nrow=(nrow(chocolates.d.test)))
for(i in 1:nrow(chocolates.d.test)){
  A <- as.numeric(coef(modelo_logistico)[1]+ coef(modelo_logistico)[2]*chocolates.d.test[i,"Broad Bean Origin_Venezuela"]+ coef(modelo_logistico)[3]*chocolates.d.test[i,"Broad Bean Origin_Ecuador"] + coef(modelo_logistico)[4]*chocolates.d.test[i,"Broad Bean Origin_Peru"]+coef(modelo_logistico)[5]*chocolates.d.test[i,"Broad Bean Origin_Madagascar"]+ coef(modelo_logistico)[6]*chocolates.d.test[i,"Broad Bean Origin_Dominican Republic"]+
    coef(modelo_logistico)[7]*chocolates.d.test[i,"Broad Bean Origin_Nicaragua"]+coef(modelo_logistico)[8]*chocolates.d.test[i,"Broad Bean Origin_Brazil"] + coef(modelo_logistico)[9]*chocolates.d.test[i,"Broad Bean Origin_Bolivia"]+ coef(modelo_logistico)[10]*chocolates.d.test[i,"Broad Bean Origin_Belize"] + coef(modelo_logistico)[11]*chocolates.d.test[i,"Broad Bean Origin_Papua New Guinea"]+ coef(modelo_logistico)[12]*chocolates.d.test[i,"Broad Bean Origin_Colombia"]+
    coef(modelo_logistico)[13]*chocolates.d.test[i,"Broad Bean Origin_Costa Rica"] + coef(modelo_logistico)[14]*chocolates.d.test[i,"Broad Bean Origin_Vietnam"] + coef(modelo_logistico)[15]*chocolates.d.test[i,"Broad Bean Origin_Tanzania"]+coef(modelo_logistico)[16]*chocolates.d.test[i,"Broad Bean Origin_Ghana"]+ coef(modelo_logistico)[17]*chocolates.d.test[i,"Broad Bean Origin_Trinidad"]+ coef(modelo_logistico)[18]*chocolates.d.test[i,"Broad Bean Origin_Mexico"]+
    coef(modelo_logistico)[19]*chocolates.d.test[i,"Company Location_U.S.A."]+coef(modelo_logistico)[20]*chocolates.d.test[i,"Company Location_France"]+ coef(modelo_logistico)[21]*chocolates.d.test[i,"Company Location_Canada"]+coef(modelo_logistico)[22]*chocolates.d.test[i,"Company Location_U.K."]+coef(modelo_logistico)[23]*chocolates.d.test[i,"Company Location_Italy"]+coef(modelo_logistico)[24]*chocolates.d.test[i,"Company Location_Ecuador"]+ coef(modelo_logistico)[25]*chocolates.d.test[i,"Company Location_Australia"]+
    coef(modelo_logistico)[26]*chocolates.d.test[i,"Company Location_Belgium"]+coef(modelo_logistico)[27]*chocolates.d.test[i,"Company Location_Switzerland"]+coef(modelo_logistico)[28]*chocolates.d.test[i,"Company Location_Germany"]+coef(modelo_logistico)[29]*chocolates.d.test[i,"Company Location_Austria"]+coef(modelo_logistico)[30]*chocolates.d.test[i,"Company Location_Spain"]+coef(modelo_logistico)[31]*chocolates.d.test[i,"Company Location_Colombia"]+coef(modelo_logistico)[32]*chocolates.d.test[i,"Company Location_Hungary"]+
    coef(modelo_logistico)[33]*chocolates.d.test[i,"Company Location_Venezuela"])
  Rating_Discreto[i,1] <- A
}
for(i in 1:nrow(chocolates.d.test)){
  Rating_Discreto[i,2] <- as.numeric(chocolates.d.test[1,7])
}
for(i in 1:nrow(chocolates.d.test)){
  Rating_Discreto[i,3] <- as.numeric(abs(Rating_Discreto[i,2]-Rating_Discreto[i,1]))
}

colnames(Rating_Discreto) <- c("Rating Estimado", "Rating Observado", "Diferencias absolutas")


View(Rating_Discreto)

# PORCENTAJE DE CACAO - ANALISIS SIN MODELO

cacao <- NA
for(i in 1:nrow(Chocolates.c)){
  if(Chocolates.c[i,7]>4){
    cacao[i] <- Chocolates.c[i,5]
  }
  else{
   cacao[i] <- NA 
  }
}

mean(cacao,na.rm = T)
