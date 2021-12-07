#Se instalan los paquetes a utilizar en caso de que no se encuentren instalados
if (!require(extraDistr) ) {
  install.packages("extraDistr", dependencies = TRUE )
  require(extraDistr)
}

if (!require(dplyr) ) {
  install.packages("dplyr", dependencies = TRUE )
  require(dplyr)
}

if (!require(tidyr) ) {
  install.packages("tidyr", dependencies = TRUE )
  require(tidyr)
}

if (!require(ggpubr) ) {
  install.packages("ggpubr", dependencies = TRUE )
  require(ggpubr)
}

#Se invocan las librerías a utilizar
library(extraDistr)
library (dplyr)
library (tidyr)
library (ggpubr)
#-----------------------------------------------------------------------------
#                                   Parte 1
#-----------------------------------------------------------------------------

#Dado el siguiente código
basename <- "Casen 2017.csv"
población <- read.csv(file.choose(), encoding = "UTF-8")
tamaño <- nrow(población)
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )
set.seed(133)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

#Definan su propia semilla y obtengan 5.000 casos para una distribución de 
#ingresos aproximadamente normal.
set.seed(80)
ingreso_normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

#A partir de la distribución conseguida, y sin usar nuevamente la función rnorm(),
#generen la correspondiente distribución Z.
ingreso_z <- (ingreso_normal - media.ingreso)/sd.ingreso

#Con la distribución Z obtenida en el punto anterior, y sin utilizar funciones 
#como rchisq(), construyan dos distribuciones x^2, cada una con más de 3 y menos 
#de 15 grados de libertad.

#Distribucion chi-cuadrado 1
v1 <- 4 #grados de libertad 1
ingreso_x2.1 <- numeric()
for (i in 1:5000){
  valor <- sum((ingreso_z[i:(i+(v1-1))])^2)
  ingreso_x2.1 <- c(ingreso_x2.1,valor)
}

#Distribucion chi-cuadrado 2
v2 <- 9 #grados de libertad 2
ingreso_x2.2 <- numeric()
for (i in 1:5000){
  valor <- sum((ingreso_z[i:(i+(v2-1))])^2)
  ingreso_x2.2 <- c(ingreso_x2.2,valor)
}

#Usando las dos distribuciones x^2 generadas en el punto anterior, 
#construyan una distribución F.
ingreso_f <- (ingreso_x2.1/v1)/(ingreso_x2.2/v2)

#En cada caso, construyan un gráfico para mostrar las distribuciones obtenidas 
#a sus compañeras y compañeros
distribuciones <- data.frame(ingreso_normal,ingreso_z,ingreso_x2.1,ingreso_x2.2,ingreso_f)
distribuciones <- distribuciones %>% pivot_longer (c("ingreso_normal", "ingreso_z",
                                                     "ingreso_x2.1", "ingreso_x2.2","ingreso_f") ,
                                                   names_to = "Distribuciones",
                                                   values_to = "ingresos")

#Grafico de cajas con distribucion normal
grafico1 <- boxplot(distribuciones$ingresos ~ distribuciones$Distribuciones,
                    main = "Distribuciones finales con la normal")
print(grafico1)

#Se elimina la distirbucion normal dentro de las distribuciones y se grafica nuevamente
#las distirbuciones pero sin la distribuciones normal
distribuciones <- distribuciones[distribuciones$Distribuciones != "ingreso_normal", ]
grafico2 <- boxplot(distribuciones$ingresos ~ distribuciones$Distribuciones,
                    main = "Distribuciones finales sin la normal")
print(grafico2)

#-----------------------------------------------------------------------------
#                                   Parte 2
#-----------------------------------------------------------------------------
#Dado el siguiente código
set.seed(133)
n.repeticiones <- 20
ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)
veinte.repeticiones <- sapply(1:n.repeticiones, ensayo)

#Definan su propia semilla y número de repeticiones para el ensayo.
set.seed(46)
n_repeticiones <- 30

ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)
repeticiones <- sapply(1:n_repeticiones, ensayo)

#Generen, sin utilizar funciones como rbinom(), una distribución binomial.


#Similarmente, construyan una distribución geométrica.


#Análogamente, generen una distribución binomial negativa.


#En cada caso, construyan un gráfico para mostrar las distribuciones obtenidas a sus compañeras y 
#compañeros.

