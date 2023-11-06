library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library("papaja")

#--- Abrir Base de Datos -------------------------------------------------------
# Supuesto de distribucion uniforme de muertes
# Supuesto de edad maxima es 110 años
# inflación, promedio de los últimos 10 años

Hombres_2023_demografia <- read_excel("repoblacev2011-2050-05_2.xlsx", sheet = "Cuadro 5 ", range = "G1017:G1177")
Mujeres_2023_demografia <- read_excel("repoblacev2011-2050-05_2.xlsx", sheet = "Cuadro 5 ", range = "G1857:G2017")

colnames(Hombres_2023_demografia) <- c("pob_H")
colnames(Mujeres_2023_demografia) <- c("pob_M")


filtro <- c()
for (i in 0:19){
  filtro <- c(filtro, (4+i*8):(8+i*8))
}

Hombres_2023_demografia <- Hombres_2023_demografia[filtro,1]
Mujeres_2023_demografia <- Mujeres_2023_demografia[filtro,1]

Prob_Trans_Hombres <- read.csv("ProbTransHombres.csv", sep = ";")
Prob_Trans_Mujeres <- read.csv("ProbTransMujeres.csv", sep = ";")


#--- Poblacion -----------------------------------------------------------------

edades <- 31:65

porcentajes <- c(0.05, 0.05, 0.08,0.08, 0.10, 0.10, 0.15, 0.15, 0.20, 0.20, 0.25, 0.25, 0.30, 0.30, 0.35,
                 rep(0.6, 20))  # Luego, 20 porcentajes uniformes de 0.6
# Caso hombres 

# Crear un dataframe para las edades
edades_df <- data.frame(Edad = edades-1)

edades_selec_H <- Hombres_2023_demografia[edades,] 

# Unir el dataframe de edades con el dataframe de hombres
edades_selec_H <- cbind(edades_df, edades_selec_H)

# Luego puedes agregar este vector de porcentajes al dataframe edades_selec_H
edades_selec_H$porc_estimados <- porcentajes

# Calcular la población por edad (multiplicar la cantidad de hombres por el porcentaje)
edades_selec_H$pob_estimada<- edades_selec_H$pob_H* edades_selec_H$porc_estimados

# Caso Mujeres 
edades_selec_M <- Mujeres_2023_demografia[edades,] 
edades_selec_M <- cbind(edades_df, edades_selec_M)
edades_selec_M$porc_estimados <- porcentajes
edades_selec_M$pob_estimada<- edades_selec_M$pob_M * edades_selec_M$porc_estimados


#--- Inflacio e Interes --------------------------------------------------------

# inflación en Costa Rica de los últimos 10 años según Base de datos del Fondo Monetario Internacional, Banco Mundial e indicador del IPC de la OCDE
inflacion_data <- data.frame(
  Ano = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
  Costa_Rica = c(4.50, 5.23, 4.52, 0.80, -0.02, 1.63, 2.22, 2.10, 0.72, 1.73, 8.27)
)

inflacion <- mean(inflacion_data$Costa_Rica) 
# inflación = 2.8818


# descargamos la curva de rendimiento soberana de los últimos 6 meses
tasas_Descuento <- read_excel("descuento.xlsx") 

descuento <- mean(tasas_Descuento$`3 meses`)
# descuento 5.8



#--- Probabilidades ------------------------------------------------------------

##--- Creacion de Dataframe con datos actuariales ------------------------------
#Esta funcion crea un dataframe con la pyoyeccion de personas en 
#ciertos estados a ciertas edades partiendo de una edad base y un sexo

obtencion_tabla_proyeccion <- function(x,status,sexo) {
  if (sexo == "H"){
    probabilidades <- Prob_Trans_Hombres
  }
  if (sexo == "M"){
    probabilidades <- Prob_Trans_Mujeres
  }
  
  tabla <- data.frame(
    "edad" = x:111,
    "l_age.x_sta.0" = rep(0, 112-x),
    "l_age.x_sta.1" = rep(0, 112-x),
    "l_age.x_sta.2" = rep(0, 112-x),
    "l_age.x_sta.3" = rep(0, 112-x),
    "l_age.x_sta.4" = rep(0, 112-x),
    "l_age.x_sta.5" = rep(0, 112-x)
  )
  tabla[1,status+2] = 1
  
  c(10000, rep(0, 111-x))
  
  
  for (fila in 2:nrow(tabla)){
    for (col in 2:7){
      tabla[fila,col] <- 
        tabla[fila-1,2]*probabilidades[(fila+x-21),col+1] + 
        tabla[fila-1,3]*probabilidades[(fila+x-21)+91,col+1] + 
        tabla[fila-1,4]*probabilidades[(fila+x-21)+182,col+1] + 
        tabla[fila-1,5]*probabilidades[(fila+x-21)+273,col+1] + 
        tabla[fila-1,6]*probabilidades[(fila+x-21)+364,col+1] + 
        tabla[fila-1,7]*probabilidades[(fila+x-21)+455,col+1]
    }
  }
  return (tabla)
}

##--- Funcion de Obtencion de Probabilidades------------------------------------

tPx_ij <- function(t=1,x=65,i=0,j=0,sexo){
  p <- obtencion_tabla_proyeccion(x,i,sexo)[t+1,j+2]
  return(p)
}

ax.n_ij <- function(x,n,i=0,j,r=5.8,inf=2.8818,sexo){
  prob <- obtencion_tabla_proyeccion(x,i,sexo)
  resultado <- 0
  for (e in 0:(n-1)){
    resultado <- ((1+inf/100)/(1+r/100))^(e)*prob[e+1,j+2] + resultado
  }
  return(resultado)
}

#--- Calculo Prima Generalizada -----------------------------------------------

A <- 183671.1703 #Beneficio estado 1
B <- 363381.6771 #Beneficio estado 2
C <- 740398.8263 #Beneficio estado 3
D <- 1572738.998 #Beneficio estado 4

##--- Beneficios ---------------------------------------------------------------
# Crear un vector vacío para almacenar los resultados de hombres
benef_H_indiv <- data.frame(benef_H_indiv = numeric())
for (x in 30:64) {
  beneficios_por_edad_hombres <- (A*ax.n_ij(x,n = 110-x,i=0,j=1,sexo="H")) +
                           (B*ax.n_ij(x,n = 110-x,i=0,j=2,sexo="H")) +
                             (C*ax.n_ij(x,n = 110-x,i=0,j=3,sexo="H")) +
                               (D*ax.n_ij(x,n= 110-x,i=0,j=4,sexo="H"))
  benef_H_indiv <- rbind(benef_H_indiv, data.frame(benef_H_indiv = beneficios_por_edad_hombres))
}

edades_selec_H <- cbind(edades_selec_H, benef_H_indiv = benef_H_indiv)
colnames(edades_selec_H)[5] <- "Benef_ind_por_edad"
benef_tot_por_edad <- edades_selec_H[, 5] * edades_selec_H[, 4]
benef_tot_por_edad <- as.vector(benef_tot_por_edad)
edades_selec_H <- cbind(edades_selec_H, benef_tot_por_edad)

suma_benef_H_indiv <- sum(edades_selec_H$benef_tot_por_edad)


# Crear un vector vacío para almacenar los resultados de mujeres
benef_M_indiv <- data.frame(benef_M_indiv = numeric())
for (x in 30:64) {
  beneficios_por_edad_mujeres <- (A*ax.n_ij(x,n = 110-x,i=0,j=1,sexo="M")) +
    (B*ax.n_ij(x,n = 110-x,i=0,j=2,sexo="M")) +
    (C*ax.n_ij(x,n = 110-x,i=0,j=3,sexo="M")) +
    (D*ax.n_ij(x,n= 110-x,i=0,j=4,sexo="M"))
  benef_M_indiv <- rbind(benef_M_indiv, data.frame(benef_M_indiv = beneficios_por_edad_mujeres))
}

edades_selec_M <- cbind(edades_selec_M, benef_M_indiv = benef_M_indiv)
colnames(edades_selec_M)[5] <- "Benef_ind_por_edad"
benef_tot_por_edad_M <- edades_selec_M[, 5] * edades_selec_M[, 4]
benef_tot_por_edad_M <- as.vector(benef_tot_por_edad_M)
edades_selec_M <- cbind(edades_selec_M, benef_tot_por_edad_M)

suma_benef_M_indiv <- sum(edades_selec_M$benef_tot_por_edad_M)

##--- Primas -------------------------------------------------------------------

# hombres 

primas_hombres <- data.frame(Primas_hombres = numeric())
for (x in 30:64) {
  primas_por_edad_hombres <- (ax.n_ij(x,n = 110-x,i=0,j=0,sexo="H")) +
    (ax.n_ij(x,n = 110-x,i=0,j=1,sexo="H")) +
    (ax.n_ij(x,n = 110-x,i=0,j=2,sexo="H")) 
  primas_hombres <- rbind(primas_hombres, data.frame(Primas_hombres = primas_por_edad_hombres))
}

edades_selec_H <- cbind(edades_selec_H, Primas_Hombres = primas_hombres)
colnames(edades_selec_H)[7] <- "primas_ind_por_edad "
primas_tot_por_edad <- edades_selec_H[, 7] * edades_selec_H[, 4]
primas_tot_por_edad <- as.vector(primas_tot_por_edad)
edades_selec_H <- cbind(edades_selec_H, primas_tot_por_edad)

primas_hombres_suma <- sum(edades_selec_H$primas_tot_por_edad)

# mujeres
primas_mujeres <- data.frame(Primas_mujeres = numeric())
for (x in 30:64) {
  primas_por_edad_mujeres <- (ax.n_ij(x,n = 110-x,i=0,0,5.8,inf=2.8818,"M")) +
    (ax.n_ij(x,n = 110-x,i=0,1,5.8,inf=2.8818,"M")) +
    (ax.n_ij(x,n = 110-x,i=0,2,5.8,inf=2.8818,"M")) 
  primas_mujeres <- rbind(primas_mujeres, data.frame(Primas_mujeres = primas_por_edad_mujeres))
}

edades_selec_M <- cbind(edades_selec_M, Primas_Mujeres = primas_mujeres)
colnames(edades_selec_M)[7] <- "primas_ind_por_edad "
primas_tot_por_edad_M <- edades_selec_M[, 7] * edades_selec_M[, 4]
primas_tot_por_edad_M <- as.vector(primas_tot_por_edad_M)
edades_selec_M <- cbind(edades_selec_M, primas_tot_por_edad_M)

primas_mujeres_suma <- sum(edades_selec_M$primas_tot_por_edad_M)


# principio de equivalencia

beneficios_totales <- suma_benef_M_indiv + suma_benef_H_indiv

primas_0.95 <- (primas_hombres_suma + primas_mujeres_suma)*0.95

costo_inicial <- 0.15

total_primas <- costo_inicial + primas_0.95

prima <- beneficios_totales / total_primas

# si consideramos primas separadas para población mujeres y población masculina sería:

primas_0.95_hombres <- primas_hombres_suma*0.95

total_primas_hombres <- costo_inicial + primas_0.95_hombres

prima_hombres <- suma_benef_H_indiv / total_primas_hombres


primas_0.95_mujeres <- primas_mujeres_suma*0.95

total_primas_mujeres <- costo_inicial + primas_0.95_mujeres

prima_mujeres <- suma_benef_M_indiv / total_primas_mujeres

#--------Graficos de poblacion--------------

p = ggplot() + 
  geom_line(data = edades_selec_H, aes(x = Edad, y = pob_estimada, color = "Hombres"), linetype = "solid", size = 1) +
  geom_line(data = edades_selec_M, aes(x = Edad, y = pob_estimada, color = "Mujeres"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Hombres" = "lightblue4", "Mujeres" = "maroon"), name = "Población") +
  xlab('Edad') +
  ylab('Población estimada') + cowplot::theme_cowplot()


print(p)


