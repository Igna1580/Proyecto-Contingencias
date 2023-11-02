library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)


#--- Abrir base de datos -------------------------------------------------------
# Supuesto de distribucion uniforme de muertes
# Supuesto de edad maxima es 110 años
# inflación, promedio de los últimos 20 años

Hombres_2023_demografia <- read_excel("repoblacev2011-2050-05_2.xlsx", sheet = "Cuadro 5 ", range = "G1017:G1177")
Mujeres_2023_demografia <- read_excel("repoblacev2011-2050-05_2.xlsx", sheet = "Cuadro 5 ", range = "G1857:G2017")

colnames(Hombres_2023_demografia) <- c("poblacion_Hombres")
colnames(Mujeres_2023_demografia) <- c("poblacion_Mujeres")


filtro <- c()
for (i in 0:19){
  filtro <- c(filtro, (4+i*8):(8+i*8))
}

Hombres_2023_demografia <- Hombres_2023_demografia[filtro,1]
Mujeres_2023_demografia <- Mujeres_2023_demografia[filtro,1]

Prob_Trans_Hombres <- read.csv("ProbTransHombres.csv", sep = ";")
Prob_Trans_Mujeres <- read.csv("ProbTransMujeres.csv", sep = ";")


#--- Poblacion -----------------------------------------------------------------

edades <- 30:64

porcentajes <- c(0.05, 0.05, 0.8, 0.08, 0.10, 0.10, 0.15, 0.15, 0.20, 0.20, 0.25, 0.25, 0.30, 0.30, 0.35,
                 rep(0.6, 20))  # Luego, 20 porcentajes uniformes de 0.6
# Caso hombres 

# Crear un dataframe para las edades
edades_df <- data.frame(Edad = edades)

edades_seleccionadas_hombres <- Hombres_2023_demografia[edades,] 

# Unir el dataframe de edades con el dataframe de hombres
edades_seleccionadas_hombres <- cbind(edades_df, edades_seleccionadas_hombres)


# Luego puedes agregar este vector de porcentajes al dataframe edades_seleccionadas_hombres
edades_seleccionadas_hombres$Porcentajes_Diferentes <- porcentajes

# Calcular la población por edad (multiplicar la cantidad de hombres por el porcentaje)
edades_seleccionadas_hombres$Poblacion_por_Edad <- edades_seleccionadas_hombres$edades_seleccionadas_hombres * edades_seleccionadas_hombres$Porcentajes_Diferentes

# Caso Mujeres 
edades_seleccionadas_mujeres <- Mujeres_2023_demografia[edades,] 
edades_seleccionadas_mujeres <- cbind(edades_df, edades_seleccionadas_mujeres)
edades_seleccionadas_mujeres$Porcentajes_Diferentes <- porcentajes
edades_seleccionadas_mujeres$Poblacion_por_Edad <- edades_seleccionadas_mujeres$edades_seleccionadas_mujeres * edades_seleccionadas_mujeres$Porcentajes_Diferentes


#--- Obtencion de inflacio e interes -------------------------------------------

# inflación en Costa Rica de los últimos 20 años según Base de datos del Fondo Monetario Internacional, Banco Mundial e indicador del IPC de la OCDE
inflacion_data <- data.frame(
  Año = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
  Costa_Rica = c(4.50, 5.23, 4.52, 0.80, -0.02, 1.63, 2.22, 2.10, 0.72, 1.73, 8.27)
)

inflacion <- mean(inflacion_data$Costa_Rica) 
# inflación = 2.8818


# descargamos la curva de rendimiento soberana de los últimos 6 meses
tasas_Descuento <- read_excel("descuento.xlsx") 

descuento <- mean(tasas_Descuento$`3 meses`)
# descuento 5.8



#--- Obtencion de Probabilidades -----------------------------------------------

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

##--- Funcion de obtencion de probabilidades------------------------------------
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

#--- Obtencion de prima por edad -----------------------------------------------



