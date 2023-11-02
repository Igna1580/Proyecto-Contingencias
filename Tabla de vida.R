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

colnames(Hombres_2023_demografia) <- c("lx")
colnames(Mujeres_2023_demografia) <- c("lx")


filtro <- c()
for (i in 0:19){
  filtro <- c(filtro, (4+i*8):(8+i*8))
}

Hombres_2023_demografia <- Hombres_2023_demografia[filtro,1]
Mujeres_2023_demografia <- Mujeres_2023_demografia[filtro,1]

Prob_Trans_Hombres <- read.csv("ProbTransHombres.csv", sep = ";")
Prob_Trans_Mujeres <- read.csv("ProbTransMujeres.csv", sep = ";")



# inflación en Costa Rica de los últimos 20 años según Base de datos del Fondo Monetario Internacional, Banco Mundial e indicador del IPC de la OCDE
inflacion_data <- data.frame(
  Año = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
  Costa_Rica = c(9.17, 9.45, 12.31, 13.80, 11.47, 9.36, 13.42, 7.84, 5.66, 4.88, 4.50, 5.23, 4.52, 0.80, -0.02, 1.63, 2.22, 2.10, 0.72, 1.73, 8.27)
)

inflacion <- mean(inflacion_data$Costa_Rica) 
# inflación = 6.15


# descargamos la curva de rendimiento soberana de los últimos 6 meses
tasas_Descuento <- read_excel("descuento.xlsx") 

descuento <- mean(tasas_Descuento$`3 meses`)
# descuento 5.8
