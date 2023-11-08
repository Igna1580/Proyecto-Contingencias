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

#abrir datos demograficos
Hombres_2023_demografia <- read_excel("repoblacev2011-2050-05_2.xlsx", sheet = "Cuadro 5 ", range = "G1017:G1177")
Mujeres_2023_demografia <- read_excel("repoblacev2011-2050-05_2.xlsx", sheet = "Cuadro 5 ", range = "G1857:G2017")

colnames(Hombres_2023_demografia) <- c("pob_H")
colnames(Mujeres_2023_demografia) <- c("pob_M")

#filtrar los datos demograficos
filtro <- c()
for (i in 0:19){
  filtro <- c(filtro, (4+i*8):(8+i*8))
}

Hombres_2023_demografia <- Hombres_2023_demografia[filtro,1]
Mujeres_2023_demografia <- Mujeres_2023_demografia[filtro,1]

#abrir datos de probabilidades de transicion
Prob_Trans_Hombres <- read.csv("ProbTransHombres.csv", sep = ";")
Prob_Trans_Mujeres <- read.csv("ProbTransMujeres.csv", sep = ";")

#ajustarlas para que sumen 1
for(fila in 1:nrow(Prob_Trans_Hombres)){
  for(col in 3:8){
    Prob_Trans_Hombres[fila,col] <- Prob_Trans_Hombres[fila,col]/sum(Prob_Trans_Hombres[fila,3:8])
    Prob_Trans_Mujeres[fila,col] <- Prob_Trans_Mujeres[fila,col]/sum(Prob_Trans_Mujeres[fila,3:8])
  }
}

#--- Poblacion -----------------------------------------------------------------
edades <- 31:65

porcentajes <- c(0.05, 0.05, 0.08, 0.08, 0.10, 0.10, 0.15, 0.15, 0.20, 0.20, 0.25, 0.25, 0.30, 0.30, 0.35,
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


#--- Inflación e Interés --------------------------------------------------------

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

##--- Creación de Dataframe con datos actuariales ------------------------------
# Esta función crea un dataframe con la proyección de personas (1 persona) en 
# ciertos estados a ciertas edades partiendo de una edad base, estado y un sexo

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

##--- Funciones actuariales ----------------------------------------------------

#Función de probabilidad
tPx_ij <- function(t=1,x=65,i=0,j=0,sexo){
  p <- obtencion_tabla_proyeccion(x,i,sexo)[t+1,j+2]
  return(p)
}

#Función de anualidad prepagable
ax.n_ij <- function(x,n,i=0,j,r=5.8,inf=2.8818,sexo){
  prob <- obtencion_tabla_proyeccion(x,i,sexo)
  resultado <- 0
  for (e in 0:(n-1)){
    resultado <- ((1+inf/100)/(1+r/100))^(e)*prob[e+1,j+2] + resultado
  }
  return(resultado)
}

#Función anualidad diferida
ax.u_ij <- function(x, u=65-x, i=0, j, r=5.8, inf=2.8818, sexo){
  diferida <- ax.n_ij(x, 110-x, i, j, r, inf, sexo) - ax.n_ij(x, u, i, j, r, inf, sexo)
  return(diferida)
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
  beneficios_por_edad_hombres <- (A*ax.u_ij(x,65-x,i=0,j=1,sexo="H")) +
                           (B*ax.u_ij(x, 65-x,i=0,j=2,sexo="H")) +
                             (C*ax.u_ij(x, 65-x,i=0,j=3,sexo="H")) +
                               (D*ax.u_ij(x, 65-x,i=0,j=4,sexo="H"))
  benef_H_indiv <- rbind(benef_H_indiv, data.frame(benef_H_indiv = beneficios_por_edad_hombres))
}

edades_selec_H <- cbind(edades_selec_H, benef_H_indiv = benef_H_indiv)
colnames(edades_selec_H)[5] <- "Benef_ind_por_edad"
benef_tot_por_edad <- edades_selec_H[, 5] * edades_selec_H[, 4]
benef_tot_por_edad <- as.vector(benef_tot_por_edad)
edades_selec_H <- cbind(edades_selec_H, benef_tot_por_edad)

suma_benef_H_total <- sum(edades_selec_H$benef_tot_por_edad)


# Crear un vector vacío para almacenar los resultados de mujeres
benef_M_indiv <- data.frame(benef_M_indiv = numeric())
for (x in 30:64) {
  beneficios_por_edad_mujeres <- (A*ax.u_ij(x, 65-x,i=0,j=1,sexo="M")) +
    (B*ax.u_ij(x, 65-x,i=0,j=2,sexo="M")) +
    (C*ax.u_ij(x, 65-x,i=0,j=3,sexo="M")) +
    (D*ax.u_ij(x, 65-x,i=0,j=4,sexo="M"))
  benef_M_indiv <- rbind(benef_M_indiv, data.frame(benef_M_indiv = beneficios_por_edad_mujeres))
}

edades_selec_M <- cbind(edades_selec_M, benef_M_indiv = benef_M_indiv)
colnames(edades_selec_M)[5] <- "Benef_ind_por_edad"
benef_tot_por_edad_M <- edades_selec_M[, 5] * edades_selec_M[, 4]
benef_tot_por_edad_M <- as.vector(benef_tot_por_edad_M)
edades_selec_M <- cbind(edades_selec_M, benef_tot_por_edad_M)

suma_benef_M_total <- sum(edades_selec_M$benef_tot_por_edad_M)

##--- Primas -------------------------------------------------------------------

# hombres 

primas_hombres <- data.frame(Primas_hombres = numeric())
for (x in 30:64) {
  primas_por_edad_hombres <- (ax.n_ij(x,n = 65-x,i=0,j=0,sexo="H")) +
    (ax.n_ij(x,n = 65-x,i=0,j=1,sexo="H")) +
    (ax.n_ij(x,n = 65-x,i=0,j=2,sexo="H")) 
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
  primas_por_edad_mujeres <- (ax.n_ij(x,n = 65-x,i=0,0,5.8,inf=2.8818,"M")) +
    (ax.n_ij(x,n = 65-x,i=0,1,5.8,inf=2.8818,"M")) +
    (ax.n_ij(x,n = 65-x,i=0,2,5.8,inf=2.8818,"M")) 
  primas_mujeres <- rbind(primas_mujeres, data.frame(Primas_mujeres = primas_por_edad_mujeres))
}

edades_selec_M <- cbind(edades_selec_M, Primas_Mujeres = primas_mujeres)
colnames(edades_selec_M)[7] <- "primas_ind_por_edad "
primas_tot_por_edad_M <- edades_selec_M[, 7] * edades_selec_M[, 4]
primas_tot_por_edad_M <- as.vector(primas_tot_por_edad_M)
edades_selec_M <- cbind(edades_selec_M, primas_tot_por_edad_M)

primas_mujeres_suma <- sum(edades_selec_M$primas_tot_por_edad_M)


# principio de equivalencia

beneficios_totales <- suma_benef_M_total + suma_benef_H_total

primas_0.95 <- (primas_hombres_suma + primas_mujeres_suma)*0.95

costo_inicial <- 0.15*(sum(edades_selec_H$pob_estimada)+sum(edades_selec_M$pob_estimada))

total_primas <- primas_0.95 - costo_inicial

prima_anual <- beneficios_totales / total_primas #P_anual = 205,600.47

prima_mensual <- prima_anual/12 #P_mensual = 17,133.37

#Si consideramos primas separadas para población mujeres y población masculina sería:

#Hombres
primas_0.95_hombres <- primas_hombres_suma*0.95
total_primas_hombres <- primas_0.95_hombres - 0.15*sum(edades_selec_H$pob_estimada)
prima_hombres_anual <- suma_benef_H_total / total_primas_hombres #P_H_anual = 167,408.38
prima_hombres_mensual <- prima_hombres_anual/12 #P_H_mensual = 13,950.69

#Mujeres
primas_0.95_mujeres <- primas_mujeres_suma*0.95
total_primas_mujeres <- primas_0.95_mujeres - 0.15*sum(edades_selec_M$pob_estimada)
prima_mujeres_anual <- suma_benef_M_total / total_primas_mujeres #P_M_anual=243,169.13
prima_mujeres_mensual <- prima_mujeres_anual/12 #P_M_mensual = 20,264.09

#--Prima según la edad de entrada-----------------------------------------------------

Prima_justa_H <- edades_selec_H[,5]/(edades_selec_H[,7] - 0.15)
Prima_justa_H_mensual <- Prima_justa_H/12
edades_selec_H <- cbind(edades_selec_H, Prima_justa_anual = Prima_justa_H)
edades_selec_H <- cbind(edades_selec_H, Prima_justa_mensual = Prima_justa_H_mensual)

Prima_justa_M <- edades_selec_M[,5]/(edades_selec_M[,7] - 0.15)
Prima_justa_M_mensual <- Prima_justa_M/12
edades_selec_M <- cbind(edades_selec_M, Prima_justa_anual = Prima_justa_M)
edades_selec_M <- cbind(edades_selec_M, Prima_justa_mensual = Prima_justa_M_mensual)


##--------Graficos de población--------------

p = ggplot() + 
  geom_line(data = edades_selec_H, aes(x = Edad, y = pob_estimada, color = "Hombres"), linetype = "solid", size = 1) +
  geom_line(data = edades_selec_M, aes(x = Edad, y = pob_estimada, color = "Mujeres"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Hombres" = "lightblue4", "Mujeres" = "maroon"), name = "Población") +
  xlab('Edad') +
  ylab('Población estimada') + cowplot::theme_cowplot()


print(p)

#--- Modelo Deterministico Cantidad Esperada de Personas al final del año ------

# Esta función crea un dataframe con la proyección a 80 años de personas en 
# ciertos estados a ciertas edades partiendo de una edad base, estado y un sexo

obtencion_tabla_proyeccion <- function(x,status,sexo) {
  if (sexo == "H"){
    probabilidades <- Prob_Trans_Hombres
    poblacion_estimada <- edades_selec_H$pob_estimada
  }
  if (sexo == "M"){
    probabilidades <- Prob_Trans_Mujeres
    poblacion_estimada <- edades_selec_M$pob_estimada
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
  
  #se generan valores aleatorios correspondientes a la cantidad de población
  #perteneciente a cierto estado en el año cero.
  div_poblacion <- sample(poblacion_estimada[x-29], 5)
  
  tabla[1,status+2] = div_poblacion[status+1]
  
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


#Se crean listas que contienen las proyecciones de personas según la edad y 
#estado del año cero, separados por sexo.


#Caso hombres
lista_proyeccionesH <- list()
for(i in 1:nrow(edades_df)){
  lista_proyecciones <- list()
  x <- edades_df[i,]
  status <- c(0:4)
  for (j in status) {
    lista_proyecciones[[j+1]] <- obtencion_tabla_proyeccion(x,j,"H")
  }
  lista_proyeccionesH[[i]] <- lista_proyecciones
}

#Caso mujeres
lista_proyeccionesM <- list()
for(i in 1:nrow(edades_df)){
  lista_proyecciones <- list()
  x <- edades_df[i,]
  status <- c(0:5)
  for (j in status) {
    lista_proyecciones[[j+1]] <- obtencion_tabla_proyeccion(x,j,"M")
  }
  lista_proyeccionesM[[i]] <- lista_proyecciones
} 


#--- Modelo Estocastico Cantidad Esperada de Personas al final del año ---------
#Proyeccion a 80 años
#Esperanza y el percentil 99,5
#Se toman 100 bases de datos, cada una con 1000 personas

set.seed(123) #establece semilla para probabilidades

#Crear vector con 100 dataframes
vector_de_df <- vector("list", 100)
for (i in 1:100) {#Crear los df
  vector_de_df[[i]] <- data.frame(
    Edad = c(rep(30,5),
             rep(31,5),
             rep(32,8),
             rep(33,8),
             rep(34,11),
             rep(35,10),
             rep(36,15),
             rep(37,16),
             rep(38,21),
             rep(39,19),
             rep(40,23),
             rep(41,23),
             rep(42,26),
             rep(43,26),
             rep(44,29),
             rep(45,48),
             rep(46,45),
             rep(47,43),
             rep(48,41),
             rep(49,40),
             rep(50,40),
             rep(51,38),
             rep(52,38),
             rep(53,37),
             rep(54,37),
             rep(55,37),
             rep(56,37),
             rep(57,37),
             rep(58,37),
             rep(59,36),
             rep(60,35),
             rep(61,34),
             rep(62,33),
             rep(63,32),
             rep(64,30)),
    Año_0 = rep(0,1000),  
    Año_1 = runif(1000),  
    Año_2 = runif(1000),  
    Año_3 = runif(1000),  
    Año_4 = runif(1000),  
    Año_5 = runif(1000),  
    Año_6 = runif(1000),  
    Año_7 = runif(1000),  
    Año_8 = runif(1000),  
    Año_9 = runif(1000),
    Año_10 = runif(1000),
    Año_11 = runif(1000),
    Año_12 = runif(1000),
    Año_13 = runif(1000),
    Año_14 = runif(1000),
    Año_15 = runif(1000),
    Año_16 = runif(1000),
    Año_17 = runif(1000),
    Año_18 = runif(1000),
    Año_19 = runif(1000),
    Año_20 = runif(1000),
    Año_21 = runif(1000),
    Año_22 = runif(1000),
    Año_23 = runif(1000),
    Año_24 = runif(1000),
    Año_25 = runif(1000),
    Año_26 = runif(1000),
    Año_27 = runif(1000),
    Año_28 = runif(1000),
    Año_29 = runif(1000),
    Año_30 = runif(1000),
    Año_31 = runif(1000),
    Año_32 = runif(1000),
    Año_33 = runif(1000),
    Año_34 = runif(1000),
    Año_35 = runif(1000),
    Año_36 = runif(1000),
    Año_37 = runif(1000),
    Año_38 = runif(1000),
    Año_39 = runif(1000),
    Año_40 = runif(1000),
    Año_41 = runif(1000),
    Año_42 = runif(1000),
    Año_43 = runif(1000),
    Año_44 = runif(1000),
    Año_45 = runif(1000),
    Año_46 = runif(1000),
    Año_47 = runif(1000),
    Año_48 = runif(1000),
    Año_49 = runif(1000),
    Año_50 = runif(1000),
    Año_51 = runif(1000),
    Año_52 = runif(1000),
    Año_53 = runif(1000),
    Año_54 = runif(1000),
    Año_55 = runif(1000),
    Año_56 = runif(1000),
    Año_57 = runif(1000),
    Año_58 = runif(1000),
    Año_59 = runif(1000),
    Año_60 = runif(1000),
    Año_61 = runif(1000),
    Año_62 = runif(1000),
    Año_63 = runif(1000),
    Año_64 = runif(1000),
    Año_65 = runif(1000),
    Año_66 = runif(1000),
    Año_67 = runif(1000),
    Año_68 = runif(1000),
    Año_69 = runif(1000),
    Año_70 = runif(1000),
    Año_71 = runif(1000),
    Año_72 = runif(1000),
    Año_73 = runif(1000),
    Año_74 = runif(1000),
    Año_75 = runif(1000),
    Año_76 = runif(1000),
    Año_77 = runif(1000),
    Año_78 = runif(1000),
    Año_79 = runif(1000),
    Año_80 = runif(1000)
  )
}

#Obtener los estados de las personas simuladas
for(i in 1:1){#(i in 1:100){
  for(col in 3:82){
    for(fil in 1:1000){
      if(vector_de_df[[i]][fil,col-1]==0){
        fil_prob <-  0
      }
      if(vector_de_df[[i]][fil,col-1]==1){
        fil_prob <-  91
      }
      if(vector_de_df[[i]][fil,col-1]==2){
        fil_prob <-  182
      }
      if(vector_de_df[[i]][fil,col-1]==3){
        fil_prob <-  273
      }
      if(vector_de_df[[i]][fil,col-1]==4){
        fil_prob <-  364
      }
      if(vector_de_df[[i]][fil,col-1]==5){
        vector_de_df[[i]][fil,col] <- 5
        next
      }
      
      traspaso_0 <- Prob_Trans_Hombres[(vector_de_df[[i]]$Edad[fil]+col-22+fil_prob),3]#pasar al estado 0
      if(vector_de_df[[i]][fil,col]<=traspaso_0){
        vector_de_df[[i]][fil,col] <- 0
      }else{
        traspaso_1 <- traspaso_0 + Prob_Trans_Hombres[(vector_de_df[[i]]$Edad[fil]+col-22+fil_prob),4]#pasar al estado 1
        if(vector_de_df[[i]][fil,col]<=traspaso_1){
          vector_de_df[[i]][fil,col] <- 1
        }else{
          traspaso_2 <- traspaso_1 + Prob_Trans_Hombres[(vector_de_df[[i]]$Edad[fil]+col-22+fil_prob),5]#pasar al estado 2
          if(vector_de_df[[i]][fil,col]<=traspaso_2){
            vector_de_df[[i]][fil,col] <- 2
          }else{
            traspaso_3 <- traspaso_2 + Prob_Trans_Hombres[(vector_de_df[[i]]$Edad[fil]+col-22+fil_prob),6]#pasar al estado 3
            if(vector_de_df[[i]][fil,col]<=traspaso_3){
              vector_de_df[[i]][fil,col] <- 3
            }else{
              traspaso_4 <- traspaso_3 + Prob_Trans_Hombres[(vector_de_df[[i]]$Edad[fil]+col-22+fil_prob),7]#pasar al estado 4
              if(vector_de_df[[i]][fil,col]<=traspaso_4){
                vector_de_df[[i]][fil,col] <- 4
              }else{
                vector_de_df[[i]][fil,col] <- 5
              }
            }
          }
        }
      }
    }
  }
}

#--- Modelo Deterministico montos esperados de ingresos y egresos para cada uno estado -----


#--- Modelo Estocastico montos esperados de ingresos y egresos para cada uno estado -----






