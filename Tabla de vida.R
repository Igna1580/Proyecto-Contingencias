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
Prob_Trans_Hombres <- Prob_Trans_Hombres %>%
  mutate_at(vars(3:8), function(x) x / rowSums(select(., 3:8)))
Prob_Trans_Mujeres <- Prob_Trans_Mujeres %>%
  mutate_at(vars(3:8), function(x) x / rowSums(select(., 3:8)))


#--- Poblacion -----------------------------------------------------------------
edades <- 31:65

porcentajes <- rep(0.005, 35)  # Luego, 20 porcentajes uniformes de 0.6


# Caso hombres 

# Crear un dataframe para las edades
edades_df <- data.frame(Edad = edades-1)
edades_selec_H <- Hombres_2023_demografia[edades,] 

# Unir el dataframe de edades con el dataframe de hombres
edades_selec_H <- cbind(edades_df, edades_selec_H)

# Luego puedes agregar este vector de porcentajes al dataframe edades_selec_H
edades_selec_H$porc_estimados <- porcentajes

# Calcular la población por edad (multiplicar la cantidad de hombres por el porcentaje)
edades_selec_H$pob_estimada<- round(edades_selec_H$pob_H* edades_selec_H$porc_estimados, digits = 0)

# Caso Mujeres 
edades_selec_M <- Mujeres_2023_demografia[edades,] 
edades_selec_M <- cbind(edades_df, edades_selec_M)
edades_selec_M$porc_estimados <- porcentajes
edades_selec_M$pob_estimada<- round(edades_selec_M$pob_M * edades_selec_M$porc_estimados, digits = 0)


#--- Inflación e Interés --------------------------------------------------------

# inflación en Costa Rica de los últimos 10 años según Base de datos del Fondo Monetario Internacional, Banco Mundial e indicador del IPC de la OCDE
inflacion_data <- data.frame(
  Ano = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
  Costa_Rica = c(4.50, 5.23, 4.52, 0.80, -0.02, 1.63, 2.22, 2.10, 0.72, 1.73, 8.27)
)

inflacion <- mean(inflacion_data$Costa_Rica)/100
# inflación = 0.02881818


# descargamos la curva de rendimiento soberana de los últimos 6 meses
tasas_Descuento <- read_excel("descuento.xlsx") 

descuento <- round(mean(tasas_Descuento$`3 meses`), digits = 1)/100
# descuento 0.058


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
ax.n_ij <- function(x,n=65-x,i=0,j,r=5.8,inf=2.881818,sexo){
  prob <- obtencion_tabla_proyeccion(x,i,sexo)
  resultado <- 0
  for (e in 0:(n-1)){
    resultado <- ((1+inf/100)/(1+r/100))^(e)*prob[e+1,j+2] + resultado
  }
  return(resultado)
}

#Función anualidad diferida
u.ax.n_ij <- function(x,u=65-x,n=111-u-x,i=0, j,r=5.8,inf=2.881818,sexo){
  prob <- obtencion_tabla_proyeccion(x,i,sexo)
  resultado <- 0
  for (e in u:(u+n-1)){
    resultado <- ((1+inf/100)/(1+r/100))^(e)*prob[e+1,j+2] + resultado
  }
  return(resultado)
}

#Anualidades mensualizadas

#Función anualidad prepagable mensualizada

v <- (1+inflacion)/(1+descuento)
d <- 1-v
d_m <- 12*(1-(v^(1/12)))
i_m <- 12*((1+descuento)^(1/12)-1)
alfa <- (descuento*d)/(i_m*d_m)
beta <- (descuento-i_m)/(i_m*d_m)

ax.n_ij_12 <- function(x,n=110-x,i=0,j,r=5.8,sexo){
  
  prob <- obtencion_tabla_proyeccion(x,i,sexo)[n,j+2]
 
  anualidad <- alfa*ax.n_ij(x, n, i, j, r, sexo)-beta*(1-(v^n)*prob)
  
  return(anualidad)
}

#Función anualidad diferida mensualizada

u.ax.n_ij_12 <- function(x,u=65-x,n=110-x-u,i=0, j,r=5.8,sexo){
  
  prob1 <- obtencion_tabla_proyeccion(x,i,sexo)[n,j+2]
  prob2 <- obtencion_tabla_proyeccion(x,i,sexo)[u,j+2]
  
  anualidad <- alfa*u.ax.n_ij(x,u,n,i,j,r,sexo)-beta*((v^u)*prob2 -(v^n)*prob1)
  
  return(anualidad)
}


#--- Calculo Prima Generalizada -----------------------------------------------

A <- 183671.1703*12 #Beneficio estado 1
B <- 363381.6771*12 #Beneficio estado 2
C <- 740398.8263*12 #Beneficio estado 3
D <- 1572738.998*12 #Beneficio estado 4

##--- Beneficios ---------------------------------------------------------------

# Crear un vector vacío para almacenar los resultados de hombres
beneficios_por_edad_hombres <- c()
for (x in 30:64) {
  beneficios_por_edad_hombres <- c(beneficios_por_edad_hombres,
                                   (A*u.ax.n_ij(x,i=0,j=1,sexo="H") +
                                      B*u.ax.n_ij(x,i=0,j=2,sexo="H") +
                                      C*u.ax.n_ij(x,i=0,j=3,sexo="H") +
                                      D*u.ax.n_ij(x,i=0,j=4,sexo="H"))
  )
}
edades_selec_H$Benef_ind_por_edad <- beneficios_por_edad_hombres
edades_selec_H$benef_tot_por_edad <- edades_selec_H$pob_estimada*edades_selec_H$Benef_ind_por_edad

suma_benef_H_total <- sum(edades_selec_H$benef_tot_por_edad)


# Crear un vector vacío para almacenar los resultados de mujeres
beneficios_por_edad_mujeres <- c()
for (x in 30:64) {
  beneficios_por_edad_mujeres <- c(beneficios_por_edad_mujeres,
                                   (A*u.ax.n_ij(x,i=0,j=1,sexo="M") +
                                      B*u.ax.n_ij(x,i=0,j=2,sexo="M") +
                                      C*u.ax.n_ij(x,i=0,j=3,sexo="M") +
                                      D*u.ax.n_ij(x,i=0,j=4,sexo="M"))
  )
}
edades_selec_M$Benef_ind_por_edad <- beneficios_por_edad_mujeres
edades_selec_M$benef_tot_por_edad <- edades_selec_M$pob_estimada*edades_selec_M$Benef_ind_por_edad

suma_benef_M_total <- sum(edades_selec_M$benef_tot_por_edad)

##--- Primas -------------------------------------------------------------------

# hombres 

primas_por_edad_hombres <- c()
for (x in 30:64) {
  primas_por_edad_hombres <- c(primas_por_edad_hombres,
                               (ax.n_ij(x,n = 65-x,i=0,j=0,sexo="H") +
                                  ax.n_ij(x,n = 65-x,i=0,j=1,sexo="H") +
                                  ax.n_ij(x,n = 65-x,i=0,j=2,sexo="H"))
  )
}
edades_selec_H$primas_ind_por_edad <- primas_por_edad_hombres
edades_selec_H$prima_tot_por_edad <- edades_selec_H$pob_estimada*edades_selec_H$primas_ind_por_edad

primas_hombres_suma <- sum(edades_selec_H$prima_tot_por_edad)

# mujeres
primas_por_edad_mujeres <- c()
for (x in 30:64) {
  primas_por_edad_mujeres <- c(primas_por_edad_mujeres,
                               (ax.n_ij(x,n = 65-x,i=0,j=0,sexo="M") +
                                  ax.n_ij(x,n = 65-x,i=0,j=1,sexo="M") +
                                  ax.n_ij(x,n = 65-x,i=0,j=2,sexo="M"))
  )
}
edades_selec_M$primas_ind_por_edad <- primas_por_edad_mujeres
edades_selec_M$prima_tot_por_edad <- edades_selec_M$pob_estimada*edades_selec_M$primas_ind_por_edad

primas_mujeres_suma <- sum(edades_selec_M$prima_tot_por_edad)


# principio de equivalencia

beneficios_totales <- suma_benef_M_total + suma_benef_H_total

primas_0.95 <- (primas_hombres_suma + primas_mujeres_suma)*0.95

costo_inicial <- 0.15*(sum(edades_selec_H$pob_estimada)+sum(edades_selec_M$pob_estimada))

total_primas <- primas_0.95 - costo_inicial

prima_anual <- beneficios_totales / total_primas #P_anual = 1 659 838

#Si consideramos primas separadas para población mujeres y población masculina sería:

#Hombres
primas_0.95_hombres <- primas_hombres_suma*0.95
total_primas_hombres <- primas_0.95_hombres - 0.15*sum(edades_selec_H$pob_estimada)
prima_hombres_anual <- suma_benef_H_total / total_primas_hombres #P_H_anual = 1 348 142

#Mujeres
primas_0.95_mujeres <- primas_mujeres_suma*0.95
total_primas_mujeres <- primas_0.95_mujeres - 0.15*sum(edades_selec_M$pob_estimada)
prima_mujeres_anual <- suma_benef_M_total / total_primas_mujeres #P_M_anual= 1 975 022

#--Prima según la edad de entrada-----------------------------------------------------

Prima_justa_H <- edades_selec_H$Benef_ind_por_edad /(edades_selec_H$primas_ind_por_edad*0.95 - 0.15)
edades_selec_H <- cbind(edades_selec_H, Prima_justa_anual = Prima_justa_H)

Prima_justa_M <- edades_selec_M$Benef_ind_por_edad/(edades_selec_M$primas_ind_por_edad*0.95 - 0.15)
edades_selec_M <- cbind(edades_selec_M, Prima_justa_anual = Prima_justa_M)


##--------Graficos de población--------------

p = ggplot() + 
  geom_line(data = edades_selec_H, aes(x = Edad, y = pob_estimada, color = "Hombres"), linetype = "solid", linewidth = 1) +
  geom_line(data = edades_selec_M, aes(x = Edad, y = pob_estimada, color = "Mujeres"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Hombres" = "#9AC0CD", "Mujeres" = "#CD3333"), name = "Población") +
  xlab("Edad") +
  ylab("Población estimada") + cowplot::theme_cowplot()

print(p)

ggsave(filename = "poblacion.pdf", plot = p, device = "pdf", width = 5, height = 3)
# Crear un dataframe para los hombres
hombres <- edades_selec_H[, c("Edad", "pob_estimada")]

distrib.hombres.estimada <- ggplot(hombres, aes(x = Edad, y = pob_estimada)) +
  geom_bar(stat = "identity", fill = "lightblue4", color = "black") +
  xlab("Edad") +
  ylab("Poblacion de Hombres") +
  ggtitle("Distribucion de la Poblacion de hombres por Edad") +
  theme_minimal() + cowplot::theme_cowplot()
print(distrib.hombres.estimada)


edad_promedio_hombres <- sum(hombres$Edad * hombres$pob_estimada) / sum(hombres$pob_estimada)
varianza_hombres <- sum((hombres$Edad - edad_promedio_hombres)^2 * hombres$pob_estimada) / sum(hombres$pob_estimada)
hombres_moda <- max(hombres$pob_estimada) # 37 años
hombres$Sexo <- "Hombre"

# Crear un dataframe para las mujeres
mujeres <- edades_selec_M[, c("Edad", "pob_estimada")]

distrib.mujeres.estimada <- ggplot(mujeres, aes(x = Edad, y = pob_estimada)) +
  geom_bar(stat = "identity", fill = "maroon", color = "black") +
  xlab("Edad") +
  ylab("Poblacion de Mujeres") +
  ggtitle("Distribucion de la Poblacion de Mujeres por Edad") +
  theme_minimal() + cowplot::theme_cowplot()

print(distrib.mujeres.estimada)

edad_promedio_mujeres <- sum(mujeres$Edad * mujeres$pob_estimada) / sum(mujeres$pob_estimada)
mujeres_moda <- max(mujeres$pob_estimada) # 37 años
varianza_mujeres <- sum((mujeres$Edad - edad_promedio_mujeres)^2 * mujeres$pob_estimada) / sum(mujeres$pob_estimada)
mujeres$Sexo <- "Mujer"

# Combinar los dataframes
demografico <- rbind(hombres, mujeres)

# Definir la secuencia creciente
breaks <- c(seq(-1800, -200, by = 400), seq(0, 1800, by = 400))

# Negar la secuencia para las etiquetas
labels <- c(abs(seq(-1800, -200, by = 400)), seq(0, 1800, by = 400))

# Crear el gráfico con las nuevas ubicaciones de las marcas y etiquetas
graf.demografico <- ggplot(demografico, aes(x = Edad, y = pob_estimada, fill = Sexo)) +
  geom_col(data = subset(demografico, Sexo == "Hombre") %>% 
             mutate(pob_estimada = -pob_estimada),
           width = 0.5, fill = "#9AC0CD") +
  geom_col(data = subset(demografico, Sexo == "Mujer"),
           width = 0.5, fill = "#CD3333") +
  coord_flip() + 
  scale_y_continuous(breaks = breaks, labels = labels) +
  scale_fill_manual(values = c("Hombres" = "#9AC0CD", "Mujeres" = "#CD3333"), name = "Población") +
  xlab("Edad") +
  ylab("Población estimada") + cowplot::theme_cowplot()  +
  annotate("text", x = Inf, y = Inf, hjust = 6.5, vjust = 3, label = "Hombres") +
  annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 3, label = "Mujeres")
ggsave(filename = "piramide.pdf", plot = graf.demografico, device = "pdf", width = 5, height = 3)

print(graf.demografico)



#--- Modelo Deterministico Cantidad Esperada de Personas al final del año ------

# Esta función crea un dataframe con la proyección a 80 años de personas en 
# ciertos estados a ciertas edades partiendo de una edad base, estado y un sexo

tabla_proyeccion_80años <- function(x,status,sexo) {
  if (sexo == "H"){
    probabilidades <- Prob_Trans_Hombres
    poblacion_estimada <- edades_selec_H$pob_estimada
  }
  if (sexo == "M"){
    probabilidades <- Prob_Trans_Mujeres
    poblacion_estimada <- edades_selec_M$pob_estimada
  }
  
  tabla <- data.frame(
    "edad" = x:(x + 81),
    "l_age.x_sta.0" = rep(0, 82),
    "l_age.x_sta.1" = rep(0, 82),
    "l_age.x_sta.2" = rep(0, 82),
    "l_age.x_sta.3" = rep(0, 82),
    "l_age.x_sta.4" = rep(0, 82),
    "l_age.x_sta.5" = rep(0, 82)
  )
  
  tabla[1,status+2] = poblacion_estimada[x-29]
  

  for (fila in 2:length(x:111)){
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
  
  if(0 %in% tabla$l_age.x_sta.5[2:82]) {
    tabla$l_age.x_sta.5[(113-x):82] <-tabla$l_age.x_sta.5[112-x]
  }
  
  return (tabla)
}

#Se crean listas que contienen las proyecciones a 80 años de personas 
#según la edad y estado del año cero (able), separados por sexo.

#Caso hombres
lista_proyeccionesH <- list()
#Caso mujeres
lista_proyeccionesM <- list()
for(i in 1:nrow(edades_df)){
  x <- edades_df[i,]
  lista_proyeccionesH[[i]] <- tabla_proyeccion_80años(x,0,"H")
  lista_proyeccionesM[[i]] <- tabla_proyeccion_80años(x,0,"M")
}

#Se obtiene la proyección total a 80 años para cada estado para hombres 
#y mujeres

#Hombres
tabla_proyeccionesH_total <- data.frame(
  "Año" = 0:81,
  "Estado 0" = rep(0, 82),
  "Estado 1" = rep(0, 82),
  "Estado 2" = rep(0, 82),
  "Estado 3" = rep(0, 82),
  "Estado 4" = rep(0, 82),
  "Estado 5" = rep(0, 82)
)

#Mujeres
tabla_proyeccionesM_total <- data.frame(
  "Año" = 0:81,
  "Estado 0" = rep(0, 82),
  "Estado 1" = rep(0, 82),
  "Estado 2" = rep(0, 82),
  "Estado 3" = rep(0, 82),
  "Estado 4" = rep(0, 82),
  "Estado 5" = rep(0, 82)
)

for(i in 1: 82) {
  sumaH <- c(0)
  sumaM <- c(0)
  for (j in 1:35) {
    sumaH <- sumaH + lista_proyeccionesH[[j]][i,]
    sumaM <- sumaM + lista_proyeccionesM[[j]][i,]
  }
  tabla_proyeccionesH_total$Estado.0[i] <- sumaH[2]
  tabla_proyeccionesH_total$Estado.1[i] <- sumaH[3]
  tabla_proyeccionesH_total$Estado.2[i] <- sumaH[4]
  tabla_proyeccionesH_total$Estado.3[i] <- sumaH[5]
  tabla_proyeccionesH_total$Estado.4[i] <- sumaH[6]
  tabla_proyeccionesH_total$Estado.5[i] <- sumaH[7]
  
  tabla_proyeccionesM_total$Estado.0[i] <- sumaM[2]
  tabla_proyeccionesM_total$Estado.1[i] <- sumaM[3]
  tabla_proyeccionesM_total$Estado.2[i] <- sumaM[4]
  tabla_proyeccionesM_total$Estado.3[i] <- sumaM[5]
  tabla_proyeccionesM_total$Estado.4[i] <- sumaM[6]
  tabla_proyeccionesM_total$Estado.5[i] <- sumaM[7]
}

proyeccion_H = ggplot() + 
  geom_line(data = tabla_proyeccionesH_total, aes(x = Año, y = as.numeric(Estado.0), color = "Able"), linetype = "solid", size = 1) +
  geom_line(data = tabla_proyeccionesH_total, aes(x = Año, y = as.numeric(Estado.1) , color = "Mild"), linetype = "solid", size = 1) +
  geom_line(data = tabla_proyeccionesH_total, aes(x = Año, y = as.numeric(Estado.2) , color = "Moderate"), linetype = "solid", size = 1) +
  geom_line(data = tabla_proyeccionesH_total, aes(x = Año, y = as.numeric(Estado.3) , color = "Severe"), linetype = "solid", size = 1) +
  geom_line(data = tabla_proyeccionesH_total, aes(x = Año, y = as.numeric(Estado.4) , color = "Profound"), linetype = "solid", size = 1) +
  geom_line(data = tabla_proyeccionesH_total, aes(x = Año, y = as.numeric(Estado.5) , color = "Dead"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Able" = "lightblue4", "Mild" = "maroon", "Moderate" = "darkblue", "Severe" = "purple", "Profound" = "pink", "Dead" ="red"), name = "Estado") +
  xlab('Año') +
  ylab('Personas esperadas (Hombres)') + cowplot::theme_cowplot()

proyeccion_M = ggplot() + 
  geom_line(data = tabla_proyeccionesM_total, aes(x = Año, y = as.numeric(Estado.0), color = "Able"), linetype = "solid", size = 1) +
  geom_line(data = tabla_proyeccionesM_total, aes(x = Año, y = as.numeric(Estado.1) , color = "Mild"), linetype = "solid", size = 1) +
  geom_line(data = tabla_proyeccionesM_total, aes(x = Año, y = as.numeric(Estado.2) , color = "Moderate"), linetype = "solid", size = 1) +
  geom_line(data = tabla_proyeccionesM_total, aes(x = Año, y = as.numeric(Estado.3) , color = "Severe"), linetype = "solid", size = 1) +
  geom_line(data = tabla_proyeccionesM_total, aes(x = Año, y = as.numeric(Estado.4) , color = "Profound"), linetype = "solid", size = 1) +
  geom_line(data = tabla_proyeccionesM_total, aes(x = Año, y = as.numeric(Estado.5) , color = "Dead"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Able" = "lightblue4", "Mild" = "maroon", "Moderate" = "darkblue", "Severe" = "purple", "Profound" = "pink", "Dead" ="red"), name = "Estado") +
  xlab('Año') +
  ylab('Personas esperadas (Mujeres)') + cowplot::theme_cowplot()

print(proyeccion_H)
print(proyeccion_M)

#--- Modelo Estocastico Cantidad Esperada de Personas al final del año ---------
#Proyeccion a 80 años
#Esperanza y el percentil 99,5
#Se toman 100 bases de datos

set.seed(123) #establece semilla para probabilidades

##--- Simulacion ---------------------------------------------------------------
#Creamos la fucnion simulacion
func_simulacion <- function(edad, col_ant, col_act, sexo, año){
  if(sexo == "H"){
    proba <- Prob_Trans_Hombres
  }
  if(sexo == "M"){
    proba <- Prob_Trans_Mujeres
  }
  
  if(col_ant == 5){
    return(5)
  }
  if(col_ant == 0){
    fil_prob <-  0
  }
  if(col_ant == 1){
    fil_prob <- 91
  }
  if(col_ant == 2){
    fil_prob <- 182
  }
  if(col_ant == 3){
    fil_prob <- 273
  }
  if(col_ant == 4){
    fil_prob <- 364
  }
  
  
  if(col_act < proba[edad+fil_prob+año-19,3]){#Pasar al estado 0
    return(0)
  }else{
    if(col_act < sum(proba[edad+fil_prob+año-19,3:4])){#Pasar al estado 1
      return(1)
    }else{
      if(col_act < sum(proba[edad+fil_prob+año-19,3:5])){#Pasar al estado 2
        return(2)
      }else{
        if(col_act < sum(proba[edad+fil_prob+año-19,3:6])){#Pasar al estado 3
          return(3)
        }else{
          if(col_act < sum(proba[edad+fil_prob+año-19,3:7])){#Pasar al estado 4
            return(4)
          }else{
            return(5)
          }
        }
      }
    }
  }
}


####--- Hombres ----

#Crear vector con 100 dataframes
vector_simulacion_H <- vector("list", 100)
pob_tot_H <- sum(edades_selec_H$pob_estimada)
for (i in 1:100) {#Crear los df
  vector_simulacion_H[[i]] <- data.frame(
    Edad = c(rep(30,edades_selec_H[1,4]),
             rep(31,edades_selec_H[2,4]),
             rep(32,edades_selec_H[3,4]),
             rep(33,edades_selec_H[4,4]),
             rep(34,edades_selec_H[5,4]),
             rep(35,edades_selec_H[6,4]),
             rep(36,edades_selec_H[7,4]),
             rep(37,edades_selec_H[8,4]),
             rep(38,edades_selec_H[9,4]),
             rep(39,edades_selec_H[10,4]),
             rep(40,edades_selec_H[11,4]),
             rep(41,edades_selec_H[12,4]),
             rep(42,edades_selec_H[13,4]),
             rep(43,edades_selec_H[14,4]),
             rep(44,edades_selec_H[15,4]),
             rep(45,edades_selec_H[16,4]),
             rep(46,edades_selec_H[17,4]),
             rep(47,edades_selec_H[18,4]),
             rep(48,edades_selec_H[19,4]),
             rep(49,edades_selec_H[20,4]),
             rep(50,edades_selec_H[21,4]),
             rep(51,edades_selec_H[22,4]),
             rep(52,edades_selec_H[23,4]),
             rep(53,edades_selec_H[24,4]),
             rep(54,edades_selec_H[25,4]),
             rep(55,edades_selec_H[26,4]),
             rep(56,edades_selec_H[27,4]),
             rep(57,edades_selec_H[28,4]),
             rep(58,edades_selec_H[29,4]),
             rep(59,edades_selec_H[30,4]),
             rep(60,edades_selec_H[31,4]),
             rep(61,edades_selec_H[32,4]),
             rep(62,edades_selec_H[33,4]),
             rep(63,edades_selec_H[34,4]),
             rep(64,edades_selec_H[35,4])),
    Año_0 = rep(0,pob_tot_H),  
    Año_1 = runif(pob_tot_H),  
    Año_2 = runif(pob_tot_H),  
    Año_3 = runif(pob_tot_H),  
    Año_4 = runif(pob_tot_H),  
    Año_5 = runif(pob_tot_H),  
    Año_6 = runif(pob_tot_H),  
    Año_7 = runif(pob_tot_H),  
    Año_8 = runif(pob_tot_H),  
    Año_9 = runif(pob_tot_H),
    Año_10 = runif(pob_tot_H),
    Año_11 = runif(pob_tot_H),
    Año_12 = runif(pob_tot_H),
    Año_13 = runif(pob_tot_H),
    Año_14 = runif(pob_tot_H),
    Año_15 = runif(pob_tot_H),
    Año_16 = runif(pob_tot_H),
    Año_17 = runif(pob_tot_H),
    Año_18 = runif(pob_tot_H),
    Año_19 = runif(pob_tot_H),
    Año_20 = runif(pob_tot_H),
    Año_21 = runif(pob_tot_H),
    Año_22 = runif(pob_tot_H),
    Año_23 = runif(pob_tot_H),
    Año_24 = runif(pob_tot_H),
    Año_25 = runif(pob_tot_H),
    Año_26 = runif(pob_tot_H),
    Año_27 = runif(pob_tot_H),
    Año_28 = runif(pob_tot_H),
    Año_29 = runif(pob_tot_H),
    Año_30 = runif(pob_tot_H),
    Año_31 = runif(pob_tot_H),
    Año_32 = runif(pob_tot_H),
    Año_33 = runif(pob_tot_H),
    Año_34 = runif(pob_tot_H),
    Año_35 = runif(pob_tot_H),
    Año_36 = runif(pob_tot_H),
    Año_37 = runif(pob_tot_H),
    Año_38 = runif(pob_tot_H),
    Año_39 = runif(pob_tot_H),
    Año_40 = runif(pob_tot_H),
    Año_41 = runif(pob_tot_H),
    Año_42 = runif(pob_tot_H),
    Año_43 = runif(pob_tot_H),
    Año_44 = runif(pob_tot_H),
    Año_45 = runif(pob_tot_H),
    Año_46 = runif(pob_tot_H),
    Año_47 = runif(pob_tot_H),
    Año_48 = runif(pob_tot_H),
    Año_49 = runif(pob_tot_H),
    Año_50 = runif(pob_tot_H),
    Año_51 = runif(pob_tot_H),
    Año_52 = runif(pob_tot_H),
    Año_53 = runif(pob_tot_H),
    Año_54 = runif(pob_tot_H),
    Año_55 = runif(pob_tot_H),
    Año_56 = runif(pob_tot_H),
    Año_57 = runif(pob_tot_H),
    Año_58 = runif(pob_tot_H),
    Año_59 = runif(pob_tot_H),
    Año_60 = runif(pob_tot_H),
    Año_61 = runif(pob_tot_H),
    Año_62 = runif(pob_tot_H),
    Año_63 = runif(pob_tot_H),
    Año_64 = runif(pob_tot_H),
    Año_65 = runif(pob_tot_H),
    Año_66 = runif(pob_tot_H),
    Año_67 = runif(pob_tot_H),
    Año_68 = runif(pob_tot_H),
    Año_69 = runif(pob_tot_H),
    Año_70 = runif(pob_tot_H),
    Año_71 = runif(pob_tot_H),
    Año_72 = runif(pob_tot_H),
    Año_73 = runif(pob_tot_H),
    Año_74 = runif(pob_tot_H),
    Año_75 = runif(pob_tot_H),
    Año_76 = runif(pob_tot_H),
    Año_77 = runif(pob_tot_H),
    Año_78 = runif(pob_tot_H),
    Año_79 = runif(pob_tot_H),
    Año_80 = runif(pob_tot_H)
  )
}

#Obtener los estados de las personas simuladas
for(i in 1:100){
  print(paste(i,"%"))
  vector_simulacion_H[[i]] <- vector_simulacion_H[[i]] %>%
    rowwise() %>%
    mutate(Año_1 = func_simulacion(Edad, Año_0, Año_1,sexo="H",año=0),
           Año_2 = func_simulacion(Edad, Año_1, Año_2,sexo="H",año=1),
           Año_3 = func_simulacion(Edad, Año_2, Año_3,sexo="H",año=2),
           Año_4 = func_simulacion(Edad, Año_3, Año_4,sexo="H",año=3),
           Año_5 = func_simulacion(Edad, Año_4, Año_5,sexo="H",año=4),
           Año_6 = func_simulacion(Edad, Año_5, Año_6,sexo="H",año=5),
           Año_7 = func_simulacion(Edad, Año_6, Año_7,sexo="H",año=6),
           Año_8 = func_simulacion(Edad, Año_7, Año_8,sexo="H",año=7),
           Año_9 = func_simulacion(Edad, Año_8, Año_9,sexo="H",año=8),
           Año_10 = func_simulacion(Edad, Año_9, Año_10,sexo="H",año=9),
           Año_11 = func_simulacion(Edad, Año_10, Año_11,sexo="H",año=10),
           Año_12 = func_simulacion(Edad, Año_11, Año_12,sexo="H",año=11),
           Año_13 = func_simulacion(Edad, Año_12, Año_13,sexo="H",año=12),
           Año_14 = func_simulacion(Edad, Año_13, Año_14,sexo="H",año=13),
           Año_15 = func_simulacion(Edad, Año_14, Año_15,sexo="H",año=14),
           Año_16 = func_simulacion(Edad, Año_15, Año_16,sexo="H",año=15),
           Año_17 = func_simulacion(Edad, Año_16, Año_17,sexo="H",año=16),
           Año_18 = func_simulacion(Edad, Año_17, Año_18,sexo="H",año=17),
           Año_19 = func_simulacion(Edad, Año_18, Año_19,sexo="H",año=18),
           Año_20 = func_simulacion(Edad, Año_19, Año_20,sexo="H",año=19),
           Año_21 = func_simulacion(Edad, Año_20, Año_21,sexo="H",año=20),
           Año_22 = func_simulacion(Edad, Año_21, Año_22,sexo="H",año=21),
           Año_23 = func_simulacion(Edad, Año_22, Año_23,sexo="H",año=22),
           Año_24 = func_simulacion(Edad, Año_23, Año_24,sexo="H",año=23),
           Año_25 = func_simulacion(Edad, Año_24, Año_25,sexo="H",año=24),
           Año_26 = func_simulacion(Edad, Año_25, Año_26,sexo="H",año=25),
           Año_27 = func_simulacion(Edad, Año_26, Año_27,sexo="H",año=26),
           Año_28 = func_simulacion(Edad, Año_27, Año_28,sexo="H",año=27),
           Año_29 = func_simulacion(Edad, Año_28, Año_29,sexo="H",año=28),
           Año_30 = func_simulacion(Edad, Año_29, Año_30,sexo="H",año=29),
           Año_31 = func_simulacion(Edad, Año_30, Año_31,sexo="H",año=30),
           Año_32 = func_simulacion(Edad, Año_31, Año_32,sexo="H",año=31),
           Año_33 = func_simulacion(Edad, Año_32, Año_33,sexo="H",año=32),
           Año_34 = func_simulacion(Edad, Año_33, Año_34,sexo="H",año=33),
           Año_35 = func_simulacion(Edad, Año_34, Año_35,sexo="H",año=34),
           Año_36 = func_simulacion(Edad, Año_35, Año_36,sexo="H",año=35),
           Año_37 = func_simulacion(Edad, Año_36, Año_37,sexo="H",año=36),
           Año_38 = func_simulacion(Edad, Año_37, Año_38,sexo="H",año=37),
           Año_39 = func_simulacion(Edad, Año_38, Año_39,sexo="H",año=38),
           Año_40 = func_simulacion(Edad, Año_39, Año_40,sexo="H",año=39),
           Año_41 = func_simulacion(Edad, Año_40, Año_41,sexo="H",año=40),
           Año_42 = func_simulacion(Edad, Año_41, Año_42,sexo="H",año=41),
           Año_43 = func_simulacion(Edad, Año_42, Año_43,sexo="H",año=42),
           Año_44 = func_simulacion(Edad, Año_43, Año_44,sexo="H",año=43),
           Año_45 = func_simulacion(Edad, Año_44, Año_45,sexo="H",año=44),
           Año_46 = func_simulacion(Edad, Año_45, Año_46,sexo="H",año=45),
           Año_47 = func_simulacion(Edad, Año_46, Año_47,sexo="H",año=46),
           Año_48 = func_simulacion(Edad, Año_47, Año_48,sexo="H",año=47),
           Año_49 = func_simulacion(Edad, Año_48, Año_49,sexo="H",año=48),
           Año_50 = func_simulacion(Edad, Año_49, Año_50,sexo="H",año=49),
           Año_51 = func_simulacion(Edad, Año_50, Año_51,sexo="H",año=50),
           Año_52 = func_simulacion(Edad, Año_51, Año_52,sexo="H",año=51),
           Año_53 = func_simulacion(Edad, Año_52, Año_53,sexo="H",año=52),
           Año_54 = func_simulacion(Edad, Año_53, Año_54,sexo="H",año=53),
           Año_55 = func_simulacion(Edad, Año_54, Año_55,sexo="H",año=54),
           Año_56 = func_simulacion(Edad, Año_55, Año_56,sexo="H",año=55),
           Año_57 = func_simulacion(Edad, Año_56, Año_57,sexo="H",año=56),
           Año_58 = func_simulacion(Edad, Año_57, Año_58,sexo="H",año=57),
           Año_59 = func_simulacion(Edad, Año_58, Año_59,sexo="H",año=58),
           Año_60 = func_simulacion(Edad, Año_59, Año_60,sexo="H",año=59),
           Año_61 = func_simulacion(Edad, Año_60, Año_61,sexo="H",año=60),
           Año_62 = func_simulacion(Edad, Año_61, Año_62,sexo="H",año=61),
           Año_63 = func_simulacion(Edad, Año_62, Año_63,sexo="H",año=62),
           Año_64 = func_simulacion(Edad, Año_63, Año_64,sexo="H",año=63),
           Año_65 = func_simulacion(Edad, Año_64, Año_65,sexo="H",año=64),
           Año_66 = func_simulacion(Edad, Año_65, Año_66,sexo="H",año=65),
           Año_67 = func_simulacion(Edad, Año_66, Año_67,sexo="H",año=66),
           Año_68 = func_simulacion(Edad, Año_67, Año_68,sexo="H",año=67),
           Año_69 = func_simulacion(Edad, Año_68, Año_69,sexo="H",año=68),
           Año_70 = func_simulacion(Edad, Año_69, Año_70,sexo="H",año=69),
           Año_71 = func_simulacion(Edad, Año_70, Año_71,sexo="H",año=70),
           Año_72 = func_simulacion(Edad, Año_71, Año_72,sexo="H",año=71),
           Año_73 = func_simulacion(Edad, Año_72, Año_73,sexo="H",año=72),
           Año_74 = func_simulacion(Edad, Año_73, Año_74,sexo="H",año=73),
           Año_75 = func_simulacion(Edad, Año_74, Año_75,sexo="H",año=74),
           Año_76 = func_simulacion(Edad, Año_75, Año_76,sexo="H",año=75),
           Año_77 = func_simulacion(Edad, Año_76, Año_77,sexo="H",año=76),
           Año_78 = func_simulacion(Edad, Año_77, Año_78,sexo="H",año=77),
           Año_79 = func_simulacion(Edad, Año_78, Año_79,sexo="H",año=78),
           Año_80 = func_simulacion(Edad, Año_79, Año_80,sexo="H",año=79),
    )
}


####--- Mujeres----
#Crear vector con 100 dataframes
vector_simulacion_M <- vector("list", 100)
pob_tot_M <- sum(edades_selec_M$pob_estimada)
for (i in 1:100) {#Crear los df
  vector_simulacion_M[[i]] <- data.frame(
    Edad = c(rep(30,edades_selec_M[1,4]),
             rep(31,edades_selec_M[2,4]),
             rep(32,edades_selec_M[3,4]),
             rep(33,edades_selec_M[4,4]),
             rep(34,edades_selec_M[5,4]),
             rep(35,edades_selec_M[6,4]),
             rep(36,edades_selec_M[7,4]),
             rep(37,edades_selec_M[8,4]),
             rep(38,edades_selec_M[9,4]),
             rep(39,edades_selec_M[10,4]),
             rep(40,edades_selec_M[11,4]),
             rep(41,edades_selec_M[12,4]),
             rep(42,edades_selec_M[13,4]),
             rep(43,edades_selec_M[14,4]),
             rep(44,edades_selec_M[15,4]),
             rep(45,edades_selec_M[16,4]),
             rep(46,edades_selec_M[17,4]),
             rep(47,edades_selec_M[18,4]),
             rep(48,edades_selec_M[19,4]),
             rep(49,edades_selec_M[20,4]),
             rep(50,edades_selec_M[21,4]),
             rep(51,edades_selec_M[22,4]),
             rep(52,edades_selec_M[23,4]),
             rep(53,edades_selec_M[24,4]),
             rep(54,edades_selec_M[25,4]),
             rep(55,edades_selec_M[26,4]),
             rep(56,edades_selec_M[27,4]),
             rep(57,edades_selec_M[28,4]),
             rep(58,edades_selec_M[29,4]),
             rep(59,edades_selec_M[30,4]),
             rep(60,edades_selec_M[31,4]),
             rep(61,edades_selec_M[32,4]),
             rep(62,edades_selec_M[33,4]),
             rep(63,edades_selec_M[34,4]),
             rep(64,edades_selec_M[35,4])),
    Año_0 = rep(0,pob_tot_M),  
    Año_1 = runif(pob_tot_M),  
    Año_2 = runif(pob_tot_M),  
    Año_3 = runif(pob_tot_M),  
    Año_4 = runif(pob_tot_M),  
    Año_5 = runif(pob_tot_M),  
    Año_6 = runif(pob_tot_M),  
    Año_7 = runif(pob_tot_M),  
    Año_8 = runif(pob_tot_M),  
    Año_9 = runif(pob_tot_M),
    Año_10 = runif(pob_tot_M),
    Año_11 = runif(pob_tot_M),
    Año_12 = runif(pob_tot_M),
    Año_13 = runif(pob_tot_M),
    Año_14 = runif(pob_tot_M),
    Año_15 = runif(pob_tot_M),
    Año_16 = runif(pob_tot_M),
    Año_17 = runif(pob_tot_M),
    Año_18 = runif(pob_tot_M),
    Año_19 = runif(pob_tot_M),
    Año_20 = runif(pob_tot_M),
    Año_21 = runif(pob_tot_M),
    Año_22 = runif(pob_tot_M),
    Año_23 = runif(pob_tot_M),
    Año_24 = runif(pob_tot_M),
    Año_25 = runif(pob_tot_M),
    Año_26 = runif(pob_tot_M),
    Año_27 = runif(pob_tot_M),
    Año_28 = runif(pob_tot_M),
    Año_29 = runif(pob_tot_M),
    Año_30 = runif(pob_tot_M),
    Año_31 = runif(pob_tot_M),
    Año_32 = runif(pob_tot_M),
    Año_33 = runif(pob_tot_M),
    Año_34 = runif(pob_tot_M),
    Año_35 = runif(pob_tot_M),
    Año_36 = runif(pob_tot_M),
    Año_37 = runif(pob_tot_M),
    Año_38 = runif(pob_tot_M),
    Año_39 = runif(pob_tot_M),
    Año_40 = runif(pob_tot_M),
    Año_41 = runif(pob_tot_M),
    Año_42 = runif(pob_tot_M),
    Año_43 = runif(pob_tot_M),
    Año_44 = runif(pob_tot_M),
    Año_45 = runif(pob_tot_M),
    Año_46 = runif(pob_tot_M),
    Año_47 = runif(pob_tot_M),
    Año_48 = runif(pob_tot_M),
    Año_49 = runif(pob_tot_M),
    Año_50 = runif(pob_tot_M),
    Año_51 = runif(pob_tot_M),
    Año_52 = runif(pob_tot_M),
    Año_53 = runif(pob_tot_M),
    Año_54 = runif(pob_tot_M),
    Año_55 = runif(pob_tot_M),
    Año_56 = runif(pob_tot_M),
    Año_57 = runif(pob_tot_M),
    Año_58 = runif(pob_tot_M),
    Año_59 = runif(pob_tot_M),
    Año_60 = runif(pob_tot_M),
    Año_61 = runif(pob_tot_M),
    Año_62 = runif(pob_tot_M),
    Año_63 = runif(pob_tot_M),
    Año_64 = runif(pob_tot_M),
    Año_65 = runif(pob_tot_M),
    Año_66 = runif(pob_tot_M),
    Año_67 = runif(pob_tot_M),
    Año_68 = runif(pob_tot_M),
    Año_69 = runif(pob_tot_M),
    Año_70 = runif(pob_tot_M),
    Año_71 = runif(pob_tot_M),
    Año_72 = runif(pob_tot_M),
    Año_73 = runif(pob_tot_M),
    Año_74 = runif(pob_tot_M),
    Año_75 = runif(pob_tot_M),
    Año_76 = runif(pob_tot_M),
    Año_77 = runif(pob_tot_M),
    Año_78 = runif(pob_tot_M),
    Año_79 = runif(pob_tot_M),
    Año_80 = runif(pob_tot_M)
  )
}

#Obtener los estados de las personas simuladas
for(i in 1:100){
  print(paste(i,"%"))
  vector_simulacion_M[[i]] <- vector_simulacion_M[[i]] %>%
    rowwise() %>%
    mutate(Año_1 = func_simulacion(Edad, Año_0, Año_1,sexo="M",año=0),
           Año_2 = func_simulacion(Edad, Año_1, Año_2,sexo="M",año=1),
           Año_3 = func_simulacion(Edad, Año_2, Año_3,sexo="M",año=2),
           Año_4 = func_simulacion(Edad, Año_3, Año_4,sexo="M",año=3),
           Año_5 = func_simulacion(Edad, Año_4, Año_5,sexo="M",año=4),
           Año_6 = func_simulacion(Edad, Año_5, Año_6,sexo="M",año=5),
           Año_7 = func_simulacion(Edad, Año_6, Año_7,sexo="M",año=6),
           Año_8 = func_simulacion(Edad, Año_7, Año_8,sexo="M",año=7),
           Año_9 = func_simulacion(Edad, Año_8, Año_9,sexo="M",año=8),
           Año_10 = func_simulacion(Edad, Año_9, Año_10,sexo="M",año=9),
           Año_11 = func_simulacion(Edad, Año_10, Año_11,sexo="M",año=10),
           Año_12 = func_simulacion(Edad, Año_11, Año_12,sexo="M",año=11),
           Año_13 = func_simulacion(Edad, Año_12, Año_13,sexo="M",año=12),
           Año_14 = func_simulacion(Edad, Año_13, Año_14,sexo="M",año=13),
           Año_15 = func_simulacion(Edad, Año_14, Año_15,sexo="M",año=14),
           Año_16 = func_simulacion(Edad, Año_15, Año_16,sexo="M",año=15),
           Año_17 = func_simulacion(Edad, Año_16, Año_17,sexo="M",año=16),
           Año_18 = func_simulacion(Edad, Año_17, Año_18,sexo="M",año=17),
           Año_19 = func_simulacion(Edad, Año_18, Año_19,sexo="M",año=18),
           Año_20 = func_simulacion(Edad, Año_19, Año_20,sexo="M",año=19),
           Año_21 = func_simulacion(Edad, Año_20, Año_21,sexo="M",año=20),
           Año_22 = func_simulacion(Edad, Año_21, Año_22,sexo="M",año=21),
           Año_23 = func_simulacion(Edad, Año_22, Año_23,sexo="M",año=22),
           Año_24 = func_simulacion(Edad, Año_23, Año_24,sexo="M",año=23),
           Año_25 = func_simulacion(Edad, Año_24, Año_25,sexo="M",año=24),
           Año_26 = func_simulacion(Edad, Año_25, Año_26,sexo="M",año=25),
           Año_27 = func_simulacion(Edad, Año_26, Año_27,sexo="M",año=26),
           Año_28 = func_simulacion(Edad, Año_27, Año_28,sexo="M",año=27),
           Año_29 = func_simulacion(Edad, Año_28, Año_29,sexo="M",año=28),
           Año_30 = func_simulacion(Edad, Año_29, Año_30,sexo="M",año=29),
           Año_31 = func_simulacion(Edad, Año_30, Año_31,sexo="M",año=30),
           Año_32 = func_simulacion(Edad, Año_31, Año_32,sexo="M",año=31),
           Año_33 = func_simulacion(Edad, Año_32, Año_33,sexo="M",año=32),
           Año_34 = func_simulacion(Edad, Año_33, Año_34,sexo="M",año=33),
           Año_35 = func_simulacion(Edad, Año_34, Año_35,sexo="M",año=34),
           Año_36 = func_simulacion(Edad, Año_35, Año_36,sexo="M",año=35),
           Año_37 = func_simulacion(Edad, Año_36, Año_37,sexo="M",año=36),
           Año_38 = func_simulacion(Edad, Año_37, Año_38,sexo="M",año=37),
           Año_39 = func_simulacion(Edad, Año_38, Año_39,sexo="M",año=38),
           Año_40 = func_simulacion(Edad, Año_39, Año_40,sexo="M",año=39),
           Año_41 = func_simulacion(Edad, Año_40, Año_41,sexo="M",año=40),
           Año_42 = func_simulacion(Edad, Año_41, Año_42,sexo="M",año=41),
           Año_43 = func_simulacion(Edad, Año_42, Año_43,sexo="M",año=42),
           Año_44 = func_simulacion(Edad, Año_43, Año_44,sexo="M",año=43),
           Año_45 = func_simulacion(Edad, Año_44, Año_45,sexo="M",año=44),
           Año_46 = func_simulacion(Edad, Año_45, Año_46,sexo="M",año=45),
           Año_47 = func_simulacion(Edad, Año_46, Año_47,sexo="M",año=46),
           Año_48 = func_simulacion(Edad, Año_47, Año_48,sexo="M",año=47),
           Año_49 = func_simulacion(Edad, Año_48, Año_49,sexo="M",año=48),
           Año_50 = func_simulacion(Edad, Año_49, Año_50,sexo="M",año=49),
           Año_51 = func_simulacion(Edad, Año_50, Año_51,sexo="M",año=50),
           Año_52 = func_simulacion(Edad, Año_51, Año_52,sexo="M",año=51),
           Año_53 = func_simulacion(Edad, Año_52, Año_53,sexo="M",año=52),
           Año_54 = func_simulacion(Edad, Año_53, Año_54,sexo="M",año=53),
           Año_55 = func_simulacion(Edad, Año_54, Año_55,sexo="M",año=54),
           Año_56 = func_simulacion(Edad, Año_55, Año_56,sexo="M",año=55),
           Año_57 = func_simulacion(Edad, Año_56, Año_57,sexo="M",año=56),
           Año_58 = func_simulacion(Edad, Año_57, Año_58,sexo="M",año=57),
           Año_59 = func_simulacion(Edad, Año_58, Año_59,sexo="M",año=58),
           Año_60 = func_simulacion(Edad, Año_59, Año_60,sexo="M",año=59),
           Año_61 = func_simulacion(Edad, Año_60, Año_61,sexo="M",año=60),
           Año_62 = func_simulacion(Edad, Año_61, Año_62,sexo="M",año=61),
           Año_63 = func_simulacion(Edad, Año_62, Año_63,sexo="M",año=62),
           Año_64 = func_simulacion(Edad, Año_63, Año_64,sexo="M",año=63),
           Año_65 = func_simulacion(Edad, Año_64, Año_65,sexo="M",año=64),
           Año_66 = func_simulacion(Edad, Año_65, Año_66,sexo="M",año=65),
           Año_67 = func_simulacion(Edad, Año_66, Año_67,sexo="M",año=66),
           Año_68 = func_simulacion(Edad, Año_67, Año_68,sexo="M",año=67),
           Año_69 = func_simulacion(Edad, Año_68, Año_69,sexo="M",año=68),
           Año_70 = func_simulacion(Edad, Año_69, Año_70,sexo="M",año=69),
           Año_71 = func_simulacion(Edad, Año_70, Año_71,sexo="M",año=70),
           Año_72 = func_simulacion(Edad, Año_71, Año_72,sexo="M",año=71),
           Año_73 = func_simulacion(Edad, Año_72, Año_73,sexo="M",año=72),
           Año_74 = func_simulacion(Edad, Año_73, Año_74,sexo="M",año=73),
           Año_75 = func_simulacion(Edad, Año_74, Año_75,sexo="M",año=74),
           Año_76 = func_simulacion(Edad, Año_75, Año_76,sexo="M",año=75),
           Año_77 = func_simulacion(Edad, Año_76, Año_77,sexo="M",año=76),
           Año_78 = func_simulacion(Edad, Año_77, Año_78,sexo="M",año=77),
           Año_79 = func_simulacion(Edad, Año_78, Año_79,sexo="M",año=78),
           Año_80 = func_simulacion(Edad, Año_79, Año_80,sexo="M",año=79),
    )
}

##--- Poblaciones --------------------------------------------------------------

###--- Encontrar las poblaciones -----------------------------------------------
encontrar_poblacion_tot_simulacion <- function(año, estado, sexo){
  if(sexo == "H"){
    df_simulacion <- vector_simulacion_H
  }
  if(sexo == "M"){
    df_simulacion <- vector_simulacion_M
  }
  v_pob <- c()
  for(i in 1:100){
    v_pob <- c(v_pob, sum(df_simulacion[[i]][,año+2] == estado))
  }
  return(v_pob)
}

encontrar_poblacion_65mas_simulacion <- function(año, estado, sexo){
  if(sexo == "H"){
    df_simulacion <- vector_simulacion_H
  }
  if(sexo == "M"){
    df_simulacion <- vector_simulacion_M
  }
  v_pob <- c()
  for(i in 1:100){
    v_pob <- c(v_pob, sum((df_simulacion[[i]][,año+2] == estado) & (año+df_simulacion[[i]][,1]) >= 65))
  }
  return(v_pob)
}

encontrar_poblacion_64menos_simulacion <- function(año, estado, sexo){
  if(sexo == "H"){
    df_simulacion <- vector_simulacion_H
  }
  if(sexo == "M"){
    df_simulacion <- vector_simulacion_M
  }
  v_pob <- c()
  for(i in 1:100){
    v_pob <- c(v_pob, sum((df_simulacion[[i]][,año+2] == estado) & (año+df_simulacion[[i]][,1]) <= 64))
  }
  return(v_pob)
}

#Poblacion Hombres
v_pob_xaño_0_H <- vector("list", length = 81)
v_pob_xaño_1_H <- vector("list", length = 81)
v_pob_xaño_2_H <- vector("list", length = 81)
v_pob_xaño_3_H <- vector("list", length = 81)
v_pob_xaño_4_H <- vector("list", length = 81)
v_pob_xaño_5_H <- vector("list", length = 81)
for (i in 1:81) {
  v_pob_xaño_0_H[[i]] <- encontrar_poblacion_tot_simulacion(año = i-1, estado = 0, sexo = "H")
  v_pob_xaño_1_H[[i]] <- encontrar_poblacion_tot_simulacion(año = i-1, estado = 1, sexo = "H")
  v_pob_xaño_2_H[[i]] <- encontrar_poblacion_tot_simulacion(año = i-1, estado = 2, sexo = "H")
  v_pob_xaño_3_H[[i]] <- encontrar_poblacion_tot_simulacion(año = i-1, estado = 3, sexo = "H")
  v_pob_xaño_4_H[[i]] <- encontrar_poblacion_tot_simulacion(año = i-1, estado = 4, sexo = "H")
  v_pob_xaño_5_H[[i]] <- encontrar_poblacion_tot_simulacion(año = i-1, estado = 5, sexo = "H")
}

df_poblaciones_simuladas_H <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Pob_estim_0 = mean(v_pob_xaño_0_H[[Año+1]]),
    Perc_99.5_0 = quantile(v_pob_xaño_0_H[[Año+1]],0.995),
    Pob_estim_1 = mean(v_pob_xaño_1_H[[Año+1]]),
    Perc_99.5_1 = quantile(v_pob_xaño_1_H[[Año+1]],0.995),
    Pob_estim_2 = mean(v_pob_xaño_2_H[[Año+1]]),
    Perc_99.5_2 = quantile(v_pob_xaño_2_H[[Año+1]],0.995),
    Pob_estim_3 = mean(v_pob_xaño_3_H[[Año+1]]),
    Perc_99.5_3 = quantile(v_pob_xaño_3_H[[Año+1]],0.995),
    Pob_estim_4 = mean(v_pob_xaño_4_H[[Año+1]]),
    Perc_99.5_4 = quantile(v_pob_xaño_4_H[[Año+1]],0.995),
    Pob_estim_5 = mean(v_pob_xaño_5_H[[Año+1]]),
    Perc_99.5_5 = quantile(v_pob_xaño_5_H[[Año+1]],0.995)
  )


#Poblacion Mujeres
v_pob_xaño_0_M <- vector("list", length = 81)
v_pob_xaño_1_M <- vector("list", length = 81)
v_pob_xaño_2_M <- vector("list", length = 81)
v_pob_xaño_3_M <- vector("list", length = 81)
v_pob_xaño_4_M <- vector("list", length = 81)
v_pob_xaño_5_M <- vector("list", length = 81)
for (i in 1:81) {
  v_pob_xaño_0_M[[i]] <- encontrar_poblacion_tot_simulacion(año = i-1, estado = 0, sexo = "M")
  v_pob_xaño_1_M[[i]] <- encontrar_poblacion_tot_simulacion(año = i-1, estado = 1, sexo = "M")
  v_pob_xaño_2_M[[i]] <- encontrar_poblacion_tot_simulacion(año = i-1, estado = 2, sexo = "M")
  v_pob_xaño_3_M[[i]] <- encontrar_poblacion_tot_simulacion(año = i-1, estado = 3, sexo = "M")
  v_pob_xaño_4_M[[i]] <- encontrar_poblacion_tot_simulacion(año = i-1, estado = 4, sexo = "M")
  v_pob_xaño_5_M[[i]] <- encontrar_poblacion_tot_simulacion(año = i-1, estado = 5, sexo = "M")
}

df_poblaciones_simuladas_M <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Pob_estim_0 = mean(v_pob_xaño_0_M[[Año+1]]),
    Perc_99.5_0 = quantile(v_pob_xaño_0_M[[Año+1]],0.995),
    Pob_estim_1 = mean(v_pob_xaño_1_M[[Año+1]]),
    Perc_99.5_1 = quantile(v_pob_xaño_1_M[[Año+1]],0.995),
    Pob_estim_2 = mean(v_pob_xaño_2_M[[Año+1]]),
    Perc_99.5_2 = quantile(v_pob_xaño_2_M[[Año+1]],0.995),
    Pob_estim_3 = mean(v_pob_xaño_3_M[[Año+1]]),
    Perc_99.5_3 = quantile(v_pob_xaño_3_M[[Año+1]],0.995),
    Pob_estim_4 = mean(v_pob_xaño_4_M[[Año+1]]),
    Perc_99.5_4 = quantile(v_pob_xaño_4_M[[Año+1]],0.995),
    Pob_estim_5 = mean(v_pob_xaño_5_M[[Año+1]]),
    Perc_99.5_5 = quantile(v_pob_xaño_5_M[[Año+1]],0.995)
  )

proyeccion_estoc_H = ggplot() + 
  geom_line(data = df_poblaciones_simuladas_H, aes(x = Año, y = as.numeric(Pob_estim_0 ), color = "Able"), linetype = "solid", size = 1) +
  geom_line(data = df_poblaciones_simuladas_H, aes(x = Año, y = as.numeric(Pob_estim_1 ) , color = "Mild"), linetype = "solid", size = 1) +
  geom_line(data = df_poblaciones_simuladas_H, aes(x = Año, y = as.numeric(Pob_estim_2 ) , color = "Moderate"), linetype = "solid", size = 1) +
  geom_line(data = df_poblaciones_simuladas_H, aes(x = Año, y = as.numeric(Pob_estim_3 ) , color = "Severe"), linetype = "solid", size = 1) +
  geom_line(data = df_poblaciones_simuladas_H, aes(x = Año, y = as.numeric(Pob_estim_4 ) , color = "Profound"), linetype = "solid", size = 1) +
  geom_line(data = df_poblaciones_simuladas_H, aes(x = Año, y = as.numeric(Pob_estim_5 ) , color = "Dead"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Able" = "lightblue4", "Mild" = "maroon", "Moderate" = "darkblue", "Severe" = "purple", "Profound" = "pink", "Dead" ="red"), name = "Estado") +
  xlab('Año') +
  ylab('Personas simuladas (Hombres)') + cowplot::theme_cowplot()
print(proyeccion_estoc_H)

proyeccion_estoc_M = ggplot() + 
  geom_line(data = df_poblaciones_simuladas_M, aes(x = Año, y = as.numeric(Pob_estim_0 ), color = "Able"), linetype = "solid", size = 1) +
  geom_line(data = df_poblaciones_simuladas_M, aes(x = Año, y = as.numeric(Pob_estim_1 ) , color = "Mild"), linetype = "solid", size = 1) +
  geom_line(data = df_poblaciones_simuladas_M, aes(x = Año, y = as.numeric(Pob_estim_2 ) , color = "Moderate"), linetype = "solid", size = 1) +
  geom_line(data = df_poblaciones_simuladas_M, aes(x = Año, y = as.numeric(Pob_estim_3 ) , color = "Severe"), linetype = "solid", size = 1) +
  geom_line(data = df_poblaciones_simuladas_M, aes(x = Año, y = as.numeric(Pob_estim_4 ) , color = "Profound"), linetype = "solid", size = 1) +
  geom_line(data = df_poblaciones_simuladas_M, aes(x = Año, y = as.numeric(Pob_estim_5 ) , color = "Dead"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Able" = "lightblue4", "Mild" = "maroon", "Moderate" = "darkblue", "Severe" = "purple", "Profound" = "pink", "Dead" ="red"), name = "Estado") +
  xlab('Año') +
  ylab('Personas simuladas (Mujeres)') + cowplot::theme_cowplot()
print(proyeccion_estoc_M)

#--- Modelo Deterministico montos esperados de ingresos y egresos para cada uno estado -----

#Proyección por generacion

Pob_H <-data.frame(tiempo = (0:81), Estado.0_65 = rep(0, 82), Estado.0.65 = rep(0, 82), Estado.1_65 = rep(0, 82), Estado.1.65 = rep(0, 82),
                   Estado.2_65 = rep(0, 82), Estado.2.65 = rep(0, 82),  Estado.3_65 = rep(0, 82), Estado.3.65 = rep(0, 82), Estado.4_65 = rep(0, 82), Estado.4.65 = rep(0, 82), Estado.5 = rep(0, 82))
Pob_M <-data.frame(tiempo = (0:81), Estado.0_65 = rep(0, 82), Estado.0.65 = rep(0, 82), Estado.1_65 = rep(0, 82), Estado.1.65 = rep(0, 82),
                   Estado.2_65 = rep(0, 82), Estado.2.65 = rep(0, 82), Estado.3_65 = rep(0, 82), Estado.3.65 = rep(0, 82), Estado.4_65 = rep(0, 82), Estado.4.65 = rep(0, 82), Estado.5 = rep(0, 82))

for(k in 0:34){
  proyeccion <- tabla_proyeccion_80años(30+k, 0, "H")
  Pob_H$Estado.0_65[1:(35-k)] <- Pob_H$Estado.0_65[1:(35-k)] + proyeccion[1:(35-k),2]
  Pob_H$Estado.0.65[(36-k):82] <- Pob_H$Estado.0.65[(36-k):82] + proyeccion[(36-k):82,2]
  Pob_H$Estado.1_65[1:(35-k)] <- Pob_H$Estado.1_65[1:(35-k)] + proyeccion[1:(35-k),3]
  Pob_H$Estado.1.65[(36-k):82] <- Pob_H$Estado.1.65[(36-k):82] + proyeccion[(36-k):82,3]
  Pob_H$Estado.2_65[1:(35-k)] <- Pob_H$Estado.2_65[1:(35-k)] + proyeccion[1:(35-k),4]
  Pob_H$Estado.3.65[(36-k):82] <- Pob_H$Estado.3.65[(36-k):82] + proyeccion[(36-k):82,5]
  Pob_H$Estado.4_65[1:(35-k)] <- Pob_H$Estado.4_65[1:(35-k)] + proyeccion[1:(35-k),6]
  Pob_H$Estado.4.65[(36-k):82] <- Pob_H$Estado.4.65[(36-k):82] + proyeccion[(36-k):82,6]
  Pob_H$Estado.5 <- Pob_H$Estado.5 + proyeccion[,7]
  
  
  proyeccion <- tabla_proyeccion_80años(30+k, 0, "M")
  Pob_M$Estado.0_65[1:(35-k)] <- Pob_M$Estado.0_65[1:(35-k)] + proyeccion[1:(35-k),2]
  Pob_M$Estado.0.65[(36-k):82] <- Pob_M$Estado.0.65[(36-k):82] + proyeccion[(36-k):82,2]
  Pob_M$Estado.1_65[1:(35-k)] <- Pob_M$Estado.1_65[1:(35-k)] + proyeccion[1:(35-k),3]
  Pob_M$Estado.1.65[(36-k):82] <- Pob_M$Estado.1.65[(36-k):82] + proyeccion[(36-k):82,3]
  Pob_M$Estado.2_65[1:(35-k)] <- Pob_M$Estado.2_65[1:(35-k)] + proyeccion[1:(35-k),4]
  Pob_M$Estado.2.65[(36-k):82] <- Pob_M$Estado.2.65[(36-k):82] + proyeccion[(36-k):82,4]
  Pob_M$Estado.3_65[1:(35-k)] <- Pob_M$Estado.3_65[1:(35-k)] + proyeccion[1:(35-k),5]
  Pob_M$Estado.3.65[(36-k):82] <- Pob_M$Estado.3.65[(36-k):82] + proyeccion[(36-k):82,5]
  Pob_M$Estado.4_65[1:(35-k)] <- Pob_M$Estado.4_65[1:(35-k)] + proyeccion[1:(35-k),6]
  Pob_M$Estado.4.65[(36-k):82] <- Pob_M$Estado.4.65[(36-k):82] + proyeccion[(36-k):82,6]
  Pob_M$Estado.5 <- Pob_M$Estado.5 + proyeccion[,7]
  
}


#Ingresos

Ingresos_H <- data.frame( tiempo = tabla_proyeccionesH_total$Año, Ingresos.E0 = rep(0, 82), Ingresos.E1 = rep(0, 82), Ingresos.E2 = rep(0, 82), Ingresos_totales = rep(0, 82))
Ingresos_M <- data.frame( tiempo = tabla_proyeccionesM_total$Año, Ingresos.E0 = rep(0, 82), Ingresos.E1 = rep(0, 82),  Ingresos.E2 = rep(0, 82), Ingresos_totales = rep(0, 82))

for (i in 1:82) {
  Ingresos_H[i,2] <- Ingresos_H[i,2] + prima_hombres_anual* (((1+inflacion)/(1+descuento))^(i-1))*Pob_H$Estado.0_65[i]
  Ingresos_H[i,3] <- Ingresos_H[i,3] + prima_hombres_anual* (((1+inflacion)/(1+descuento))^(i-1))*Pob_H$Estado.1_65[i]
  Ingresos_H[i,4] <- Ingresos_H[i,4] + prima_hombres_anual* (((1+inflacion)/(1+descuento))^(i-1))*Pob_H$Estado.2_65[i]
  
  Ingresos_M[i,2] <- Ingresos_M[i,2] + prima_mujeres_anual* (((1+inflacion)/(1+descuento))^(i-1))*Pob_M$Estado.0_65[i]
  Ingresos_M[i,3] <- Ingresos_M[i,3] + prima_mujeres_anual* (((1+inflacion)/(1+descuento))^(i-1))*Pob_M$Estado.1_65[i]
  Ingresos_M[i,4] <- Ingresos_M[i,4] + prima_mujeres_anual* (((1+inflacion)/(1+descuento))^(i-1))*Pob_M$Estado.2_65[i]
}

Ingresos_H$Ingresos_totales <- rowSums(Ingresos_H[, 2:4, drop = FALSE], na.rm = TRUE, dims = 1)
Ingresos_M$Ingresos_totales <- rowSums(Ingresos_M[, 2:4, drop = FALSE], na.rm = TRUE, dims = 1)


G.ingresos_H = ggplot() + 
  geom_line(data = Ingresos_H, aes(x = tiempo, y = Ingresos.E0, color = "Able"), linetype = "solid", size = 1) +
  geom_line(data = Ingresos_H, aes(x = tiempo, y = Ingresos.E1 , color = "Mild"), linetype = "solid", size = 1) +
  geom_line(data = Ingresos_H, aes(x = tiempo, y = Ingresos.E2 , color = "Moderate"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Able" = "lightblue4", "Mild" = "maroon", "Moderate" = "darkblue"), name = "Estado") +
  xlab('Tiempo') +
  ylab('Ingresos Esperados') + cowplot::theme_cowplot()


print(G.ingresos_H)

G.ingresos_M = ggplot() + 
  geom_line(data = Ingresos_M, aes(x = tiempo, y = Ingresos.E0, color = "Able"), linetype = "solid", size = 1) +
  geom_line(data = Ingresos_M, aes(x = tiempo, y = Ingresos.E1 , color = "Mild"), linetype = "solid", size = 1) +
  geom_line(data = Ingresos_M, aes(x = tiempo, y = Ingresos.E2 , color = "Moderate"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Able" = "lightblue4", "Mild" = "maroon", "Moderate" = "darkblue"), name = "Estado") +
  xlab('Tiempo') +
  ylab('Ingresos Esperados') + cowplot::theme_cowplot()


print(G.ingresos_M)


#Egresos

Egresos_H <- data.frame( tiempo = tabla_proyeccionesH_total$Año, Egresos.E0 = rep(0, 82), Egresos.E1 = rep(0, 82), Egresos.E2 = rep(0, 82), Egresos.E3 = rep(0, 82), Egresos.E4 = rep(0, 82), Egresos.E5 = rep(0, 82))
Egresos_M <- data.frame( tiempo = tabla_proyeccionesM_total$Año, Egresos.E0 = rep(0, 82), Egresos.E1 = rep(0, 82), Egresos.E2 = rep(0, 82), Egresos.E3 = rep(0, 82), Egresos.E4 = rep(0, 82), Egresos.E5 = rep(0, 82))

for (i in 1:82) {
  if(i == 1){
    Egresos_H[i,2] <- Egresos_H[i,2] + 0.2*prima_hombres_anual*Pob_H$Estado.0_65[i]
    Egresos_M[i,2] <- Egresos_M[i,2] + 0.2*prima_mujeres_anual*Pob_M$Estado.0_65[i]
    
  } else{
    Egresos_H[i,2] <- Egresos_H[i,2] + (0.05*prima_hombres_anual)*(((1+inflacion)/(1+descuento))^(i-1))*Pob_H$Estado.0_65[i]
    Egresos_H[i,3] <- Egresos_H[i,3] + (0.05*prima_hombres_anual)*(((1+inflacion)/(1+descuento))^(i-1))*Pob_H$Estado.1_65[i] + A*(((1+inflacion)/(1+descuento))^(i))*Pob_H$Estado.1.65[i]
    Egresos_H[i,4] <- Egresos_H[i,4] + (0.05*prima_hombres_anual)*(((1+inflacion)/(1+descuento))^(i-1))*Pob_H$Estado.2_65[i] + B*(((1+inflacion)/(1+descuento))^(i))*Pob_H$Estado.2.65[i]
    Egresos_H[i,5] <- Egresos_H[i,5] + C*(((1+inflacion)/(1+descuento))^(i))*Pob_H$Estado.3.65[i]
    Egresos_H[i,6] <- Egresos_H[i,6] + D*(((1+inflacion)/(1+descuento))^(i))*Pob_H$Estado.4.65[i]
    
    Egresos_M[i,2] <- Egresos_M[i,2] + (0.05*prima_mujeres_anual)*(((1+inflacion)/(1+descuento))^(i-1))*Pob_M$Estado.0_65[i]
    Egresos_M[i,3] <- Egresos_M[i,3] + (0.05*prima_mujeres_anual)*(((1+inflacion)/(1+descuento))^(i-1))*Pob_M$Estado.1_65[i] + A*(((1+inflacion)/(1+descuento))^(i))*Pob_M$Estado.1.65[i]
    Egresos_M[i,4] <- Egresos_M[i,4] + (0.05*prima_mujeres_anual)*(((1+inflacion)/(1+descuento))^(i-1))*Pob_M$Estado.2_65[i] + B*(((1+inflacion)/(1+descuento))^(i))*Pob_M$Estado.2.65[i]
    Egresos_M[i,5] <- Egresos_M[i,5] + C*(((1+inflacion)/(1+descuento))^(i))*Pob_M$Estado.3.65[i]
    Egresos_M[i,6] <- Egresos_M[i,6] + D*(((1+inflacion)/(1+descuento))^(i))*Pob_M$Estado.4.65[i]
  }
}



G.egresos_H = ggplot() + 
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E0, color = "Able"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E1 , color = "Mild"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E2 , color = "Moderate"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E3 , color = "Severe"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E4 , color = "Profound"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Able" = "lightblue4", "Mild" = "maroon", "Moderate" = "darkblue", "Severe" = "purple", "Profound" = "pink"), name = "Estado") +
  xlab('Tiempo') +
  ylab('Egresos Esperados') + cowplot::theme_cowplot()

print(G.egresos_H)


G.egresos_M = ggplot() + 
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E0, color = "Able"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E1 , color = "Mild"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E2 , color = "Moderate"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E3 , color = "Severe"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E4 , color = "Profound"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Able" = "lightblue4", "Mild" = "maroon", "Moderate" = "darkblue", "Severe" = "purple", "Profound" = "pink"), name = "Estado") +
  xlab('Tiempo') +
  ylab('Egresos Esperados') + cowplot::theme_cowplot()

print(G.egresos_M)




#--- Modelo Estocastico montos esperados de ingresos y egresos para cada uno estado -----

##--- Poblaciones --------------------------------------------------------------

#Poblacion Hombres edades 65+ y 64-
v_pob_65mas_xaño_0_H <- vector("list", length = 81)
v_pob_65mas_xaño_1_H <- vector("list", length = 81)
v_pob_65mas_xaño_2_H <- vector("list", length = 81)
v_pob_65mas_xaño_3_H <- vector("list", length = 81)
v_pob_65mas_xaño_4_H <- vector("list", length = 81)
v_pob_65mas_xaño_5_H <- vector("list", length = 81)
for (i in 1:81) {
  v_pob_65mas_xaño_0_H[[i]] <- encontrar_poblacion_65mas_simulacion(año = i-1, estado = 0, sexo = "H")
  v_pob_65mas_xaño_1_H[[i]] <- encontrar_poblacion_65mas_simulacion(año = i-1, estado = 1, sexo = "H")
  v_pob_65mas_xaño_2_H[[i]] <- encontrar_poblacion_65mas_simulacion(año = i-1, estado = 2, sexo = "H")
  v_pob_65mas_xaño_3_H[[i]] <- encontrar_poblacion_65mas_simulacion(año = i-1, estado = 3, sexo = "H")
  v_pob_65mas_xaño_4_H[[i]] <- encontrar_poblacion_65mas_simulacion(año = i-1, estado = 4, sexo = "H")
  v_pob_65mas_xaño_5_H[[i]] <- encontrar_poblacion_65mas_simulacion(año = i-1, estado = 5, sexo = "H")
}

v_pob_64menos_xaño_0_H <- vector("list", length = 81)
v_pob_64menos_xaño_1_H <- vector("list", length = 81)
v_pob_64menos_xaño_2_H <- vector("list", length = 81)
v_pob_64menos_xaño_3_H <- vector("list", length = 81)
v_pob_64menos_xaño_4_H <- vector("list", length = 81)
v_pob_64menos_xaño_5_H <- vector("list", length = 81)
for (i in 1:81) {
  v_pob_64menos_xaño_0_H[[i]] <- encontrar_poblacion_64menos_simulacion(año = i-1, estado = 0, sexo = "H")
  v_pob_64menos_xaño_1_H[[i]] <- encontrar_poblacion_64menos_simulacion(año = i-1, estado = 1, sexo = "H")
  v_pob_64menos_xaño_2_H[[i]] <- encontrar_poblacion_64menos_simulacion(año = i-1, estado = 2, sexo = "H")
  v_pob_64menos_xaño_3_H[[i]] <- encontrar_poblacion_64menos_simulacion(año = i-1, estado = 3, sexo = "H")
  v_pob_64menos_xaño_4_H[[i]] <- encontrar_poblacion_64menos_simulacion(año = i-1, estado = 4, sexo = "H")
  v_pob_64menos_xaño_5_H[[i]] <- encontrar_poblacion_64menos_simulacion(año = i-1, estado = 5, sexo = "H")
}

#Poblacion Mujeres edades 65+ y 64-
v_pob_65mas_xaño_0_M <- vector("list", length = 81)
v_pob_65mas_xaño_1_M <- vector("list", length = 81)
v_pob_65mas_xaño_2_M <- vector("list", length = 81)
v_pob_65mas_xaño_3_M <- vector("list", length = 81)
v_pob_65mas_xaño_4_M <- vector("list", length = 81)
v_pob_65mas_xaño_5_M <- vector("list", length = 81)
for (i in 1:81) {
  v_pob_65mas_xaño_0_M[[i]] <- encontrar_poblacion_65mas_simulacion(año = i-1, estado = 0, sexo = "M")
  v_pob_65mas_xaño_1_M[[i]] <- encontrar_poblacion_65mas_simulacion(año = i-1, estado = 1, sexo = "M")
  v_pob_65mas_xaño_2_M[[i]] <- encontrar_poblacion_65mas_simulacion(año = i-1, estado = 2, sexo = "M")
  v_pob_65mas_xaño_3_M[[i]] <- encontrar_poblacion_65mas_simulacion(año = i-1, estado = 3, sexo = "M")
  v_pob_65mas_xaño_4_M[[i]] <- encontrar_poblacion_65mas_simulacion(año = i-1, estado = 4, sexo = "M")
  v_pob_65mas_xaño_5_M[[i]] <- encontrar_poblacion_65mas_simulacion(año = i-1, estado = 5, sexo = "M")
}

v_pob_64menos_xaño_0_M <- vector("list", length = 81)
v_pob_64menos_xaño_1_M <- vector("list", length = 81)
v_pob_64menos_xaño_2_M <- vector("list", length = 81)
v_pob_64menos_xaño_3_M <- vector("list", length = 81)
v_pob_64menos_xaño_4_M <- vector("list", length = 81)
v_pob_64menos_xaño_5_M <- vector("list", length = 81)
for (i in 1:81) {
  v_pob_64menos_xaño_0_M[[i]] <- encontrar_poblacion_64menos_simulacion(año = i-1, estado = 0, sexo = "M")
  v_pob_64menos_xaño_1_M[[i]] <- encontrar_poblacion_64menos_simulacion(año = i-1, estado = 1, sexo = "M")
  v_pob_64menos_xaño_2_M[[i]] <- encontrar_poblacion_64menos_simulacion(año = i-1, estado = 2, sexo = "M")
  v_pob_64menos_xaño_3_M[[i]] <- encontrar_poblacion_64menos_simulacion(año = i-1, estado = 3, sexo = "M")
  v_pob_64menos_xaño_4_M[[i]] <- encontrar_poblacion_64menos_simulacion(año = i-1, estado = 4, sexo = "M")
  v_pob_64menos_xaño_5_M[[i]] <- encontrar_poblacion_64menos_simulacion(año = i-1, estado = 5, sexo = "M")
}

##--- Prima diferida por sexo --------------------------------------------------

###--- Ingresos -----------------------------------------------------------------

#Hombres
df_ingresos_simulados_H <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Ing_estim_0 = prima_hombres_anual*mean(v_pob_64menos_xaño_0_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_0 = prima_hombres_anual*quantile(v_pob_64menos_xaño_0_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Ing_estim_1 = prima_hombres_anual*mean(v_pob_64menos_xaño_1_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_1 = prima_hombres_anual*quantile(v_pob_64menos_xaño_1_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Ing_estim_2 = prima_hombres_anual*mean(v_pob_64menos_xaño_2_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_2 = prima_hombres_anual*quantile(v_pob_64menos_xaño_2_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año)
  )

G.ingresos_simulados_H = ggplot() + 
  geom_line(data = df_ingresos_simulados_H, aes(x = Año, y = Ing_estim_0 , color = "Capaz"), linetype = "solid", size = 1) +
  geom_line(data = df_ingresos_simulados_H, aes(x = Año, y = Ing_estim_1 , color = "Leve"), linetype = "solid", size = 1) +
  geom_line(data = df_ingresos_simulados_H, aes(x = Año, y = Ing_estim_2 , color = "Moderado"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue"), name = "Estado") +
  labs(title = "Ingresos Simulados para Hombres \ncon Prima por Sexo", x = "Tiempo", y = "Ingresos") +
  xlim(0, 35) +
  cowplot::theme_cowplot()
print(G.ingresos_simulados_H)

#Mujeres
df_ingresos_simulados_M <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Ing_estim_0 = prima_mujeres_anual*mean(v_pob_64menos_xaño_0_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_0 = prima_mujeres_anual*quantile(v_pob_64menos_xaño_0_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Ing_estim_1 = prima_mujeres_anual*mean(v_pob_64menos_xaño_1_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_1 = prima_mujeres_anual*quantile(v_pob_64menos_xaño_1_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Ing_estim_2 = prima_mujeres_anual*mean(v_pob_64menos_xaño_2_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_2 = prima_mujeres_anual*quantile(v_pob_64menos_xaño_2_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año)
  )

G.ingresos_simulados_M = ggplot() + 
  geom_line(data = df_ingresos_simulados_M, aes(x = Año, y = Ing_estim_0 , color = "Capaz"), linetype = "solid", size = 1) +
  geom_line(data = df_ingresos_simulados_M, aes(x = Año, y = Ing_estim_1 , color = "Leve"), linetype = "solid", size = 1) +
  geom_line(data = df_ingresos_simulados_M, aes(x = Año, y = Ing_estim_2 , color = "Moderado"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue"), name = "Estado") +
  labs(title = "Ingresos Simulados para Mujeres \ncon Prima por Sexo", x = "Tiempo", y = "Ingresos") +
  xlim(0, 35) +
  cowplot::theme_cowplot()
print(G.ingresos_simulados_M)


###--- Egresos ---------------------------------------------------------------

#Hombres
df_beneficios_simulados_H <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Benef_estim_1 = A*mean(v_pob_65mas_xaño_1_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_1 = A*quantile(v_pob_65mas_xaño_1_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1),
    Benef_estim_2 = B*mean(v_pob_65mas_xaño_2_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_2 = B*quantile(v_pob_65mas_xaño_2_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1),
    Benef_estim_3 = C*mean(v_pob_65mas_xaño_3_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_3 = C*quantile(v_pob_65mas_xaño_3_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1),
    Benef_estim_4 = D*mean(v_pob_65mas_xaño_4_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_4 = D*quantile(v_pob_65mas_xaño_4_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1)
  )

df_costos_simulados_H <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Costo_estim_0 = 0.05*prima_hombres_anual*mean(v_pob_64menos_xaño_0_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_0 = 0.05*prima_hombres_anual*quantile(v_pob_64menos_xaño_0_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Costo_estim_1 = 0.05*prima_hombres_anual*mean(v_pob_64menos_xaño_1_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_1 = 0.05*prima_hombres_anual*quantile(v_pob_64menos_xaño_1_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Costo_estim_2 = 0.05*prima_hombres_anual*mean(v_pob_64menos_xaño_2_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_2 = 0.05*prima_hombres_anual*quantile(v_pob_64menos_xaño_2_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año)
  )
df_costos_simulados_H[1,2:3] <- 0.2*prima_anual*mean(v_pob_64menos_xaño_0_H[[1]])

df_egresos_simulado_H <- tibble(
  Año = 0:80) %>%
  mutate(
    Egresos_estim_0 = df_costos_simulados_H$Costo_estim_0,
    Perc_99.5_0 = df_costos_simulados_H$Perc_99.5_0,
    Egresos_estim_1 = df_beneficios_simulados_H$Benef_estim_1 + df_costos_simulados_H$Costo_estim_1,
    Perc_99.5_1 = df_beneficios_simulados_H$Perc_99.5_1 + df_costos_simulados_H$Perc_99.5_1,
    Egresos_estim_2 = df_beneficios_simulados_H$Benef_estim_2 + df_costos_simulados_H$Costo_estim_2,
    Perc_99.5_2 = df_beneficios_simulados_H$Perc_99.5_2 + df_costos_simulados_H$Perc_99.5_2,
    Egresos_estim_3 = df_beneficios_simulados_H$Benef_estim_3,
    Perc_99.5_3 = df_beneficios_simulados_H$Perc_99.5_3,
    Egresos_estim_4 = df_beneficios_simulados_H$Benef_estim_4,
    Perc_99.5_4 = df_beneficios_simulados_H$Perc_99.5_4
  )

G.egresos_simulados_H = ggplot() + 
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_0, color = "Capaz"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_1 , color = "Leve"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_2 , color = "Moderado"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_3 , color = "Severo"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_4 , color = "Profundo"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue", "Severo" = "purple", "Profundo" = "pink"), name = "Estado") +
  labs(title = "Egresos Simulados para Hombres \ncon Prima por Sexo", x = "Tiempo", y = "Egresos") +
  cowplot::theme_cowplot()
print(G.egresos_simulados_H)


#Mujeres
df_beneficios_simulados_M <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Benef_estim_1 = A*mean(v_pob_65mas_xaño_1_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_1 = A*quantile(v_pob_65mas_xaño_1_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1),
    Benef_estim_2 = B*mean(v_pob_65mas_xaño_2_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_2 = B*quantile(v_pob_65mas_xaño_2_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1),
    Benef_estim_3 = C*mean(v_pob_65mas_xaño_3_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_3 = C*quantile(v_pob_65mas_xaño_3_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1),
    Benef_estim_4 = D*mean(v_pob_65mas_xaño_4_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_4 = D*quantile(v_pob_65mas_xaño_4_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1)
  )

df_costos_simulados_M <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Costo_estim_0 = 0.05*prima_mujeres_anual*mean(v_pob_64menos_xaño_0_M[[Año+1]]*((1+inflacion)/(1+descuento))^(Año)),
    Perc_99.5_0 = 0.05*prima_mujeres_anual*quantile(v_pob_64menos_xaño_0_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Costo_estim_1 = 0.05*prima_mujeres_anual*mean(v_pob_64menos_xaño_1_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_1 = 0.05*prima_mujeres_anual*quantile(v_pob_64menos_xaño_1_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Costo_estim_2 = 0.05*prima_mujeres_anual*mean(v_pob_64menos_xaño_2_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_2 = 0.05*prima_mujeres_anual*quantile(v_pob_64menos_xaño_2_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año)
  )
df_costos_simulados_M[1,2:3] <- 0.2*prima_anual*mean(v_pob_64menos_xaño_0_M[[1]])

df_egresos_simulado_M <- tibble(
  Año = 0:80) %>%
  mutate(
    Egresos_estim_0 = df_costos_simulados_M$Costo_estim_0,
    Perc_99.5_0 = df_costos_simulados_M$Perc_99.5_0,
    Egresos_estim_1 = df_beneficios_simulados_M$Benef_estim_1 + df_costos_simulados_M$Costo_estim_1,
    Perc_99.5_1 = df_beneficios_simulados_M$Perc_99.5_1 + df_costos_simulados_M$Perc_99.5_1,
    Egresos_estim_2 = df_beneficios_simulados_M$Benef_estim_2 + df_costos_simulados_M$Costo_estim_2,
    Perc_99.5_2 = df_beneficios_simulados_M$Perc_99.5_2 + df_costos_simulados_M$Perc_99.5_2,
    Egresos_estim_3 = df_beneficios_simulados_M$Benef_estim_3,
    Perc_99.5_3 = df_beneficios_simulados_M$Perc_99.5_3,
    Egresos_estim_4 = df_beneficios_simulados_M$Benef_estim_4,
    Perc_99.5_4 = df_beneficios_simulados_M$Perc_99.5_4
  )

G.egresos_simulados_M = ggplot() + 
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_0, color = "Capaz"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_1 , color = "Leve"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_2 , color = "Moderado"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_3 , color = "Severo"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_4 , color = "Profundo"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue", "Severo" = "purple", "Profundo" = "pink"), name = "Estado") +
  labs(title = "Egresos Simulados para Mujeres \ncon Prima por Sexo", x = "Tiempo", y = "Egresos") + 
  cowplot::theme_cowplot()
print(G.egresos_simulados_M)


###--- Balance ------------------------------------------------------------------

#Hombres
df_balance_simulado_H <- tibble(
  Año = 0:80) %>%
  mutate(
    Balance_estim_0 = df_ingresos_simulados_H$Ing_estim_0 - df_costos_simulados_H$Costo_estim_0,
    Perc_99.5_0 = df_ingresos_simulados_H$Perc_99.5_0 - df_costos_simulados_H$Perc_99.5_0,
    Balance_estim_1 = df_ingresos_simulados_H$Ing_estim_1 - df_beneficios_simulados_H$Benef_estim_1 - df_costos_simulados_H$Costo_estim_1,
    Perc_99.5_1 = df_ingresos_simulados_H$Perc_99.5_1 - df_beneficios_simulados_H$Perc_99.5_1 - df_costos_simulados_H$Perc_99.5_1,
    Balance_estim_2 = df_ingresos_simulados_H$Ing_estim_2 - df_beneficios_simulados_H$Benef_estim_2 - df_costos_simulados_H$Costo_estim_2,
    Perc_99.5_2 = df_ingresos_simulados_H$Perc_99.5_2 - df_beneficios_simulados_H$Perc_99.5_2 - df_costos_simulados_H$Perc_99.5_2,
    Balance_estim_3 = (-df_beneficios_simulados_H$Benef_estim_3),
    Perc_99.5_3 = (-df_beneficios_simulados_H$Perc_99.5_3),
    Balance_estim_4 = (-df_beneficios_simulados_H$Benef_estim_4),
    Perc_99.5_4 = (-df_beneficios_simulados_H$Perc_99.5_4)
  )

#Mujeres
df_balance_simulado_M <- tibble(
  Año = 0:80) %>%
  mutate(
    Balance_estim_0 = df_ingresos_simulados_M$Ing_estim_0 - df_costos_simulados_M$Costo_estim_0,
    Perc_99.5_0 = df_ingresos_simulados_M$Perc_99.5_0 - df_costos_simulados_M$Perc_99.5_0,
    Balance_estim_1 = df_ingresos_simulados_M$Ing_estim_1 - df_beneficios_simulados_M$Benef_estim_1 - df_costos_simulados_M$Costo_estim_1,
    Perc_99.5_1 = df_ingresos_simulados_M$Perc_99.5_1 - df_beneficios_simulados_M$Perc_99.5_1 - df_costos_simulados_M$Perc_99.5_1,
    Balance_estim_2 = df_ingresos_simulados_M$Ing_estim_2 - df_beneficios_simulados_M$Benef_estim_2 - df_costos_simulados_M$Costo_estim_2,
    Perc_99.5_2 = df_ingresos_simulados_M$Perc_99.5_2 - df_beneficios_simulados_M$Perc_99.5_2 - df_costos_simulados_M$Perc_99.5_2,
    Balance_estim_3 = (-df_beneficios_simulados_M$Benef_estim_3),
    Perc_99.5_3 = (-df_beneficios_simulados_M$Perc_99.5_3),
    Balance_estim_4 = (-df_beneficios_simulados_M$Benef_estim_4),
    Perc_99.5_4 = (-df_beneficios_simulados_M$Perc_99.5_4)
  )


##--- Prima unitaria --------------------------------------------------

###--- Ingresos -----------------------------------------------------------------

#Hombres
df_ingresos_simulados_unit_H <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Ing_estim_0 = prima_anual*mean(v_pob_64menos_xaño_0_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_0 = prima_anual*quantile(v_pob_64menos_xaño_0_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Ing_estim_1 = prima_anual*mean(v_pob_64menos_xaño_1_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_1 = prima_anual*quantile(v_pob_64menos_xaño_1_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Ing_estim_2 = prima_anual*mean(v_pob_64menos_xaño_2_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_2 = prima_anual*quantile(v_pob_64menos_xaño_2_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año)
  )

G.df_ingresos_simulados_unit_H = ggplot() + 
  geom_line(data = df_ingresos_simulados_unit_H, aes(x = Año, y = Ing_estim_0 , color = "Capaz"), linetype = "solid", size = 1) +
  geom_line(data = df_ingresos_simulados_unit_H, aes(x = Año, y = Ing_estim_1 , color = "Leve"), linetype = "solid", size = 1) +
  geom_line(data = df_ingresos_simulados_unit_H, aes(x = Año, y = Ing_estim_2 , color = "Moderado"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue"), name = "Estado") +
  labs(title = "Ingresos Simulados para Hombres \ncon Prima Indiferente", x = "Tiempo", y = "Ingresos") +
  xlim(0, 35) +
  cowplot::theme_cowplot()
print(G.df_ingresos_simulados_unit_H)

#Mujeres
df_ingresos_simulados_unit_M <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Ing_estim_0 = prima_anual*mean(v_pob_64menos_xaño_0_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_0 = prima_anual*quantile(v_pob_64menos_xaño_0_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Ing_estim_1 = prima_anual*mean(v_pob_64menos_xaño_1_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_1 = prima_anual*quantile(v_pob_64menos_xaño_1_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Ing_estim_2 = prima_anual*mean(v_pob_64menos_xaño_2_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_2 = prima_anual*quantile(v_pob_64menos_xaño_2_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año)
  )

G.ingresos_simulados_unit_M = ggplot() + 
  geom_line(data = df_ingresos_simulados_unit_M, aes(x = Año, y = Ing_estim_0 , color = "Capaz"), linetype = "solid", size = 1) +
  geom_line(data = df_ingresos_simulados_unit_M, aes(x = Año, y = Ing_estim_1 , color = "Leve"), linetype = "solid", size = 1) +
  geom_line(data = df_ingresos_simulados_unit_M, aes(x = Año, y = Ing_estim_2 , color = "Moderado"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue"), name = "Estado") +
  labs(title = "Ingresos Simulados para Mujeres \ncon Prima Indiferente", x = "Tiempo", y = "Ingresos") +
  xlim(0, 35) +
  cowplot::theme_cowplot()
print(G.ingresos_simulados_unit_M)


###--- Egresos ---------------------------------------------------------------

#Hombres
df_beneficios_simulados_unit_H <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Benef_estim_1 = A*mean(v_pob_65mas_xaño_1_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_1 = A*quantile(v_pob_65mas_xaño_1_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1),
    Benef_estim_2 = B*mean(v_pob_65mas_xaño_2_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_2 = B*quantile(v_pob_65mas_xaño_2_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1),
    Benef_estim_3 = C*mean(v_pob_65mas_xaño_3_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_3 = C*quantile(v_pob_65mas_xaño_3_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1),
    Benef_estim_4 = D*mean(v_pob_65mas_xaño_4_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_4 = D*quantile(v_pob_65mas_xaño_4_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1)
  )

df_costos_simulados_unit_H <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Costo_estim_0 = 0.05*prima_anual*mean(v_pob_64menos_xaño_0_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_0 = 0.05*prima_anual*quantile(v_pob_64menos_xaño_0_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Costo_estim_1 = 0.05*prima_anual*mean(v_pob_64menos_xaño_1_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_1 = 0.05*prima_anual*quantile(v_pob_64menos_xaño_1_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Costo_estim_2 = 0.05*prima_anual*mean(v_pob_64menos_xaño_2_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_2 = 0.05*prima_anual*quantile(v_pob_64menos_xaño_2_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año)
  )
df_costos_simulados_unit_H[1,2:3] <- 0.2*prima_anual*mean(v_pob_64menos_xaño_0_H[[1]])

df_egresos_simulado_unit_H <- tibble(
  Año = 0:80) %>%
  mutate(
    Egresos_estim_0 = df_costos_simulados_unit_H$Costo_estim_0,
    Perc_99.5_0 = df_costos_simulados_unit_H$Perc_99.5_0,
    Egresos_estim_1 = df_beneficios_simulados_unit_H$Benef_estim_1 + df_costos_simulados_unit_H$Costo_estim_1,
    Perc_99.5_1 = df_beneficios_simulados_unit_H$Perc_99.5_1 + df_costos_simulados_unit_H$Perc_99.5_1,
    Egresos_estim_2 = df_beneficios_simulados_unit_H$Benef_estim_2 + df_costos_simulados_unit_H$Costo_estim_2,
    Perc_99.5_2 = df_beneficios_simulados_unit_H$Perc_99.5_2 + df_costos_simulados_unit_H$Perc_99.5_2,
    Egresos_estim_3 = df_beneficios_simulados_unit_H$Benef_estim_3,
    Perc_99.5_3 = df_beneficios_simulados_unit_H$Perc_99.5_3,
    Egresos_estim_4 = df_beneficios_simulados_unit_H$Benef_estim_4,
    Perc_99.5_4 = df_beneficios_simulados_unit_H$Perc_99.5_4
  )

G.egresos_simulados_unit_H = ggplot() + 
  geom_line(data = df_egresos_simulado_unit_H, aes(x = Año, y = Egresos_estim_0, color = "Capaz"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_unit_H, aes(x = Año, y = Egresos_estim_1 , color = "Leve"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_unit_H, aes(x = Año, y = Egresos_estim_2 , color = "Moderado"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_unit_H, aes(x = Año, y = Egresos_estim_3 , color = "Severo"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_unit_H, aes(x = Año, y = Egresos_estim_4 , color = "Profundo"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue", "Severo" = "purple", "Profundo" = "pink"), name = "Estado") +
  labs(title = "Egresos Simulados para Hombres \ncon Prima Indiferente", x = "Tiempo", y = "Egresos") +
  cowplot::theme_cowplot()
print(G.egresos_simulados_unit_H)


#Mujeres
df_beneficios_simulados_unit_M <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Benef_estim_1 = A*mean(v_pob_65mas_xaño_1_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_1 = A*quantile(v_pob_65mas_xaño_1_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1),
    Benef_estim_2 = B*mean(v_pob_65mas_xaño_2_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_2 = B*quantile(v_pob_65mas_xaño_2_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1),
    Benef_estim_3 = C*mean(v_pob_65mas_xaño_3_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_3 = C*quantile(v_pob_65mas_xaño_3_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1),
    Benef_estim_4 = D*mean(v_pob_65mas_xaño_4_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año+1),
    Perc_99.5_4 = D*quantile(v_pob_65mas_xaño_4_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año+1)
  )

df_costos_simulados_unit_M <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Costo_estim_0 = 0.05*prima_anual*mean(v_pob_64menos_xaño_0_M[[Año+1]]*((1+inflacion)/(1+descuento))^(Año)),
    Perc_99.5_0 = 0.05*prima_anual*quantile(v_pob_64menos_xaño_0_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Costo_estim_1 = 0.05*prima_anual*mean(v_pob_64menos_xaño_1_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_1 = 0.05*prima_anual*quantile(v_pob_64menos_xaño_1_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Costo_estim_2 = 0.05*prima_anual*mean(v_pob_64menos_xaño_2_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_2 = 0.05*prima_anual*quantile(v_pob_64menos_xaño_2_M[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año)
  )
df_costos_simulados_unit_M[1,2:3] <- 0.2*prima_anual*mean(v_pob_64menos_xaño_0_M[[1]])

df_egresos_simulado_unit_M <- tibble(
  Año = 0:80) %>%
  mutate(
    Egresos_estim_0 = df_costos_simulados_unit_M$Costo_estim_0,
    Perc_99.5_0 = df_costos_simulados_unit_M$Perc_99.5_0,
    Egresos_estim_1 = df_beneficios_simulados_unit_M$Benef_estim_1 + df_costos_simulados_unit_M$Costo_estim_1,
    Perc_99.5_1 = df_beneficios_simulados_unit_M$Perc_99.5_1 + df_costos_simulados_unit_M$Perc_99.5_1,
    Egresos_estim_2 = df_beneficios_simulados_unit_M$Benef_estim_2 + df_costos_simulados_unit_M$Costo_estim_2,
    Perc_99.5_2 = df_beneficios_simulados_unit_M$Perc_99.5_2 + df_costos_simulados_unit_M$Perc_99.5_2,
    Egresos_estim_3 = df_beneficios_simulados_unit_M$Benef_estim_3,
    Perc_99.5_3 = df_beneficios_simulados_unit_M$Perc_99.5_3,
    Egresos_estim_4 = df_beneficios_simulados_unit_M$Benef_estim_4,
    Perc_99.5_4 = df_beneficios_simulados_unit_M$Perc_99.5_4
  )

G.egresos_simulados_unit_M = ggplot() + 
  geom_line(data = df_egresos_simulado_unit_M, aes(x = Año, y = Egresos_estim_0, color = "Capaz"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_unit_M, aes(x = Año, y = Egresos_estim_1 , color = "Leve"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_unit_M, aes(x = Año, y = Egresos_estim_2 , color = "Moderado"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_unit_M, aes(x = Año, y = Egresos_estim_3 , color = "Severo"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_unit_M, aes(x = Año, y = Egresos_estim_4 , color = "Profundo"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue", "Severo" = "purple", "Profundo" = "pink"), name = "Estado") +
  labs(title = "Egresos Simulados para Mujeres \ncon Prima Indiferente", x = "Tiempo", y = "Egresos") +
  cowplot::theme_cowplot()
print(G.egresos_simulados_unit_M)


###--- Balance ------------------------------------------------------------------

#Hombres
df_balance_simulado_unit_H <- tibble(
  Año = 0:80) %>%
  mutate(
    Balance_estim_0 = df_ingresos_simulados_unit_H$Ing_estim_0 - df_costos_simulados_unit_H$Costo_estim_0,
    Perc_99.5_0 = df_ingresos_simulados_unit_H$Perc_99.5_0 - df_costos_simulados_unit_H$Perc_99.5_0,
    Balance_estim_1 = df_ingresos_simulados_unit_H$Ing_estim_1 - df_beneficios_simulados_unit_H$Benef_estim_1 - df_costos_simulados_unit_H$Costo_estim_1,
    Perc_99.5_1 = df_ingresos_simulados_unit_H$Perc_99.5_1 - df_beneficios_simulados_unit_H$Perc_99.5_1 - df_costos_simulados_unit_H$Perc_99.5_1,
    Balance_estim_2 = df_ingresos_simulados_unit_H$Ing_estim_2 - df_beneficios_simulados_unit_H$Benef_estim_2 - df_costos_simulados_unit_H$Costo_estim_2,
    Perc_99.5_2 = df_ingresos_simulados_unit_H$Perc_99.5_2 - df_beneficios_simulados_unit_H$Perc_99.5_2 - df_costos_simulados_unit_H$Perc_99.5_2,
    Balance_estim_3 = (-df_beneficios_simulados_unit_H$Benef_estim_3),
    Perc_99.5_3 = (-df_beneficios_simulados_unit_H$Perc_99.5_3),
    Balance_estim_4 = (-df_beneficios_simulados_unit_H$Benef_estim_4),
    Perc_99.5_4 = (-df_beneficios_simulados_unit_H$Perc_99.5_4)
  )

#Mujeres
df_balance_simulado_unit_M <- tibble(
  Año = 0:80) %>%
  mutate(
    Balance_estim_0 = df_ingresos_simulados_unit_M$Ing_estim_0 - df_costos_simulados_unit_M$Costo_estim_0,
    Perc_99.5_0 = df_ingresos_simulados_unit_M$Perc_99.5_0 - df_costos_simulados_unit_M$Perc_99.5_0,
    Balance_estim_1 = df_ingresos_simulados_unit_M$Ing_estim_1 - df_beneficios_simulados_unit_M$Benef_estim_1 - df_costos_simulados_unit_M$Costo_estim_1,
    Perc_99.5_1 = df_ingresos_simulados_unit_M$Perc_99.5_1 - df_beneficios_simulados_unit_M$Perc_99.5_1 - df_costos_simulados_unit_M$Perc_99.5_1,
    Balance_estim_2 = df_ingresos_simulados_unit_M$Ing_estim_2 - df_beneficios_simulados_unit_M$Benef_estim_2 - df_costos_simulados_unit_M$Costo_estim_2,
    Perc_99.5_2 = df_ingresos_simulados_unit_M$Perc_99.5_2 - df_beneficios_simulados_unit_M$Perc_99.5_2 - df_costos_simulados_unit_M$Perc_99.5_2,
    Balance_estim_3 = (-df_beneficios_simulados_unit_M$Benef_estim_3),
    Perc_99.5_3 = (-df_beneficios_simulados_unit_M$Perc_99.5_3),
    Balance_estim_4 = (-df_beneficios_simulados_unit_M$Benef_estim_4),
    Perc_99.5_4 = (-df_beneficios_simulados_unit_M$Perc_99.5_4)
  )

##--- Graficos conjuntos ------------------------------------------------------- 

#Prima por sexo

ambos_ingresos_H = ggplot() + 
  geom_line(data = df_ingresos_simulados_H, aes(x = Año, y = Ing_estim_0 , color = "Capaz Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_ingresos_simulados_H, aes(x = Año, y = Ing_estim_1 , color = "Leve Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_ingresos_simulados_H, aes(x = Año, y = Ing_estim_2 , color = "Moderado Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Ingresos_H, aes(x = tiempo, y = Ingresos.E0, color = "Capaz"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Ingresos_H, aes(x = tiempo, y = Ingresos.E1 , color = "Leve"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Ingresos_H, aes(x = tiempo, y = Ingresos.E2 , color = "Moderado"), linetype = "solid", size = 1, alpha = 0.5) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue","Capaz Simulado" = "lightblue4", "Leve Simulado" = "maroon", "Moderado Simulado" = "darkblue"), name = "Estado") +
  labs(title = "Ingresos Conjuntos para Hombres \ncon Prima por Sexo", x = "Tiempo", y = "Ingresos") +
  xlim(0, 35) +
  cowplot::theme_cowplot()
print(ambos_ingresos_H)

ambos_egresos_H = ggplot() + 
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_0, color = "Capaz Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_1 , color = "Leve Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_2 , color = "Moderado Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_3 , color = "Severo Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_4 , color = "Profundo Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E0, color = "Capaz"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E1 , color = "Leve"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E2 , color = "Moderado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E3 , color = "Severo"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E4 , color = "Profundo"), linetype = "solid", size = 1, alpha = 0.5) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue", "Severo" = "purple", "Profundo" = "pink","Capaz Simulado" = "lightblue4", "Leve Simulado" = "maroon", "Moderado Simulado" = "darkblue", "Severo Simulado" = "purple", "Profundo Simulado" = "pink"), name = "Estado") +
  labs(title = "Egresos Conjuntos para Hombres \ncon Prima por Sexo", x = "Tiempo", y = "Egresos") + 
  cowplot::theme_cowplot()
print(ambos_egresos_H)

ambos_ingresos_M = ggplot() + 
  geom_line(data = df_ingresos_simulados_M, aes(x = Año, y = Ing_estim_0 , color = "Capaz Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_ingresos_simulados_M, aes(x = Año, y = Ing_estim_1 , color = "Leve Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_ingresos_simulados_M, aes(x = Año, y = Ing_estim_2 , color = "Moderado Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Ingresos_M, aes(x = tiempo, y = Ingresos.E0, color = "Capaz"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Ingresos_M, aes(x = tiempo, y = Ingresos.E1 , color = "Leve"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Ingresos_M, aes(x = tiempo, y = Ingresos.E2 , color = "Moderado"), linetype = "solid", size = 1, alpha = 0.5) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue","Capaz Simulado" = "lightblue4", "Leve Simulado" = "maroon", "Moderado Simulado" = "darkblue"), name = "Estado") +
  labs(title = "Ingresos Conjuntos para Hombres \ncon Prima por Sexo", x = "Tiempo", y = "Ingresos") +
  xlim(0, 35) +
  cowplot::theme_cowplot()
print(ambos_ingresos_M)

ambos_egresos_M = ggplot() + 
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_0, color = "Capaz Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_1 , color = "Leve Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_2 , color = "Moderado Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_3 , color = "Severo Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_4 , color = "Profundo Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E0, color = "Capaz"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E1 , color = "Leve"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E2 , color = "Moderado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E3 , color = "Severo"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E4 , color = "Profundo"), linetype = "solid", size = 1, alpha = 0.5) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue", "Severo" = "purple", "Profundo" = "pink","Capaz Simulado" = "lightblue4", "Leve Simulado" = "maroon", "Moderado Simulado" = "darkblue", "Severo Simulado" = "purple", "Profundo Simulado" = "pink"), name = "Estado") +
  labs(title = "Egresos Conjuntos para Hombres \ncon Prima por Sexo", x = "Tiempo", y = "Egresos") + 
  cowplot::theme_cowplot()
print(ambos_egresos_M)

#Prima unitaria

ambos_ingresos_unit_H = ggplot() + 
  geom_line(data = df_ingresos_simulados_unit_H, aes(x = Año, y = Ing_estim_0 , color = "Capaz Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_ingresos_simulados_unit_H, aes(x = Año, y = Ing_estim_1 , color = "Leve Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_ingresos_simulados_unit_H, aes(x = Año, y = Ing_estim_2 , color = "Moderado Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Ingresos_H, aes(x = tiempo, y = Ingresos.E0, color = "Capaz"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Ingresos_H, aes(x = tiempo, y = Ingresos.E1 , color = "Leve"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Ingresos_H, aes(x = tiempo, y = Ingresos.E2 , color = "Moderado"), linetype = "solid", size = 1, alpha = 0.5) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue","Capaz Simulado" = "lightblue4", "Leve Simulado" = "maroon", "Moderado Simulado" = "darkblue"), name = "Estado") +
  labs(title = "Ingresos Conjuntos para Hombres \ncon Prima Indiferente", x = "Tiempo", y = "Ingresos") +
  xlim(0, 35) +
  cowplot::theme_cowplot()
print(ambos_ingresos_unit_H)

ambos_egresos_unit_H = ggplot() + 
  geom_line(data = df_egresos_simulado_unit_H, aes(x = Año, y = Egresos_estim_0, color = "Capaz Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_unit_H, aes(x = Año, y = Egresos_estim_1 , color = "Leve Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_unit_H, aes(x = Año, y = Egresos_estim_2 , color = "Moderado Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_unit_H, aes(x = Año, y = Egresos_estim_3 , color = "Severo Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_unit_H, aes(x = Año, y = Egresos_estim_4 , color = "Profundo Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E0, color = "Capaz"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E1 , color = "Leve"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E2 , color = "Moderado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E3 , color = "Severo"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E4 , color = "Profundo"), linetype = "solid", size = 1, alpha = 0.5) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue", "Severo" = "purple", "Profundo" = "pink","Capaz Simulado" = "lightblue4", "Leve Simulado" = "maroon", "Moderado Simulado" = "darkblue", "Severo Simulado" = "purple", "Profundo Simulado" = "pink"), name = "Estado") +
  labs(title = "Egresos Conjuntos para Hombres \ncon Prima Indiferente", x = "Tiempo", y = "Egresos") + 
  cowplot::theme_cowplot()
print(ambos_egresos_unit_H)

ambos_ingresos_unit_M = ggplot() + 
  geom_line(data = df_ingresos_simulados_unit_M, aes(x = Año, y = Ing_estim_0 , color = "Capaz Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_ingresos_simulados_unit_M, aes(x = Año, y = Ing_estim_1 , color = "Leve Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_ingresos_simulados_unit_M, aes(x = Año, y = Ing_estim_2 , color = "Moderado Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Ingresos_M, aes(x = tiempo, y = Ingresos.E0, color = "Capaz"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Ingresos_M, aes(x = tiempo, y = Ingresos.E1 , color = "Leve"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Ingresos_M, aes(x = tiempo, y = Ingresos.E2 , color = "Moderado"), linetype = "solid", size = 1, alpha = 0.5) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue","Capaz Simulado" = "lightblue4", "Leve Simulado" = "maroon", "Moderado Simulado" = "darkblue"), name = "Estado") +
  labs(title = "Ingresos Conjuntos para Hombres \ncon Prima Indiferente", x = "Tiempo", y = "Ingresos") +
  xlim(0, 35) +
  cowplot::theme_cowplot()
print(ambos_ingresos_unit_M)

ambos_egresos_unit_M = ggplot() + 
  geom_line(data = df_egresos_simulado_unit_M, aes(x = Año, y = Egresos_estim_0, color = "Capaz Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_unit_M, aes(x = Año, y = Egresos_estim_1 , color = "Leve Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_unit_M, aes(x = Año, y = Egresos_estim_2 , color = "Moderado Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_unit_M, aes(x = Año, y = Egresos_estim_3 , color = "Severo Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = df_egresos_simulado_unit_M, aes(x = Año, y = Egresos_estim_4 , color = "Profundo Simulado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E0, color = "Capaz"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E1 , color = "Leve"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E2 , color = "Moderado"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E3 , color = "Severo"), linetype = "solid", size = 1, alpha = 0.5) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E4 , color = "Profundo"), linetype = "solid", size = 1, alpha = 0.5) +
  scale_color_manual(values = c("Capaz" = "lightblue4", "Leve" = "maroon", "Moderado" = "darkblue", "Severo" = "purple", "Profundo" = "pink","Capaz Simulado" = "lightblue4", "Leve Simulado" = "maroon", "Moderado Simulado" = "darkblue", "Severo Simulado" = "purple", "Profundo Simulado" = "pink"), name = "Estado") +
  labs(title = "Egresos Conjuntos para Hombres \ncon Prima Indiferente", x = "Tiempo", y = "Egresos") + 
  cowplot::theme_cowplot()
print(ambos_egresos_unit_M)

