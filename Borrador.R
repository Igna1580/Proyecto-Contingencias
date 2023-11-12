
i <- 0.04
v <- 1/(1+i)

#Probabilidades

ProbaH <- read_delim("ProbTransHombres.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
ProbaM <- read_delim("ProbTransMujeres.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Definir probabilidad de transicion desde DF

p1_transicion <- function(sex, age, from, to){
  
  if(sex == "hombre"){
    p <- ProbaH[ProbaH$Status == from & ProbaH$Age == age, to]
  } else{
    p <- ProbaM[ProbaH$Status == from & ProbaH$Age == age, to]
  }
  
  return(p)
}

pt_transición <- function(sex, age, state, t){#probabilidad de mantenerse en un estado
  x+k <- age
  k <- 0
  p <- 1
  while(k<t){
    p <- p*p_transicion(sex, x+k, state, state)
    k <- k + 1
  }
  
  return(p)
}

#Anualidades

a_vitalicia <- function(age, from, to){
  p <- funcion_de_probabilidad
  a <- sum(sapply(1:(109-age), function(k) v^k(p(k))))
  return(a)
}
  
a_temporal <- function(age, from, to){
  p <- funcion_de_probabilidad
  a <- sum(sapply(1:(65-age), function(k) v^k(p(k))))
  return(a)
}

#---beneficios-------------------------------------

B_mild <- 0.1
B_moderate <- 0.3
B_severe <- 2000000
B_profound <- 8000000

#----gastos-----------------------------------------

G_emision <- 0.01 #%de prima
G_renovación <- 0.5 #% de prima




Prima <- function(age){
  P <- (B_severe*a_vitalicia(age, "Able", "Severe") 
        + B_profound*a_vitalicia(age, "Able", "Profound"))/(1-G_renovacion)((a_temporal(age, "Able", "Able") +
                                                              a_temporal(age, "Able", "Mild") +
                                                              a_temporal(age, "Able", "Moderate")) - a_vitalicia(age, "Able", "Mild") - a_vitalicia(age, "Able", "Moderate"))-G_emision
  
  return(P)
}



#--- Ignacio --------------------------------------------------------------

Prim_vec <- table(vector_simulacion_H[[1]][,5])

sum((vector_simulacion_H[[1]][,1+2] == 5) & (1+vector_simulacion_H[[1]][,1]) >= 65)
(vector_simulacion_H[[i]][,1+2] == 0) & (1==1)

safg <- sum((1+vector_simulacion_H[[1]][,1]) >= 65)
safg
for(i in 1:66){
  print((v_pob_64menos_xaño_0_H[[i]]+v_pob_65mas_xaño_0_H[[i]]+
     v_pob_64menos_xaño_1_H[[i]]+v_pob_65mas_xaño_1_H[[i]]+
     v_pob_64menos_xaño_2_H[[i]]+v_pob_65mas_xaño_2_H[[i]]+
     v_pob_64menos_xaño_3_H[[i]]+v_pob_65mas_xaño_3_H[[i]]+
     v_pob_64menos_xaño_4_H[[i]]+v_pob_65mas_xaño_4_H[[i]]+
     v_pob_64menos_xaño_5_H[[i]]+v_pob_65mas_xaño_5_H[[i]]
  ))
}









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
    v_pob <- c(v_pob, table(df_simulacion[[i]][,año+2])
               sum(df_simulacion[[i]][,año+2] == estado))
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
    v_pob <- c(v_pob, sum((df_simulacion[[i]][,año+2] == estado) & (año+df_simulacion[[i]][,1]) <= 65))
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



