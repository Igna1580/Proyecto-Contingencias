
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
sum(rowSums(df_balance_simulado_H[,c(2,4,6,8,10)]))
sum(rowSums(df_balance_simulado_M[,c(2,4,6,8,10)]))

(
  sum(rowSums(df_ingresos_simulados_H[,c(2,4,6)])) -
    sum(rowSums(df_beneficios_simulados_H[,c(2,4,6,8)])) -
    sum(rowSums(df_costos_simulados_H[,c(2,4,6)])) +
    
    sum(rowSums(df_ingresos_simulados_M[,c(2,4,6)])) -
    sum(rowSums(df_beneficios_simulados_M[,c(2,4,6,8)])) -
    sum(rowSums(df_costos_simulados_M[,c(2,4,6)]))
)


Prob_Trans_Hombres
Verificacion_simul <- vector_simulacion_H[[1]]

vector_simulacion_H_probabilidades <- vector_simulacion_H[[1]]

i<-81
(
v_pob_64menos_xaño_0_M[[i]] + v_pob_65mas_xaño_0_M[[i]] +
v_pob_64menos_xaño_1_M[[i]] + v_pob_65mas_xaño_1_M[[i]] +
v_pob_64menos_xaño_2_M[[i]] + v_pob_65mas_xaño_2_M[[i]] +
v_pob_64menos_xaño_3_M[[i]] + v_pob_65mas_xaño_3_M[[i]] +
v_pob_64menos_xaño_4_M[[i]] + v_pob_65mas_xaño_4_M[[i]] +
v_pob_64menos_xaño_5_M[[i]] + v_pob_65mas_xaño_5_M[[i]]

)
tPx_ij(30,35,i=0,j=0,sexo = "H")
obtencion_tabla_proyeccion(30,0,"H")
u.ax.n_ij(30,j=0,sexo="H")
u.ax.n_ij_12(30,j=0,sexo="H")
