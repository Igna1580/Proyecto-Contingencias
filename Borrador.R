
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
sum(rowSums(df_balance_simulado_H[,c(2,4,6,8,10)]))/sum(edades_selec_H$benef_tot_por_edad)
sum(rowSums(df_balance_simulado_M[,c(2,4,6,8,10)]))/sum(edades_selec_M$benef_tot_por_edad)


ambos_ingresos = ggplot() + 
  geom_line(data = df_ingresos_simulados_H, aes(x = Año, y = Ing_estim_0 , color = "Able_s"), linetype = "solid", size = 1) +
  geom_line(data = df_ingresos_simulados_H, aes(x = Año, y = Ing_estim_1 , color = "Mild_s"), linetype = "solid", size = 1) +
  geom_line(data = df_ingresos_simulados_H, aes(x = Año, y = Ing_estim_2 , color = "Moderate_s"), linetype = "solid", size = 1) +
  geom_line(data = Ingresos_H, aes(x = tiempo, y = Ingresos.E0, color = "Able"), linetype = "solid", size = 1) +
  geom_line(data = Ingresos_H, aes(x = tiempo, y = Ingresos.E1 , color = "Mild"), linetype = "solid", size = 1) +
  geom_line(data = Ingresos_H, aes(x = tiempo, y = Ingresos.E2 , color = "Moderate"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Able" = "lightblue4", "Mild" = "maroon", "Moderate" = "darkblue","Able_s" = "lightblue4", "Mild_s" = "maroon", "Moderate_s" = "darkblue"), name = "Estado") +
  xlab('Tiempo') +
  ylab('Ingresos Simulados') + cowplot::theme_cowplot()
print(ambos_ingresos)

ambos_egresos = ggplot() + 
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_0, color = "Able_s"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_1 , color = "Mild_s"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_2 , color = "Moderate_s"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_3 , color = "Severe_s"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_H, aes(x = Año, y = Egresos_estim_4 , color = "Profound_s"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E0, color = "Able"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E1 , color = "Mild"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E2 , color = "Moderate"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E3 , color = "Severe"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_H, aes(x = tiempo, y = Egresos.E4 , color = "Profound"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Able" = "lightblue4", "Mild" = "maroon", "Moderate" = "darkblue", "Severe" = "purple", "Profound" = "pink","Able_s" = "lightblue4", "Mild_s" = "maroon", "Moderate_s" = "darkblue", "Severe_s" = "purple", "Profound_s" = "pink"), name = "Estado") +
  xlab('Tiempo') +
  ylab('Egresos Simulados') + cowplot::theme_cowplot()
print(ambos_egresos)



x <- 30
u.ax.n_ij(x,i=0,j=1,sexo = "H")
obtencion_tabla_proyeccion(x,0,"H")

(A*u.ax.n_ij(x,i=0,j=1,sexo="H")) +
  (B*u.ax.n_ij(x,i=0,j=2,sexo="H")) +
  (C*u.ax.n_ij(x,i=0,j=3,sexo="H")) +
  (D*u.ax.n_ij(x,i=0,j=4,sexo="H")) +
  0.15*Prima_justa_H[x-29] -
  0.95*Prima_justa_H[x-29]*((ax.n_ij(x,n = 65-x,i=0,j=0,sexo="H")) +
                       (ax.n_ij(x,n = 65-x,i=0,j=1,sexo="H")) +
                       (ax.n_ij(x,n = 65-x,i=0,j=2,sexo="H")))

0.006232994
0.01198371
0.01730149

