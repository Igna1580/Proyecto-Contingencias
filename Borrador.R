
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


ambos_ingresos = ggplot() + 
  geom_line(data = df_ingresos_simulados_M, aes(x = Año, y = Ing_estim_0 , color = "Able_s"), linetype = "solid", size = 1) +
  geom_line(data = df_ingresos_simulados_M, aes(x = Año, y = Ing_estim_1 , color = "Mild_s"), linetype = "solid", size = 1) +
  geom_line(data = df_ingresos_simulados_M, aes(x = Año, y = Ing_estim_2 , color = "Moderate_s"), linetype = "solid", size = 1) +
  geom_line(data = Ingresos_M, aes(x = tiempo, y = Ingresos.E0, color = "Able"), linetype = "solid", size = 1) +
  geom_line(data = Ingresos_M, aes(x = tiempo, y = Ingresos.E1 , color = "Mild"), linetype = "solid", size = 1) +
  geom_line(data = Ingresos_M, aes(x = tiempo, y = Ingresos.E2 , color = "Moderate"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Able" = "lightblue4", "Mild" = "maroon", "Moderate" = "darkblue","Able_s" = "lightblue4", "Mild_s" = "maroon", "Moderate_s" = "darkblue"), name = "Estado") +
  xlab('Tiempo') +
  ylab('Ingresos Simulados') + cowplot::theme_cowplot()
print(ambos_ingresos)

ambos_egresos = ggplot() + 
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_0, color = "Able_s"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_1 , color = "Mild_s"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_2 , color = "Moderate_s"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_3 , color = "Severe_s"), linetype = "solid", size = 1) +
  geom_line(data = df_egresos_simulado_M, aes(x = Año, y = Egresos_estim_4 , color = "Profound_s"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E0, color = "Able"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E1 , color = "Mild"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E2 , color = "Moderate"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E3 , color = "Severe"), linetype = "solid", size = 1) +
  geom_line(data = Egresos_M, aes(x = tiempo, y = Egresos.E4 , color = "Profound"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Able" = "lightblue4", "Mild" = "maroon", "Moderate" = "darkblue", "Severe" = "purple", "Profound" = "pink","Able_s" = "lightblue4", "Mild_s" = "maroon", "Moderate_s" = "darkblue", "Severe_s" = "purple", "Profound_s" = "pink"), name = "Estado") +
  xlab('Tiempo') +
  ylab('Egresos Simulados') + cowplot::theme_cowplot()
print(ambos_egresos)

df_beneficios_simulados_M <- tibble(
  Año = 0:80) %>%
  rowwise() %>%
  mutate(
    Benef_estim_1 = A*mean(v_pob_65mas_xaño_1_M[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_1 = A*quantile(v_pob_65mas_xaño_1_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Benef_estim_2 = B*mean(v_pob_65mas_xaño_2_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_2 = B*quantile(v_pob_65mas_xaño_2_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Benef_estim_3 = C*mean(v_pob_65mas_xaño_3_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_3 = C*quantile(v_pob_65mas_xaño_3_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año),
    Benef_estim_4 = D*mean(v_pob_65mas_xaño_4_H[[Año+1]])*((1+inflacion)/(1+descuento))^(Año),
    Perc_99.5_4 = D*quantile(v_pob_65mas_xaño_4_H[[Año+1]],0.995)*((1+inflacion)/(1+descuento))^(Año)
  )
