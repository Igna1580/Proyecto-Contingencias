
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

#--------------------------------------------------

Prima <- function(age){
  P <- (B_severe*a_vitalicia(age, "Able", "Severe") 
        + B_profound*a_vitalicia(age, "Able", "Profound"))/(a_temporal(age, "Able", "Able") +
                                                              a_temporal(age, "Able", "Mild") +
                                                              a_temporal(age, "Able", "Moderate") - a_vitalicia(age, "Able", "Mild") - a_vitalicia(age, "Able", "Moderate"))
  
  return(P)
}





