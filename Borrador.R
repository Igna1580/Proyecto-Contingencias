
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



#--- Ignacio -------------------------------------------------------------------


func_simulacion <- function(prim_col, col_ant, col_act, sexo, año){
  
  if(sexo == "H"){
    proba <- Prob_Trans_Hombres
  }
  if(sexo == "M"){
    proba <- Prob_Trans_Mujeres
  }
  
  if(col_ant == 5){
    col_act <- 5
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
  
  
  if(col_act < proba[prim_col+fil_prob+año-22,3]){#Pasar al estado 0
    return(0)
  }else{
    if(col_act < sum(proba[prim_col+fil_prob+año-22,3:4])){#Pasar al estado 1
      return(1)
    }else{
      if(col_act < sum(proba[prim_col+fil_prob+año-22,3:5])){#Pasar al estado 2
        return(2)
      }else{
        if(col_act < sum(proba[prim_col+fil_prob+año-22,3:6])){#Pasar al estado 3
          return(3)
        }else{
          if(col_act < sum(proba[prim_col+fil_prob+año-22,3:7])){#Pasar al estado 4
            return(4)
          }else{
            return(5)
          }
        }
      }
    }
  }
}

vector_simulacion_H[[1]] %>%
  select(Edad,Año_0,Año_1,Año_2,Año_3,Año_4,Año_5,Año_6,Año_7,Año_8,Año_9) %>%
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

vector_simulacion_H[[1]] <- vector_simulacion_H[[1]] %>%
  rowwise() %>%
  mutate(Año_1 = func_simulacion(Edad, Año_0, Año_1,sexo="H",año=0),
         Año_2 = func_simulacion(Edad, Año_1, Año_2,sexo="H",año=1),
         Año_3 = func_simulacion(Edad, Año_2, Año_3,sexo="H",año=2)
  )
