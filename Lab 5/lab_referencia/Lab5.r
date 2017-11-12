#Nostros guardamos las tablas en un archivo txt donde se separan las columnas por
#comas, luego usamos read.table.
#file.choose() nos permite buscar el archivo en vez de escribir su ruta
#sep = "," es necesario para que R entienda que las columnas están separadas por
#comas
library(party)
Estudio<-read.table(file.choose(),header = T,sep = ",")

##### PREPROCESAMIENTO

# Se eliminan algunas columnas de datos que no serán utiizadas.

#LOOP 1:
keeps <- c("age","sex","on.thyroxine","query.on.thyroxine","on.antithyroid.medication","sick","pregnant","thyroid.surgery","I131.treatment","query.hypothyroid","query.hyperthyroid","lithium","goitre","tumor","hypopituitary","psych","TSH.measured","TSH","T3.measured","T3","TT4.measured","TT4","T4U.measured","T4U","diagnostico")

#keeps <- c("age","sex","on.thyroxine","on.antithyroid.medication","sick","pregnant","thyroid.surgery","I131.treatment","lithium","goitre","tumor","hypopituitary","psych","TSH","T3","TT4","T4U","diagnostico")

EstudioLimpiado <- Estudio[ , keeps, drop = FALSE]

myvector = vector(length = 0)
for(row in 1:nrow(EstudioLimpiado)){
 for (col in 1:length(EstudioLimpiado)){
    if( EstudioLimpiado[row, col] == "?"){
      myvector = append(myvector, row)
      print(row)
      break
    }
  }
}
EstudioLimpiado <- EstudioLimpiado[-myvector, ,drop=F]


# Se eliminan los registros cuyos valores están fuera de un rango de valores "aceptables".
TSHMAX <- 4.70 + abs(0.5 - 4.70) * 3
T3MAX <-  2.8 + abs(0.9 - 2.8) * 3
TT4MAX <- 161 + abs(58-161) * 3
T4UMAX <- 1.3 + abs(0.8 - 1.3) * 3
AGEMAX <- 120
EstudioLimpiado <- subset(EstudioLimpiado, (as.numeric(as.character(age)) <= AGEMAX) & (as.numeric(as.character(TSH)) <= TSHMAX) & (as.numeric(as.character(T3)) <= T3MAX) & (as.numeric(as.character(TT4)) <= TT4MAX) & (as.numeric(as.character(T4U)) <= T4UMAX))

# Se realiza una transformación de los datos al tipo de dato al cual corresponden.
# Esto para que las variables numericas no sean manejadas como factores.
Estudio2 <- EstudioLimpiado
Estudio2$age <- as.numeric(as.character(Estudio2$age))
Estudio2$TSH <- as.numeric(as.character(Estudio2$TSH))
Estudio2$T3 <- as.numeric(as.character(Estudio2$T3))
Estudio2$TT4 <- as.numeric(as.character(Estudio2$TT4))
Estudio2$T4U <- as.numeric(as.character(Estudio2$T4U))

# Conversión de diagnostico a varible logica:
Estudio2$diagnostico <- (Estudio2$diagnostico == "hyperthyroid.")

# Discretización de las variables
Estudio2$age <- cut(Estudio2$age, breaks =  seq(0,100,20))
Estudio2$TSH <- cut(Estudio2$TSH, breaks =  c(0, 0.5, 4.7, Inf), labels = c("Inf", "Nor", "Sup"))
Estudio2$T3 <- cut(Estudio2$T3, breaks =  c(0, 0.9, 2.8, Inf), labels = c("Inf", "Nor", "Sup"))
Estudio2$TT4 <- cut(Estudio2$TT4, breaks =  c(0, 58, 161, Inf), labels = c("Inf", "Nor", "Sup"))
Estudio2$T4U <- cut(Estudio2$T4U, breaks =  c(0, 0.8, 1.3, Inf), labels = c("Inf", "Nor", "Sup"))


### Variable Estudio2 cuenta con los datos filtrados y bien codificados


##### ARBOL DE DECISIÓN
str(Estudio2)

png(file = "decision_tree.png")

output.tree <- ctree(
  diagnostico ~ age+sex+on.thyroxine+query.on.thyroxine+on.antithyroid.medication+sick+pregnant+thyroid.surgery+I131.treatment+query.hypothyroid+query.hyperthyroid+lithium+goitre+tumor+hypopituitary+psych+TSH.measured+TSH+T3.measured+T3+TT4.measured+TT4+T4U.measured+T4U, 
  data = Estudio2)

plot(output.tree)

dev.off()
