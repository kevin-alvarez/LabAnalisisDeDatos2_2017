
rawdata <- read.csv("c:\\Users\\Chocho\\Desktop\\Analisis de Datos\\Laboratorio\\Lab 1\\allhypo_filtered.csv", header=TRUE, stringsAsFactors=FALSE)
data <- rawdata[(rawdata$age!="?" & rawdata$sex != "?" & rawdata$TSH.measured=="t" & rawdata$T3.measured=="t" & rawdata$TT4.measured=="t" & rawdata$T4U.measured=="t" & rawdata$FTI.measured=="t"),]

#Clean state
d <- c()
for(i in data$state){
  d <- c(d, strsplit(i, ".", fixed = TRUE)[[1]][1])
}
data$state <- d

#Var Vectors
age <- as.numeric(data$age)
TSH <- as.numeric(data$TSH)
T3 <- as.numeric(data$T3)
TT4 <- as.numeric(data$TT4)
T4U <- as.numeric(data$T4U)
FTI <- as.numeric(data$FTI)

mean_age <- mean(age)
median_age <- median(age)
var_age <- var(age)
moda_age <- mlv(age,method = "mfv")[1]

mean_TSH <- mean(TSH)
median_TSH <- median(TSH)
var_TSH <- var(TSH)
moda_TSH <- mlv(TSH,method = "mfv")[1]

mean_T3 <- mean(T3)
median_T3 <- median(T3)
var_T3 <- var(T3)
moda_T3 <- mlv(T3,method = "mfv")[1]

mean_TT4 <- mean(TT4)
median_TT4 <- median(TT4)
var_TT4 <- var(TT4)
moda_TT4 <- mlv(TT4,method = "mfv")[1]

mean_T4U <- mean(T4U)
median_T4U <- median(T4U)
var_T4U <- var(T4U)
moda_T4U <- mlv(T4U,method = "mfv")[1]

mean_FTI <- mean(FTI)
median_FTI <- median(FTI)
var_FTI <- var(FTI)
moda_FTI <- mlv(FTI,method = "mfv")[1]

#Plots (package: ggplot2)
#Gráfico clasificacion de la muestra.
p1 <- ggplot(data, aes(data$state))+geom_bar()+theme_bw()+scale_fill_brewer(palette = "Set1")

#State vs sex
p2 <- ggplot(data, aes(data$state, fill = data$sex))+geom_bar(position = position_dodge())+theme_bw()+scale_fill_brewer(palette = "Paired")

#State vs pregnant
p3 <- ggplot(data, aes(data$state, fill = data$pregnant))+geom_bar(position = position_dodge())+theme_bw()+scale_fill_brewer(palette = "Paired")

#State vs thyroid.surgery
p4 <- ggplot(data, aes(data$state, fill = data$thyroid.surgery))+geom_bar(position = position_dodge())+theme_bw()+scale_fill_brewer(palette = "Paired")

#State vs I131.treatment
p5 <- ggplot(data, aes(data$state, fill = data$I131.treatment))+geom_bar(position = position_dodge())+theme_bw()+scale_fill_brewer(palette = "Paired")

#State vs lithium
p6 <- ggplot(data, aes(data$state, fill = data$lithium))+geom_bar(position = position_dodge())+theme_bw()+scale_fill_brewer(palette = "Paired")

#State vs goitre
p7 <- ggplot(data, aes(data$state, fill = data$goitre))+geom_bar(position = position_dodge())+theme_bw()+scale_fill_brewer(palette = "Paired")


#Correlation
corData <- data.frame(age, TSH, T3, TT4, T4U, FTI)
resCorrelate <- cor(corData, method = "pearson")

