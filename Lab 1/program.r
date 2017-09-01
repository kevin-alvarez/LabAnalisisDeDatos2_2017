
rawdata <- read.csv("C:/Users/Chocho/Desktop/Analisis de Datos/Laboratorio/Lab 1/allhypo_filtered.csv", header=TRUE, stringsAsFactors=FALSE)
data <- rawdata[(rawdata$age!="?" & rawdata$TSH.measured=="t" & rawdata$T3.measured=="t" & rawdata$TT4.measured=="t" & rawdata$T4U.measured=="t" & rawdata$FTI.measured=="t"),]


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

mean_TSH <- mean(TSH)
median_TSH <- median(TSH)
var_TSH <- var(TSH)

mean_T3 <- mean(T3)
median_T3 <- median(T3)
var_T3 <- var(T3)

mean_TT4 <- mean(TT4)
median_TT4 <- median(TT4)
var_TT4 <- var(TT4)

mean_T4U <- mean(T4U)
median_T4U <- median(T4U)
var_T4U <- var(T4U)

mean_FTI <- mean(FTI)
median_FTI <- median(FTI)
var_FTI <- var(FTI)

#Plots
#hist(t3levels)
#mynewdata <- mydata[-c(1, 2, 3), ] - Delete data