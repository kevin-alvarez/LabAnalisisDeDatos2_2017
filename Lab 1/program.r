
rawdata <- read.csv("C:/Users/Chocho/Desktop/Analisis de Datos/Laboratorio/Lab 1/allhypo_filtered.csv", header=TRUE)
data <- rawdata[(rawdata$TSH.measured=="t" & rawdata$T3.measured=="t" & rawdata$TT4.measured=="t" & rawdata$T4U.measured=="t" & rawdata$FTI.measured=="t"),]


#Var Vectors
tshlevels <- as.numeric(data$TSH)
t3levels <- as.numeric(data$T3)
t4ulevels <- as.numeric(data$T4U)

#Plots
hist(t3levels)
#mynewdata <- mydata[-c(1, 2, 3), ] - Delete data