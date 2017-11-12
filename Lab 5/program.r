# Árboles de decisión
library(C50)

preprocessing <- function(rawdata){
  #delete id from clasification|id column
  d <- c()
  for(i in rawdata$`clasification|id`){
    d <- c(d, strsplit(i, ".", fixed = TRUE)[[1]][1])
  }
  colnames(rawdata)[30] <- "clasification"
  rawdata$clasification <- as.factor(d)
  #data pre-processing
  #delete NA values
  data <- rawdata[(rawdata$age!="?" & rawdata$sex!="?" & rawdata$on.thyroxine!="?" & rawdata$query.on.thyroxine!="?" & rawdata$on.antithyroid.medication!="?" & rawdata$sick!="?" & rawdata$pregnant!="?" & rawdata$thyroid.surgery!="?" & rawdata$I131.treatment!="?" & rawdata$query.hypothyroid!="?" & rawdata$query.hyperthyroid!="?" & rawdata$lithium!="?" & rawdata$goitre!="?" & rawdata$tumor!="?" & rawdata$hypopituitary!="?" & rawdata$psych!="?" & rawdata$TSH.measured!="f" & rawdata$T3.measured!="f" & rawdata$TT4.measured!="f" & rawdata$T4U.measured!="f" & rawdata$FTI.measured!="f"),]
  #delete variable TBG
  data$TBG.measured <- NULL
  data$TBG <- NULL
  #delete measuring variables
  data$TSH.measured <- NULL
  data$T3.measured <- NULL
  data$TT4.measured <- NULL
  data$T4U.measured <- NULL
  data$FTI.measured <- NULL
  #delete referral.source variable
  data$referral.source <- NULL
  
  #data format transform
  #nominal variables
  data$sex <- as.factor(data$sex)
  data$on.thyroxine <- as.factor(data$on.thyroxine)
  data$query.on.thyroxine <- as.factor(data$query.on.thyroxine)
  data$on.antithyroid.medication <- as.factor(data$on.antithyroid.medication)
  data$sick <- as.factor(data$sick)
  data$pregnant <- as.factor(data$pregnant)
  data$thyroid.surgery <- as.factor(data$thyroid.surgery)
  data$I131.treatment <- as.factor(data$thyroid.surgery)
  data$query.hypothyroid <- as.factor(data$query.hypothyroid)
  data$query.hyperthyroid <- as.factor(data$query.hyperthyroid)
  data$lithium <- as.factor(data$lithium)
  data$goitre <- as.factor(data$goitre)
  data$tumor <- as.factor(data$tumor)
  data$hypopituitary <- as.factor(data$hypopituitary)
  data$psych <- as.factor(data$psych)
  
  #continuous variables
  data$age <-as.numeric(data$age)
  data$TSH <- as.numeric(data$TSH)
  data$T3 <- as.numeric(data$T3)
  data$TT4 <- as.numeric(data$TT4)
  data$T4U <- as.numeric(data$T4U)
  data$FTI <- as.numeric(data$FTI)
  
  #Discretización de las variables
  data$age <- cut(data$age, breaks =  seq(0,100,20))
  data$TSH <- cut(data$TSH, breaks =  c(0, 0.5, 4.7, Inf), labels = c("Inf", "Nor", "Sup"))
  data$T3 <- cut(data$T3, breaks =  c(0, 0.9, 2.8, Inf), labels = c("Inf", "Nor", "Sup"))
  data$TT4 <- cut(data$TT4, breaks =  c(0, 58, 161, Inf), labels = c("Inf", "Nor", "Sup"))
  data$T4U <- cut(data$T4U, breaks =  c(0, 0.8, 1.3, Inf), labels = c("Inf", "Nor", "Sup"))
  
  #Set binary clasification
  data$clasification <- ifelse(data$clasification %in% c("primary hypothyroid", "secondary hypothyroid", "compensated hypothyroid"), 1, 0)
  data$clasification <- as.factor(data$clasification)
  names(data)[names(data) == "clasification"] <- "hypothyroid"
  
  return(data)
}

#Data read
rawdata <- read.csv("allhypo.data", header = FALSE, sep = ",", stringsAsFactors = FALSE)
colnames(rawdata) <- c("age", "sex", "on.thyroxine", "query.on.thyroxine", "on.antithyroid.medication", "sick", "pregnant", "thyroid.surgery", "I131.treatment", "query.hypothyroid", "query.hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH.measured", "TSH", "T3.measured", "T3", "TT4.measured", "TT4", "T4U.measured", "T4U", "FTI.measured", "FTI", "TBG.measured", "TBG", "referral.source", "clasification|id")

#Preprocessing for data
data <- preprocessing(rawdata)
data_tree <- subset(data, select = -hypothyroid)
data_class <- subset(data, select = hypothyroid)$hypothyroid


#Model
model <-C5.0(data_tree, data_class)

#Tree plot
tree <- plot(model)

#output.tree <- ctree(
#  clasification ~ age+sex+on.thyroxine+query.on.thyroxine+on.antithyroid.medication+sick+pregnant+thyroid.surgery+I131.treatment+query.hypothyroid+query.hyperthyroid+lithium+goitre+tumor+hypopituitary+psych+TSH+T3+TT4+T4U, 
#  data = dataHypo)

#plot(output.tree)

#dev.off()