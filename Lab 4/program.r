# Clasificador bayesiano ingenuo - naiveBayes
# Clasifica clases preestablecidas
# No tiene requerimientos para las variables

library(e1071)



rawdata <- read.csv("allhypo.data", header = FALSE, sep = ",", stringsAsFactors = FALSE)
colnames(rawdata) <- c("age", "sex", "on.thyroxine", "query.on.thyroxine", "on.antithyroid.medication", "sick", "pregnant", "thyroid.surgery", "I131.treatment", "query.hypothyroid", "query.hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH.measured", "TSH", "T3.measured", "T3", "TT4.measured", "TT4", "T4U.measured", "T4U", "FTI.measured", "FTI", "TBG.measured", "TBG", "referral.source", "clasification|id")

#delete id from clasification|id column
d <- c()
for(i in rawdata$`clasification|id`){
  d <- c(d, strsplit(i, ".", fixed = TRUE)[[1]][1])
}
colnames(rawdata)[30] <- "clasification"
rawdata$clasification <- as.factor(d)

#data pre-processing
#delete NA values
data5 <- rawdata[(rawdata$age!="?" & rawdata$sex!="?" & rawdata$on.thyroxine!="?" & rawdata$query.on.thyroxine!="?" & rawdata$on.antithyroid.medication!="?" & rawdata$sick!="?" & rawdata$pregnant!="?" & rawdata$thyroid.surgery!="?" & rawdata$I131.treatment!="?" & rawdata$query.hypothyroid!="?" & rawdata$query.hyperthyroid!="?" & rawdata$lithium!="?" & rawdata$goitre!="?" & rawdata$tumor!="?" & rawdata$hypopituitary!="?" & rawdata$psych!="?" & rawdata$TSH.measured!="f" & rawdata$T3.measured!="f" & rawdata$TT4.measured!="f" & rawdata$T4U.measured!="f" & rawdata$FTI.measured!="f"),]
#delete variable TBG
data5$TBG.measured <- NULL
data5$TBG <- NULL
#delete measuring variables
data5$TSH.measured <- NULL
data5$T3.measured <- NULL
data5$TT4.measured <- NULL
data5$T4U.measured <- NULL
data5$FTI.measured <- NULL
#delete referral.source variable
data5$referral.source <- NULL

#data format transform
#nominal variables
data5$sex <- as.factor(data5$sex)
data5$on.thyroxine <- as.factor(data5$on.thyroxine)
data5$query.on.thyroxine <- as.factor(data5$query.on.thyroxine)
data5$on.antithyroid.medication <- as.factor(data5$on.antithyroid.medication)
data5$sick <- as.factor(data5$sick)
data5$pregnant <- as.factor(data5$pregnant)
data5$thyroid.surgery <- as.factor(data5$thyroid.surgery)
data5$I131.treatment <- as.factor(data5$thyroid.surgery)
data5$query.hypothyroid <- as.factor(data5$query.hypothyroid)
data5$query.hyperthyroid <- as.factor(data5$query.hyperthyroid)
data5$lithium <- as.factor(data5$lithium)
data5$goitre <- as.factor(data5$goitre)
data5$tumor <- as.factor(data5$tumor)
data5$hypopituitary <- as.factor(data5$hypopituitary)
data5$psych <- as.factor(data5$psych)

#continuous variables
data5$age <-as.numeric(data5$age)
data5$TSH <- as.numeric(data5$TSH)
data5$T3 <- as.numeric(data5$T3)
data5$TT4 <- as.numeric(data5$TT4)
data5$T4U <- as.numeric(data5$T4U)
data5$FTI <- as.numeric(data5$FTI)


model <- naiveBayes(clasification ~., data = data5)
    
# Importancia de cada variable
model$importance 

# predict necesita el parámetro newdata
results <- predict(object = model, newdata=data5, type = "class")
mc <- table(results,data5$clasification)
# Correctamente clasificados
100 * sum(diag(mc)) / sum(mc)

