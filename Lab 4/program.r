# Clasificador bayesiano ingenuo - naiveBayes
# Clasifica clases preestablecidas
# No tiene requerimientos para las variables

library("e1071")

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
  
  return(data)
}

#Data read
rawdata_train <- read.csv("allhypo.data", header = FALSE, sep = ",", stringsAsFactors = FALSE)
colnames(rawdata_train) <- c("age", "sex", "on.thyroxine", "query.on.thyroxine", "on.antithyroid.medication", "sick", "pregnant", "thyroid.surgery", "I131.treatment", "query.hypothyroid", "query.hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH.measured", "TSH", "T3.measured", "T3", "TT4.measured", "TT4", "T4U.measured", "T4U", "FTI.measured", "FTI", "TBG.measured", "TBG", "referral.source", "clasification|id")

rawdata_test <- read.csv("allhypo.test", header = FALSE, sep = ",", stringsAsFactors = FALSE)
colnames(rawdata_test) <- c("age", "sex", "on.thyroxine", "query.on.thyroxine", "on.antithyroid.medication", "sick", "pregnant", "thyroid.surgery", "I131.treatment", "query.hypothyroid", "query.hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH.measured", "TSH", "T3.measured", "T3", "TT4.measured", "TT4", "T4U.measured", "T4U", "FTI.measured", "FTI", "TBG.measured", "TBG", "referral.source", "clasification|id")

#Preprocessing for both data, train and test
data_train <- preprocessing(rawdata_train)
data_test <- preprocessing(rawdata_test)

#Training
model <- naiveBayes(clasification ~., data = data_train)

#Data predict
results <- predict(object = model, newdata=data_test, type = "class")

#Confusion Matrix - Predicted vs Trained
cm <- table(results,data_test$clasification)

#Percentage of hit
hit <- 100 * sum(diag(cm)) / sum(cm)
hit

