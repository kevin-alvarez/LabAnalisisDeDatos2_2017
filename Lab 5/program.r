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
  
  #continuous variables and clasification to binary 
  #age values
  child.adult_border <- 18
  adult.oldman_border <- 60
  #min values
  TSH.min <- 0.4
  T3.min <- 1.07
  TT4.min <- 64.0
  T4U.min <- 0.7
  FTI.min <- 33.108
  #max values
  TSH.max <- 4.0
  T3.max <- 3.37
  TT4.max <- 154.0
  T4U.max <- 1.8
  FTI.max <- 135.191
  
  #vectors(zeros)
  child <- integer(length(data[[1]]))
  adult <- integer(length(data[[1]]))
  oldman <- integer(length(data[[1]]))
  TSH.under <- integer(length(data[[1]]))
  T3.under <- integer(length(data[[1]]))
  TT4.under <- integer(length(data[[1]]))
  T4U.under <- integer(length(data[[1]]))
  FTI.under <- integer(length(data[[1]]))
  TSH.over <- integer(length(data[[1]]))
  T3.over <- integer(length(data[[1]]))
  TT4.over <- integer(length(data[[1]]))
  T4U.over <- integer(length(data[[1]]))
  FTI.over <- integer(length(data[[1]]))
  
  #change data to binary
  for(i in 1:length(data[[1]])){
    #age
    if(data$age[i] < child.adult_border){
      child[i] <- 1
    }else if(data$age[i] >= child.adult_border & data$age[i] < adult.oldman_border){
      adult[i] <- 1
    }else if(data$age[i] >= adult.oldman_border){
      oldman[i] <- 1
    }
    #hormones
    if(data$TSH[i] >= TSH.max){
      TSH.over[i] <- 1
    }else if(data$TSH[i] <= TSH.min){
      TSH.under[i] <- 1
    }
    if(data$T3[i] >= T3.max){
      T3.over[i] <- 1
    }else if(data$T3[i] <= T3.min){
      T3.under[i] <- 1
    }
    if(data$TT4[i] >= TT4.max){
      TT4.over[i] <- 1
    }else if(data$TT4[i] <= TT4.min){
      TT4.under[i] <- 1
    }
    if(data$T4U[i] >= T4U.max){
      T4U.over[i] <- 1
    }else if(data$T4U[i] <= T4U.min){
      T4U.under[i] <- 1
    }
    if(data$FTI[i] >= FTI.max){
      FTI.over[i] <- 1
    }else if(data$FTI[i] <= FTI.min){
      FTI.under[i] <- 1
    }
  }
  
  #Discretización de las variables
  #data$age <- cut(data$age, breaks =  seq(0,100,20))
  #data$TSH <- cut(data$TSH, breaks =  c(0, 0.5, 4.7, Inf), labels = c("Inf", "Nor", "Sup"))
  #data$T3 <- cut(data$T3, breaks =  c(0, 0.9, 2.8, Inf), labels = c("Inf", "Nor", "Sup"))
  #data$TT4 <- cut(data$TT4, breaks =  c(0, 58, 161, Inf), labels = c("Inf", "Nor", "Sup"))
  #data$T4U <- cut(data$T4U, breaks =  c(0, 0.8, 1.3, Inf), labels = c("Inf", "Nor", "Sup"))
  
  data$clasification <- ifelse(data$clasification %in% c("primary hypothyroid", "secondary hypothyroid", "compensated hypothyroid"), 1, 0)
  
  #replace vectors on data frame
  data$age <- NULL
  data$TSH <- NULL
  data$T3 <- NULL
  data$TT4 <- NULL
  data$T4U <- NULL
  data$FTI <- NULL
  data$age.child <- as.factor(child)
  data$age.adult <- as.factor(adult)
  data$age.oldman <- as.factor(oldman)
  data$TSH.over <- as.factor(TSH.over)
  data$T3.over <- as.factor(T3.over)
  data$TT4.over <- as.factor(TT4.over)
  data$T4U.over <- as.factor(T4U.over)
  data$FTI.over <- as.factor(FTI.over)
  data$TSH.under <- as.factor(TSH.under)
  data$T3.under <- as.factor(T3.under)
  data$TT4.under <- as.factor(TT4.under)
  data$T4U.under <- as.factor(T4U.under)
  data$FTI.under <- as.factor(FTI.under)
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


#Model
model <-C5.0(data_tree, data$hypothyroid)

#Tree plot
tree <- plot(model)

#output.tree <- ctree(
#  clasification ~ age+sex+on.thyroxine+query.on.thyroxine+on.antithyroid.medication+sick+pregnant+thyroid.surgery+I131.treatment+query.hypothyroid+query.hyperthyroid+lithium+goitre+tumor+hypopituitary+psych+TSH+T3+TT4+T4U, 
#  data = dataHypo)

#plot(output.tree)

#dev.off()