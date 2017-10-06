library("arules")
library("arulesViz")


rawdata <- read.csv("allhypo.data", header = FALSE, sep = ",", stringsAsFactors = FALSE)
colnames(rawdata) <- c("age", "sex", "on.thyroxine", "query.on.thyroxine", "on.antithyroid.medication", "sick", "pregnant", "thyroid.surgery", "I131.treatment", "query.hypothyroid", "query.hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH.measured", "TSH", "T3.measured", "T3", "TT4.measured", "TT4", "T4U.measured", "T4U", "FTI.measured", "FTI", "TBG.measured", "TBG", "referral.source", "clasification|id")

#delete id from clasification|id column
d <- c()
for(i in rawdata$`clasification|id`){
  d <- c(d, strsplit(i, ".", fixed = TRUE)[[1]][1])
}
colnames(rawdata)[30] <- "clasification"
rawdata$clasification <- as.factor(d)

#Data pre-processing
#delete NA values
data <- rawdata[(rawdata$age!="?" & rawdata$sex!="?" & rawdata$on.thyroxine!="?" & rawdata$query.on.thyroxine!="?" & rawdata$on.antithyroid.medication!="?" & rawdata$sick!="?" & rawdata$pregnant!="?" & rawdata$thyroid.surgery!="?" & rawdata$I131.treatment!="?" & rawdata$query.hypothyroid!="?" & rawdata$query.hyperthyroid!="?" & rawdata$lithium!="?" & rawdata$goitre!="?" & rawdata$tumor!="?" & rawdata$hypopituitary!="?" & rawdata$psych!="?" & rawdata$TSH.measured!="f" & rawdata$T3.measured!="f" & rawdata$TT4.measured!="f" & rawdata$T4U.measured!="f" & rawdata$FTI.measured!="f"),]
#Delete variable TBG
data$TBG.measured <- NULL
data$TBG <- NULL
#Delete measuring variables
data$TSH.measured <- NULL
data$T3.measured <- NULL
data$TT4.measured <- NULL
data$T4U.measured <- NULL
data$FTI.measured <- NULL
#Delete referral.source variable
data$referral.source <- NULL

#Deleting continuous variables
#data$age <-NULL
#data$TSH <- NULL
#data$T3 <- NULL
#data$TT4 <- NULL
#data$T4U <- NULL
#data$FTI <- NULL

#Data format transform
#Nominal variables
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

data$age <-as.factor(data$age)
data$TSH <- as.factor(data$TSH)
data$T3 <- as.factor(data$T3)
data$TT4 <- as.factor(data$TT4)
data$T4U <- as.factor(data$T4U)
data$FTI <- as.factor(data$FTI)

# Data selection - set max value for detection of data anomalies
#TSHMAX <- 4.0 + abs(0.4 - 4.0) * 2
#T3MAX <-  3.37 + abs(1.07 - 3.37) * 2
#TT4MAX <- 164 + abs(64 - 164) * 2
#T4UMAX <- 1.8 + abs(0.7 - 1.8) * 2
#FTIMAX <- 1.8 + abs(33.108 - 135.191) * 2
#data <- subset(data, (age <= 100) & (TSH <= TSHMAX) & (T3 <= T3MAX) & (TT4 <= TT4MAX) & (T4U <= T4UMAX) & (FTI <= FTIMAX))

d <- c()
for(i in data$clasification){
  if(i == "negative"){
    d <- c(d, 0)
  }else if(i == "primary hypothyroid"){
    d <- c(d, 1)
  }else if(i == "secondary hypothyroid"){
    d <- c(d, 2)
  }else if(i == "compensated hypothyroid"){
    d <- c(d, 3)
  }
}
data$clasification <- as.factor(d)

#rules without restrictions, with min 2 antecedents and 1 consecuent [support=0.5, Confidence=0.8]
rules <- apriori(data, parameter = list(minlen=3, support=0.3, confidence=0.8, target="rules"))

#rules with class restrinction, with min 2 antecedents and 1 consecuent [support=0.5, Confidence=0.8]
rules_rest <- apriori(data, parameter = list(minlen=3, support=0.5, confidence=0.8, target="rules"), appearance = list(rhs=c("clasification=0", "clasification=1", "clasification=2", "clasification=3"), default="lhs"))
#rules_rest <- apriori(data, parameter = list(minlen=2, support=0.1, confidence=0.5, target="rules"), appearance = list(rhs=c("sex=F", "sex=M"), default="lhs"))

#sort rules
rules_sorted <- sort(rules_rest, by="confidence")
inspect(head(rules_sorted, 100))

#Clean Redundant Rules (No funciona con datasets grandes)
subset.matrix<-is.subset(rules_ord,rules_ord)
subset.matrix[lower.tri(subset.matrix,diag=T)]<-NA
redundant<-colSums(subset.matrix,na.rm = T)>=1
rules_notRed<-rules_ord[!redundant]
