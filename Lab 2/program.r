library("cluster")

rawdata <- read.csv("allhypo.data", header = FALSE, sep = ",", stringsAsFactors = FALSE)
colnames(rawdata) <- c("age", "sex", "on.thyroxine", "query.on.thyroxine", "on.antithyroid.medication", "sick", "pregnant", "thyroid.surgery", "I131.treatment", "query.hypothyroid", "query.hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH.measured", "TSH", "T3.measured", "T3", "TT4.measured", "TT4", "T4U.measured", "T4U", "FTI.measured", "FTI", "TBG.measured", "TBG", "referral.source", "clasification|id")

#delete id from clasification|id column
d <- c()
for(i in rawdata$`clasification|id`){
  d <- c(d, strsplit(i, ".", fixed = TRUE)[[1]][1])
}
colnames(rawdata)[30] <- "clasification"
rawdata$clasification <- d

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
#Delete clasification variable
data$clasification <- NULL

#groups <- 6
#cluster <- pam(rawdata, groups, metric = "euclidean")
