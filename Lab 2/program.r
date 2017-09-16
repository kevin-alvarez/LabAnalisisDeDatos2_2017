library("cluster")
library("ggplot2")
library("Rtsne")

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

#Continuous variables
data$age <- as.numeric(data$age)
data$TSH <- as.numeric(data$TSH)
data$T3 <- as.numeric(data$T3)
data$TT4 <- as.numeric(data$TT4)
data$T4U <- as.numeric(data$T4U)
data$FTI <- as.numeric(data$FTI)

# Data selection - set max value for detection of data anomalies
TSHMAX <- 4.0 + abs(0.4 - 4.0) * 2
T3MAX <-  3.37 + abs(1.07 - 3.37) * 2
TT4MAX <- 164 + abs(64 - 164) * 2
T4UMAX <- 1.8 + abs(0.7 - 1.8) * 2
FTIMAX <- 1.8 + abs(33.108 - 135.191) * 2
data <- subset(data, (age <= 100) & (TSH <= TSHMAX) & (T3 <= T3MAX) & (TT4 <= TT4MAX) & (T4U <= T4UMAX) & (FTI <= FTIMAX))

#Calculating Dissimilarity with Gower Distance
data_dist <- daisy(data, metric = "gower")

#Calculating optimal number of groups with Silhouette width
sil <- c()
for (i in 2:15){
  fit <- pam(data_dist, diss = TRUE, k = i)
  sil[i] <- fit$silinfo$avg.width
}

clust_num_plot <- ggplot(data.frame(clust_num = 2:15, sil_width = sil[2:15]), aes(x = clust_num, y = sil_width))+labs(x = "Number of Clusters", y = "Silhouette Width")+geom_line(color = "blue")+geom_point(color = "blue")+ggtitle("Number of Clusters vs Silhouette Width")+theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

#Clustering [k = 3]
data_cluster <- pam(data_dist, diss = TRUE, k = 3)

#t-SNE plot
tsne <- Rtsne(data_dist, is_distance = TRUE)
ggplot(data.frame(tsne$Y), aes(x = X1, y = X2))+labs(x = "X", y = "Y")+geom_point(color = factor(data_cluster$clustering))

