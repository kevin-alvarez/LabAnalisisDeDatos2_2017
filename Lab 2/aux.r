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
data2 <- rawdata[(rawdata$age!="?" & rawdata$sex!="?" & rawdata$on.thyroxine!="?" & rawdata$query.on.thyroxine!="?" & rawdata$on.antithyroid.medication!="?" & rawdata$sick!="?" & rawdata$pregnant!="?" & rawdata$thyroid.surgery!="?" & rawdata$I131.treatment!="?" & rawdata$query.hypothyroid!="?" & rawdata$query.hyperthyroid!="?" & rawdata$lithium!="?" & rawdata$goitre!="?" & rawdata$tumor!="?" & rawdata$hypopituitary!="?" & rawdata$psych!="?" & rawdata$TSH.measured!="f" & rawdata$T3.measured!="f" & rawdata$TT4.measured!="f" & rawdata$T4U.measured!="f" & rawdata$FTI.measured!="f"),]
#Delete variable TBG
data2$TBG.measured <- NULL
data2$TBG <- NULL
data2$sex.measured <- NULL
data2$sex <- NULL
data2$on.thyroxine.measured <- NULL
data2$on.thyroxine <- NULL
data2$query.on.thyroxine.measured <- NULL
data2$query.on.thyroxine<- NULL
data2$on.antithyroid.medication.measured <- NULL
data2$on.antithyroid.medication <- NULL
data2$sick.measured <- NULL
data2$sick <- NULL
data2$pregnant.measured <- NULL
data2$pregnant <- NULL
data2$thyroid.surgery.measured <- NULL
data2$thyroid.surgery <- NULL
data2$I131.treatment.measured <- NULL
data2$I131.treatment <- NULL
data2$query.hypothyroid.measured <- NULL
data2$query.hypothyroid <- NULL
data2$query.hyperthyroid.measured <- NULL
data2$query.hyperthyroid <- NULL
data2$lithium.measured <- NULL
data2$lithium <- NULL
data2$goitre.measured <- NULL
data2$goitre <- NULL
data2$tumor.measured <- NULL
data2$tumor <- NULL
data2$hypopituitary.measured <- NULL
data2$hypopituitary <- NULL
data2$psych.measured <- NULL
data2$psych <- NULL

#Delete measuring variables

data2$TSH.measured <- NULL
data2$T3.measured <- NULL
data2$TT4.measured <- NULL
data2$T4U.measured <- NULL
data2$FTI.measured <- NULL
#Delete referral.source variable
data2$referral.source <- NULL
#Delete clasification variable
data2$clasification <- NULL

#Data format transform
#Nominal variables

#Continuous variables
data2$age <- as.numeric(data2$age)
data2$TSH <- as.numeric(data2$TSH)
data2$T3 <- as.numeric(data2$T3)
data2$TT4 <- as.numeric(data2$TT4)
data2$T4U <- as.numeric(data2$T4U)
data2$FTI <- as.numeric(data2$FTI)

# Data selection - set max value for detection of data anomalies
TSHMAX <- 4.0 + abs(0.4 - 4.0) * 2
T3MAX <-  3.37 + abs(1.07 - 3.37) * 2
TT4MAX <- 164 + abs(64 - 164) * 2
T4UMAX <- 1.8 + abs(0.7 - 1.8) * 2
FTIMAX <- 1.8 + abs(33.108 - 135.191) * 2
data2 <- subset(data2, (age <= 100) & (TSH <= TSHMAX) & (T3 <= T3MAX) & (TT4 <= TT4MAX) & (T4U <= T4UMAX) & (FTI <= FTIMAX))


#Calculating Dissimilarity with Gower Distance
data_distEuclidiana <- daisy(data2, metric = "euclidean")

#Calculating optimal number of groups with Silhouette width
sil <- c()
for (i in 2:15){
  fit <- pam(data_distEuclidiana, diss = TRUE, k = i)
  sil[i] <- fit$silinfo$avg.width
}

clust_num_plot <- ggplot(data.frame(clust_num = 2:15, sil_width = sil[2:15]), aes(x = clust_num, y = sil_width))+labs(x = "Number of Clusters", y = "Silhouette Width")+geom_line(color = "blue")+geom_point(color = "blue")+ggtitle("Number of Clusters vs Silhouette Width")+theme_minimal()+theme(plot.title = element_text(hjust = 0.5))

#Clustering [k = 3]
data_cluster <- pam(data_distEuclidiana, diss = TRUE, k = 3)

#t-SNE plot
tsne <- Rtsne(data_distEuclidiana, is_distance = TRUE)
plot_groups <- ggplot(data.frame(tsne$Y), aes(x = X1, y = X2))+labs(x = "X", y = "Y")+geom_point(color = factor(data_cluster$clustering))

#Adding a new column with the cluster number
data2["cluster"] <- data_cluster$clustering

