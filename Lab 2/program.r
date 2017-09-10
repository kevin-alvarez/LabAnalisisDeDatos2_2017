library("cluster")

rawdata <- read.csv("allhypo_filtered.csv", header = TRUE, sep = ",")
groups <- 6
cluster <- pam(rawdata, groups, metric = "euclidean")
