##CLUSTER ANALYSIS

##################
####example1######
##################

##data
##url import

data1 <- read.table("http://analisisydecision.es/wp-content/uploads/2009/06/alimentos2.txt", header=FALSE, sep="\t")
data_names <- c("name", "inter_hidratos", "kcal", "proteins", "fats")
names(data1) <- data_names
View(data1)

str(data1)

##distance matrix
dist1 <- dist(data1, method = "manhattan")
cluster1 <- hclust(dist1)

dist2 <- dist(data1, method = "euclidean")
cluster2 <- hclust(dist2)

dist3 <- dist(data1, method = "maximum")
cluster3 <- hclust(dist3)

dist4 <- dist(data1, method = "canberra")
cluster4 <- hclust(dist4)

##graphs
op <- par(mfcol = c(1,1))
par(las = 1)
plot(cluster1, main = "Manhattan method")
plot(cluster2, main = "Euclidean method")
plot(cluster3, main = "Maximum method")
plot(cluster4, main = "Canberra method")

##PAM: Partitioning Around Medoids
library(cluster)
step1 <- pam(dist2, 2)
step2 <- pam(dist2, 3)
step3 <- pam(dist2, 4)
step4 <- pam(dist2, 5)

par(mfrow = c(1,1))
plot(step1)
plot(step2)
plot(step3)
plot(step4)

cluster.final <- kmeans(dist2, 3)
cluster.final$size

cluster.final <- kmeans(dist2, 4)
cluster.final$size

cluster.final <- kmeans(dist2, 5)
cluster.final$size

##choose 4 groups
cluster.final <- kmeans(dist2, 4)
cluster.final$size
groups <- data.frame(data1)
clus <- as.factor(cluster.final$cluster)
groups <- cbind(data.frame(data1), clus)

##ordering dataframe
install.packages("reshape")
library(reshape)

groups <- sort_df(groups, vars='clus')
groups

##analysis between clusters
aggregate(groups$inter_hidratos, list(groups$clus), mean)
aggregate(groups$kcal, list(groups$clus), mean)
aggregate(groups$proteins, list(groups$clus), mean)
aggregate(groups$fats, list(groups$clus), mean)


##################
####example2######
##################
library(tidyverse) #data manipulation
library(cluster) #clustering algorithms
library(factoextra) #clustering algorithms & visualization 

##analisys in US arrets statistics
df <- USArrests

##remove missing values
df <- na.omit(df)

##scaling data
df <- data.frame(scale(df))
head(df)
str(df)
##calculating distances: how similar my values are?
##distance methods euclidean and manhattan
dist1 <- dist(df, method = "manhattan")
cluster <- hclust(dist1)

plot(cluster, main = "Euclidean method")

##k-means clustering
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
k2

##graphic results
df %>% 
    #tibble() %>%
    mutate(cluster=k2$cluster,
         state=row.names(USArrests)) %>%
ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
geom_text()

