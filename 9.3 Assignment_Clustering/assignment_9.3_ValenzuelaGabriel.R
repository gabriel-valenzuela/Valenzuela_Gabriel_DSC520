# Assignment: ASSIGNMENT 9.3

# Name: Valenzuela, Gabriel

# Date: 05 02 2020

## Answers


cluster_data <- read.csv("clustering-data.csv")


# A)

ggplot2::ggplot(cluster_data,ggplot2::aes(x=x,y=y)) + ggplot2::geom_point()


# B)

set.seed(123)
clusters2 <- kmeans(cluster_data,2)
clusters3 <- kmeans(cluster_data,3)
clusters4 <- kmeans(cluster_data,4)
clusters5 <- kmeans(cluster_data,5)
clusters6 <- kmeans(cluster_data,6)
clusters7 <- kmeans(cluster_data,7)
clusters8 <- kmeans(cluster_data,8)
clusters9 <- kmeans(cluster_data,9)
clusters10 <- kmeans(cluster_data,10)
clusters11<- kmeans(cluster_data,11)
clusters12 <- kmeans(cluster_data,12)

p2 <- factoextra::fviz_cluster(clusters2,geom = "point",data = cluster_data) + ggplot2::ggtitle("k=2")
p3 <- factoextra::fviz_cluster(clusters3,geom = "point",data = cluster_data) + ggplot2::ggtitle("k=3")
p4 <- factoextra::fviz_cluster(clusters4,geom = "point",data = cluster_data) + ggplot2::ggtitle("k=4")
p5 <- factoextra::fviz_cluster(clusters5,geom = "point",data = cluster_data) + ggplot2::ggtitle("k=5")
p6 <- factoextra::fviz_cluster(clusters6,geom = "point",data = cluster_data) + ggplot2::ggtitle("k=6")
p7 <- factoextra::fviz_cluster(clusters7,geom = "point",data = cluster_data) + ggplot2::ggtitle("k=7")
p8 <- factoextra::fviz_cluster(clusters8,geom = "point",data = cluster_data) + ggplot2::ggtitle("k=8")
p9 <- factoextra::fviz_cluster(clusters9,geom = "point",data = cluster_data) + ggplot2::ggtitle("k=9")
p10 <- factoextra::fviz_cluster(clusters10,geom = "point",data = cluster_data) + ggplot2::ggtitle("k=10")
p11 <- factoextra::fviz_cluster(clusters11,geom = "point",data = cluster_data) + ggplot2::ggtitle("k=11")
p12 <- factoextra::fviz_cluster(clusters12,geom = "point",data = cluster_data) + ggplot2::ggtitle("k=12")

p2
p3
p4
p5
p6
p7
p8
p9
p10
p11
p12

# C)

set.seed(123)

wss <- function(k) {
  kmeans(cluster_data,k,nstart = 10)$tot.withinss
  
}

k.values <- 2:12

wss_values <- purrr::map_dbl(k.values,wss)

plot(k.values,wss_values,type = "b", pch = 19,frame = FALSE,xlab = "K Values", ylab = "Average Distance")


# D)

#  Looking at the graph above in determining the elbow point, I would say that it is at about the 5th K Value. 




















