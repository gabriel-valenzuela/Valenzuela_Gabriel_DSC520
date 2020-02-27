# Assignment: ASSIGNMENT 9.2

# Name: Valenzuela, Gabriel

# Date: 05 02 2020

## Answers
library(class)

binary <- read.csv("binary-classifier-data.csv")
trinary <- read.csv("trinary-classifier-data.csv")
summary(binary)
summary(trinary)

# A)
binary$label <- as.factor(binary$label)
ggplot2::ggplot(binary,ggplot2::aes(x=x,y=y,shape = label, color = label)) + ggplot2::geom_point()


trinary$label <- as.factor(trinary$label)
ggplot2::ggplot(trinary,ggplot2::aes(x=x,y=y,shape = label, color = label)) + ggplot2::geom_point()


# B)

dist(rbind(binary$x,binary$y))

dist(rbind(trinary$x,trinary$y))

# Binary
set.seed(123)
dat.d <- sample(1:nrow(binary),size = nrow(binary)*0.7,replace = FALSE) # random selection of 70% of data

train.binary <- binary[dat.d,] # 70% used for training
test.binary <- binary[-dat.d,] # 30% used for testing

train.binary_labels <- binary[dat.d,1]
test.binary_labels <- binary[-dat.d,1]

knn.3 <- knn(train = train.binary,test = test.binary,cl = train.binary_labels, k=3)
knn.5 <- knn(train = train.binary,test = test.binary,cl = train.binary_labels, k=5)
knn.10 <- knn(train = train.binary,test = test.binary,cl = train.binary_labels, k=10)
knn.15 <- knn(train = train.binary,test = test.binary,cl = train.binary_labels, k=15)
knn.20 <- knn(train = train.binary,test = test.binary,cl = train.binary_labels, k=20)
knn.25 <- knn(train = train.binary,test = test.binary,cl = train.binary_labels, k=25)

ACC.3 <- 100 * sum(test.binary_labels == knn.3)/NROW(test.binary_labels)
ACC.5 <- 100 * sum(test.binary_labels == knn.5)/NROW(test.binary_labels)
ACC.10 <- 100 * sum(test.binary_labels == knn.10)/NROW(test.binary_labels)
ACC.15 <- 100 * sum(test.binary_labels == knn.15)/NROW(test.binary_labels)
ACC.20 <- 100 * sum(test.binary_labels == knn.20)/NROW(test.binary_labels)
ACC.25 <- 100 * sum(test.binary_labels == knn.25)/NROW(test.binary_labels)

ACC.3
ACC.5
ACC.10
ACC.15
ACC.20
ACC.25



# Trinary
set.seed(123)
dat.d <- sample(1:nrow(trinary),size = nrow(trinary)*0.7,replace = FALSE) # random selection of 70% of data

train.trinary <- trinary[dat.d,] # 70% used for training
test.trinary <- trinary[-dat.d,] # 30% used for testing

train.trinary_labels <- trinary[dat.d,1]
test.trinary_labels <- trinary[-dat.d,1]

knn.t3 <- knn(train = train.trinary,test = test.trinary,cl = train.trinary_labels, k=3)
knn.t5 <- knn(train = train.trinary,test = test.trinary,cl = train.trinary_labels, k=5)
knn.t10 <- knn(train = train.trinary,test = test.trinary,cl = train.trinary_labels, k=10)
knn.t15 <- knn(train = train.trinary,test = test.trinary,cl = train.trinary_labels, k=15)
knn.t20 <- knn(train = train.trinary,test = test.trinary,cl = train.trinary_labels, k=20)
knn.t25 <- knn(train = train.trinary,test = test.trinary,cl = train.trinary_labels, k=25)

ACC.t3 <- 100 * sum(test.trinary_labels == knn.t3)/NROW(test.trinary_labels)
ACC.t5 <- 100 * sum(test.trinary_labels == knn.t5)/NROW(test.trinary_labels)
ACC.t10 <- 100 * sum(test.trinary_labels == knn.t10)/NROW(test.trinary_labels)
ACC.t15 <- 100 * sum(test.trinary_labels == knn.t15)/NROW(test.trinary_labels)
ACC.t20 <- 100 * sum(test.trinary_labels == knn.t20)/NROW(test.trinary_labels)
ACC.t25 <- 100 * sum(test.trinary_labels == knn.t25)/NROW(test.trinary_labels)

ACC.t3
ACC.t5
ACC.t10
ACC.t15
ACC.t20
ACC.t25

# K)

binary_graph_df <- data.frame(x = c(3,5,10,15,20,25),y = c(ACC.3,ACC.5,ACC.10,ACC.15,ACC.20,ACC.25))
ggplot2::ggplot(binary_graph_df,ggplot2::aes(x=x,y=y)) + ggplot2::geom_point()

trinary_graph_df <- data.frame(x = c(3,5,10,15,20,25),y = c(ACC.t3,ACC.t5,ACC.t10,ACC.t15,ACC.t20,ACC.t25))
ggplot2::ggplot(trinary_graph_df,ggplot2::aes(x=x,y=y)) + ggplot2::geom_point()


# C)

# When it comes to these two data sets, I do believe that a linear classifier will work for them.


