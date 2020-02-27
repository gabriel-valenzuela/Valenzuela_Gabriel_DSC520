# Assignment: ASSIGNMENT 6.2

# Name: Valenzuela, Gabriel

# Date: 30 01 2020

## Answers

binary <- read.csv("binary-classifier-data.csv")
summary(binary)
# A)
lrfit <- glm(label ~ x + y,data = binary, family = "binomial")
fitted.results <- predict(lrfit,newdata = binary,type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != binary$label)
print(paste('Accuracy',1-misClasificError))

# B)
# KNN Algorithm - K Nearest Neighbor Algorithm
set.seed(123)
dat.d <- sample(1:nrow(binary),size = nrow(binary)*0.7,replace = FALSE) # random selection of 70% of data

train.binary <- binary[dat.d,] # 70% used for training
test.binary <- binary[-dat.d,] # 30% used for testing

train.binary_labels <- binary[dat.d,1]
test.binary_labels <- binary[-dat.d,1]

NROW(train.binary_labels)

knn.32 <- knn(train = train.binary,test = test.binary,cl = train.binary_labels, k=32)
knn.33 <- knn(train = train.binary,test = test.binary,cl = train.binary_labels, k=33)

ACC.32 <- 100 * sum(test.binary_labels == knn.32)/NROW(test.binary_labels)
ACC.33 <- 100 * sum(test.binary_labels == knn.33)/NROW(test.binary_labels)

ACC.32
ACC.33


caret::confusionMatrix(table(knn.32, test.binary_labels))

# The accuracy of the logistic regression classifier are different accuracies compared to one another


# C)

# The accuracy is different because of the KNN algorithm is learning based off of testing data while the 
# the logisitic regression is using the predictor variables along with the model to make the predicition. 