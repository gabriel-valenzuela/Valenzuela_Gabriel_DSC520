# Assignment: ASSIGNMENT 6

# Name: Valenzuela, Gabriel

# Date: 30 01 2020


thoracic_surgery <- read.csv("csv_result-ThoraricSurgery.csv")


# A)


lrfit <- glm(Risk1Yr ~ DGN + PRE4+ PRE5+ PRE6+ PRE7
             + PRE8+ PRE9+ PRE10+ PRE11+ PRE14
             + PRE17+ PRE19+ PRE25+ PRE30+ PRE32+ AGE,data = thoracic_surgery, family = "binomial")
lrfit
summary(lrfit)



# B)

# According to the summary, the variables PRE9(Dyspnoea Before Surgery), PRE14(Size of Tumor), PRE17(Type 2 Diabetes), 
# and PRE30(Smoking) had the greatest effect on survival rate. 


# C)
fitted.results <- predict(lrfit,newdata = thoracic_surgery,type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != thoracic_surgery$Risk1Yr)
print(paste('Accuracy',1-misClasificError))

# The accuracy of the model is 0.83

