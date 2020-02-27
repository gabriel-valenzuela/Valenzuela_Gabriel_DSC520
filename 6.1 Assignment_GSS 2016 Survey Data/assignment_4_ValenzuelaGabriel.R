
# Assignment: ASSIGNMENT 4

# Name: Valenzuela, Gabriel

# Date: 19 01 2020

GSSSurvey <- read.csv("gss-2016.csv")
library(ggplot2)
# Part 1

# a 
children <-GSSSurvey$CHILDS
siblings <- GSSSurvey$SIBS
ggplot(GSSSurvey, aes(x=siblings,y=children)) + geom_point() + geom_smooth(method = "lm")

# Based on the regression line, it seems that the more siblings a respondent had
# the more children that respondent tended to have at the time. It looks like it 
# grew slightly then begin to not rise as fast. 

# b 
cov(siblings, children, use = "pairwise.complete.obs")

# Since they are missing values in the data, which was concluded by "NA" appearning on
# a normal cov(), it can be speculated based on the result of 1.064853 that there 
# is a positive relationship between these two variables

# c
# I chose the pearson method because it is primarily used as a primary check for the
# relationship between two variables. The coefficient of correlation is a measure of
# the strength of the linear relationship between two variables. The test will yeild a
# positive correlation. 

cor.test(siblings, children, method = "pearson")

# d
cor(GSSSurvey[,c("SIBS","CHILDS")], use = "pairwise.complete.obs")

# With looking at the correlation matrix, these two variables have a
# a very weak posivitve relation to one another. The value is barely
# abover zero between SIBS and CHILDS. 

# e
family.lm <- lm(siblings ~ children, data = GSSSurvey)
#correlation coefficient
summary(family.lm)


#correlation determination
summary(family.lm)$r.squared

# The relationship between these two variables have a weak 
# and positive linear relationship

# f
# Thus far, I would say that there is a relationship between these two variables, but
# at the same time, it is not a very strong relationship for both of them. 

# g



# h
ppcor::pcor.test(siblings,children,GSSSurvey$SEX, method = "pearson")




# Part 2

# a
library(datasets)
model <- lm(formula = siblings~children, data = GSSSurvey, na.action = na.exclude)
model

# b
summary(model)
# The intercept is 3.0080 and the slope would be 0.3818.
# Coefficient of determination: 0.03954
# Correlation coefficient: 0.19885


# c
# Based on this model, we can explain that the variation in the number of children
# that someone has due to siblings is 3.95%. Hence, the amount of variation not
# explained by the number of siblings is 96.05%.


# d
# For the calculated F-Ratio, this model does have a better prediction of
# the number of children than if someone had the mean value of siblings


# e
# outcome = (model) + error
# outcome = 3.00804 + (0.38179 * 3)
threeKids <- 3.00804 + (0.38179 * 3)
threeKids
# Using the prediction model, the predicted number of children would be 4

# f
# outcome = (model) + error
# outcome = 3.00804 + (0.38179 * 0)
zeroKids <- 3.00804 + (0.38179 * 0)
zeroKids
# Using the prediction model, the predicted number of children would be 3


