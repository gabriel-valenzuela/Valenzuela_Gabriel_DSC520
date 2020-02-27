
# Assignment: ASSIGNMENT 3

# Name: Valenzuela, Gabriel

# Date: 08 01 2020

studentSurvery <- read.csv("student-survey.csv")

# a
ReadingTime <- studentSurvery$TimeReading
TVTime <- studentSurvery$TimeTV
Gender <- studentSurvery$Gender
Happiness <- studentSurvery$Happiness

cov(ReadingTime, TVTime)
# I chose to do this calculation because will it give insight on if there is a linear
# relationship between the two survey observation variables. Based on the result of
# -20.36364, it indicates that there is a negative linear relationship between these
# two variables. 

# b
# The measurement being used for the survery data variables are time for Reading and TV
# , a scale of 0 - 100 for happiness, and a 1 or 0 to present either Male or Female.
# When it comes to changing the measurement for the variables, I think it depends on
# what the change of measurement would be and if it were done across all of the variables
# . If a measurement were to change, it could lead to the relationship either being
# stronger or weaker. 

# c
# I chose the pearson method because it is primarily used as a primary check for the
# relationship between two variables. The coefficient of correlation is a measure of
# the strength of the linear relationship between two variables. The test will yeild a
# negative correlation. 

# d - 1
cor(studentSurvery)

# d - 2
cor.test(ReadingTime, TVTime, method = "pearson")

# d- 3
cor.test(ReadingTime, TVTime, method = "pearson", conf.level = 0.99)

# d- 4
# Well first off, it makes sense that the correlation between the same variable on the 
# row and column are at 1.00. When it comes to TimeReading, it seems that there is a 
# negative correlation with each variable. With TimeTV and Happiness, there seems to 
# be a positive correlation between those two. Now, with TimeTV and Gender, it is a 
# very slim next to nothing correlation between each other. Lastly, between Gender
# and Happiness, it seems that there is a positive correlation but not a large one. 

# e
time.lm <- lm(ReadingTime ~ TVTime, data = studentSurvery)
#correlation coefficient
summary(time.lm)


#correlation determination
summary(time.lm)$r.squared

# The relationship between these two variables have a strong fit 
# and positive linear relationship

# f
# When determining whether watching more TV caused students to read less, I would 
# speculate that to be true seeing that there is a strong negative correlation 
# between the two resulting in more time spent watching tv will result in less time
# spent reading for a student. 

# g
cor.test(TVTime,Happiness, method = "pearson")
ppcor::pcor.test(TVTime,Happiness,Gender, method = "pearson")
# With using the partical correlation test and controlling Gender, it seems that
# there was not much of an effect or change when controlling Gender within the test.
# By looking at the results, it further pushes the hypothesis that Happiness is
# higher with higher TV Time but that Gender does not have an effect on the relationship. 







