# Assignment: ASSIGNMENT 3

# Name: Valenzuela, Gabriel

# Date: 15 12 2019

library(ggplot2)
library(pastecs)
library(psych)
library(moments)
survey <- read.csv("acs-14-1yr-s0201.csv")

# Task 1 
# Variables:
#   Id: Quantitative
#   Id2: Quantitative
#   Geography: Categorical
#   PopGroupID: Quantative
#   POPGROUP.display-label: Categorical 
#   RacesReported:Quantitative
#   HSDegree: Quantitative
#   BachDegree: Quantitative

# Task 2
str(survey)
nrow(survey)
ncol(survey)

# Task 3
histo <- ggplot(survey, aes(x = HSDegree)) + geom_histogram(aes(y = ..density..),binwidth = 2) + ggtitle("High School Degrees") + xlab("Degrees") + ylab("Number of Degrees")
histo
# Task 4 - a.
#   The data distribution is unimodal in the histogram.
#
# Task 4 - b.
avg <- mean(survey$HSDegree)
med <- median(survey$HSDegree)
avg
med
#  The histogram is not symmetrical.
#
# Task 4 - c. 
#   The histogram is not bell-shaped. 

# Task 4 - d.
#   The histogram is normal. 

# Task 4 - e.
#   The histogram is approximately normal so it is not skewed 

# Task 4 - f.
histo <- histo + stat_function(fun = dnorm, color = "red", args=list(mean=mean(survey$HSDegree, na.rm = TRUE), sd=sd(survey$HSDegree, na.rm = TRUE)))
histo

# Task 4 - g.
#   A normal distribution can accurately be used as a model for this data because it shows the distribution of the data as it increases and decreases.  

# Task 5
probplt <-qqnorm(survey$HSDegree)
probline <- qqline(survey$HSDegree)
probline
probplt

# Task 6 - a.
#   The distribution is approximately normal because of the closeness to the dtribution line that is within the plot that was created. If more
#   points were further from the line, it won't be a normal distribution. 
#
# Task 6 - b.
#   N/A, the distribution is normal. 
#
# Task 7
quan_norm <- stat.desc(survey)
quan_norm
#
# Task 8
#    When it comes to the results based on skew, kurtosis, and z-scores, the histogram is only slightly skewed and to the right since it is positive.  
#    Because of the kurtosis, it can also be said that the distribution of the data is mesokuritc. For the z-scores, they show that the std deviation and the mean
#    are not that far apart. If the sample size were larger, it would possibly yield different results since certain
#    counties were selected for the survey. 
#
#