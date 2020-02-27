# Assignment: ASSIGNMENT 2

# Name: Valenzuela, Gabriel

# Date: 08 12 2019

sections <- read.csv("scores.csv")
 

# Task 1

# Observarional Units: the students in the section of the same course

# Task 2

# Variables: 
#   Count - Quantitative
#   Score - Quantitative
#   Sections - Categorical

# Task 3 

sections_regular <- sections[sections$Section == 'Regular',]
sections_regular
sections_sports <- sections[sections$Section == 'Sports',]
sections_sports

# Task 4

plot(sections_regular$Score, sections_regular$Count, main = "Regular Section" ,xlab = "Scores of Students", ylab = "Number of Students")
plot(sections_sports$Score,  sections_regular$Count, main = "Sports Section" ,xlab = "Scores of Students", ylab = "Number of Students")


# Task 4 - a.
# With the two sections, I would say that the Regular Section tended to score more points than the Sports Section.
# Looking at the scores of the sections, the Regular Section had a higher average score and median than the Sports Section. 
# The Sports section also had more students scoring less than 300 compared to the Regular Section.

regular_avg <- mean(sections_regular$Score)
regular_med <- median(sections_regular$Score)
sports_avg <- mean(sections_sports$Score)
sports_med <- median(sections_sports$Score)
regular_avg
regular_med
sports_avg
sports_med

# Task 4 - b.
# No, a student did not score more points than every other student in another section.
# In this context, the distribution of the points are asymmetric.

# Task 4 - c.
# A variable that could affect the scores are the times of the section.
