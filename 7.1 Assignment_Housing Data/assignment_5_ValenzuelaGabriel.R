# Assignment: ASSIGNMENT 5

# Name: Valenzuela, Gabriel

# Date: 26 01 2020
library(readxl)
library(QuantPsyc)
library(lm.beta)

housing_df <- read_excel("~/Bellevue University/Statistics in R/Assignment 5 DSC 520/week-7-housing.xlsx")

# B)

SalePrice_SquareFootofLot_model <- lm(housing_df$`Sale Price`~housing_df$sq_ft_lot,data = housing_df)
SalePrice_Bedrooms_BathFullCount_model <- lm(housing_df$`Sale Price`~housing_df$sq_ft_lot+housing_df$bedrooms+housing_df$bath_full_count,data = housing_df)

# C)
summary(SalePrice_SquareFootofLot_model)
summary(SalePrice_Bedrooms_BathFullCount_model)

# Sales Price and Square Foot of Lot model: R2 = 0.01435 Adjusted R2 = 0.01428
# Since the results are closer to zero, this does not represent a well-fitting model.

# Sales Price and Square Food of Lot + Bedrooms + Bath Full Count Model: R2 = 0.1127 Adjusted R2: 0.1125
# Since the results are still very close to zero even with including the other variables, it still does not represent a well-fitting model.

# The inclusion of these additional predictors did not aid in explaining the large variations
# found in Sale Price.

# D)
lm.beta(SalePrice_Bedrooms_BathFullCount_model)

# Square Foot of Lot: 0.1017484
# Bedrooms: 0.1492025
# Bath Full Count: 0.2346207

# With these values, it indicates that a change in one standard deviation will have these impacts
# on these certain variables. For example, one standard deviation has a bigger impact on bath full count
# than compared to square foot of lot. 

# E)
predict(SalePrice_Bedrooms_BathFullCount_model,interval = "confidence")

# F)
aov_test_model1 <-aov(formula = housing_df$`Sale Price`~housing_df$sq_ft_lot,data = housing_df)
aov_test_model2 <- aov(formula = housing_df$`Sale Price`~housing_df$sq_ft_lot+housing_df$bedrooms+housing_df$bath_full_count,data = housing_df)
summary(aov_test_model1)
summary(aov_test_model2)

# After performing an analysis of variance on both of the models, it does not seem that the means 
# are the same. Hence, it can be speculated that there is a significant relationship between the sales
# price and the other factors such as square foot of lot, bedrooms, and numbe of complete bathrooms. 

# G)
sq_foot_lot_outliers <- boxplot.stats(housing_df$sq_ft_lot)$out
bedroom_outliers <- boxplot.stats(housing_df$bedrooms)$out
bath_full_count_outliers <- boxplot.stats(housing_df$bath_full_count)$out

# H)
housing_stdres_list <- vector(mode = "list", length = 0)
housing_stdres_large <- vector(mode = "list", length = 0)
housing_stdres_list <- rstandard(SalePrice_Bedrooms_BathFullCount_model)
housing_stdres_df <- rstandard(SalePrice_Bedrooms_BathFullCount_model)
housing_stdres_df
rstandard(SalePrice_Bedrooms_BathFullCount_model)
for(i in 1:length(housing_stdres_list)){
  if(housing_stdres_list[i] < -2){
    housing_stdres_large[i] <- housing_stdres_list[i]
  }
  else{
    housing_stdres_large[i] <- 0
  }
  
}

# I)
sum_large_stdres <- Reduce("+",housing_stdres_large)
sum_large_stdres

# J)
residuals_housing <- residuals(SalePrice_Bedrooms_BathFullCount_model)

# K)
# Leverage
library(influence.ME)
plot(hatvalues(SalePrice_Bedrooms_BathFullCount_model),type="h")
# Cooks distance
plot(cooks.distance(SalePrice_Bedrooms_BathFullCount_model),type="h")

# Covariance Ratio
covratio(SalePrice_Bedrooms_BathFullCount_model)


# L)

chisq.test(housing_df$`Sale Price`,housing_df$sq_ft_lot, correct = FALSE)
chisq.test(housing_df$`Sale Price`,housing_df$bedrooms, correct = FALSE)
chisq.test(housing_df$`Sale Price`,housing_df$bath_full_count, correct = FALSE)
# Since the p-values of the tests is less than the significane level of 0.05,
# the variables are in fact dependent on each other

# M)
test_df <- data.frame(housing_df$`Sale Price`,housing_df$sq_ft_lot,housing_df$bedrooms,housing_df$bath_full_count)
multicollinearity <- cor(test_df)
round(multicollinearity,2)

# Since the magnitude of the correlation coefficients are less than .80, the condition is being met.


# N)



# O)

# I beleive that this regression model is biased since it does not include all of the factors and is only
# focusing on the the few variables that may possibly have an influence on the sales prices. 




