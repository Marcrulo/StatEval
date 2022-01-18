# Loading the library
library(rattle.data)
# Loading the wine data
data(wine)

# Checking the structure of wine dataset
str(wine)




library(dplyr)
# Using sample_frac to create 70 - 30 slipt into test and train
train <- sample_frac(wine, 0.7)
sample_id <- as.numeric(rownames(train)) # rownames() returns character so as.numeric
test <- wine[-sample_id,]

# Setting the basline 
train$Type <- relevel(train$Type, ref = "3")

require(nnet)
# Training the multinomial model
multinom.fit <- multinom(Type ~ Alcohol + Color -1, data = train)

# Checking the model
summary(multinom.fit)

## extracting coefficients from the model and exponentiate
exp(coef(multinom.fit))

head(probability.table <- fitted(multinom.fit))


# Predicting the values for train dataset
train$precticed <- predict(multinom.fit, newdata = train, "class")

# Building classification table
ctable <- table(train$Type, train$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)


# Predicting the values for test dataset
test$precticed <- predict(multinom.fit, newdata = test, "class")

# Building classification table
ctable <- table(test$Type, test$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)






