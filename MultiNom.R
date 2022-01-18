rm(list=ls())
graphics.off()
library(cvTools)
library(dplyr)
library(nnet)

# Load data
load('armdata.RData')

### Fit a smoothing spline 
for (i in 1:16) {
  for (j in 1:10) {
    for (k in 1:10) {
      for (n in 1:3) {
        x<-seq(1,100)
        y<-armdata[[i]][[j]][[k]][,n]
        
        dat <- structure(list(x = x, y = y),
                         .Names = c("x", "y"), row.names = c(NA, 100L), class = "data.frame")
        smoo <- with(dat[!is.na(dat$y),],smooth.spline(x,y))
        result <- with(dat,predict(smoo,x[is.na(y)]))
        dat[is.na(dat$y),] <- result
        
        armdata[[i]][[j]][[k]][,n] =dat[,"y"]
      }
    }
  }
}
##### Data
wine = matrix(NA,1600,301)
for (e in 1:16) { # experiment
  for (p in 1:10) { # person
    for (r in 1:10){ # rep
      for (c in 1:3){ # x/y/z
        for (x in 1:100){ # step
          wine[(e-1)*100 + (p-1)*10 + r,(c-1)*100+x]=armdata[[e]][[p]][[r]][x,c] # M[row,column]
          wine[(e-1)*100 + (p-1)*10 + r, 301] = e
        }
      }
    }
  }
}

############ MLR
wine <- as.data.frame(wine)

# Using sample_frac to create 70 - 30 slipt into test and train
#train <- sample_frac(wine, 0.7)
#sample_id <- as.numeric(rownames(train)) # rownames() returns character so as.numeric
#test <- wine[-sample_id,]
set.seed(222)
sample_size = round(nrow(wine)*.70) # setting what is 70%
index <- sample(seq_len(nrow(wine)), size = sample_size)
train <- wine[index, ]
test <- wine[-index, ]


### First model, inspired by the ML scripts. It achieves an accuracy of app. 
### 8%. 

attributeNames = as.vector(colnames(wine))[1:300]
classNames <- as.vector(1:16)

X_train <- train[1:300]
N_train <- dim(train)[1]
y_train<- train[301][,1]

X_test <- test[1:300]
N_test <- dim(test)[1]
y_test <- test[301][,1]

X_traindf <- data.frame(X_train)
X_testdf <- data.frame(X_test)


Y_train=factor(y_train)
Y_test=factor(y_test)

(fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse= "+"))))
model <- multinom(formula=fmla, data=X_traindf, MaxNWts =10000000, maxit=150)

## Compute results on test data
# Get the predicted output for the test data
Y_test_est = predict(object=model, newdata=X_testdf, type='probs')

# Compute the class index by finding the class with highest probability from the multinomial regression model
y_ <- apply(Y_test_est, 1, max)
y_test_est <- apply(Y_test_est, 1, which.max)
# Subtract one to have y_test_est between 0 and C-1
y_test_est = y_test_est-1

# Compute error rate
ErrorRate = sum(y_test!=y_test_est)/N_test;
print(paste('Error rate: ', ErrorRate*100, '%', sep=''))






### Let's see what the optimal value for number of iterations is: 
### Optimizing without k-folkd cross validation. 

Iterations = as.vector(1:25 * 10)
ErrorRates = as.vector(1:25 * 0)

for (i in Iterations) {
  model <- multinom(formula=fmla, data=X_traindf, MaxNWts =10000000, maxit=i)
  
  ## Compute results on test data
  # Get the predicted output for the test data
  Y_test_est = predict(object=model, newdata=X_testdf, type='probs')
  
  # Compute the class index by finding the class with highest probability from the multinomial regression model
  y_ <- apply(Y_test_est, 1, max)
  y_test_est <- apply(Y_test_est, 1, which.max)
  # Subtract one to have y_test_est between 0 and C-1
  y_test_est = y_test_est-1
  
  # Compute error rate
  ErrorRate = sum(y_test!=y_test_est)/N_test;
  ErrorRates[i/10] = ErrorRate*100
  print(paste('Error rate: ', ErrorRate*100, '%', sep=''))
}

print(min(ErrorRates))
plot(x = c(1:25) * 10, ErrorRates, main='Error rate', xlab='Number of iterations', ylab='Classification error rate (%)', pch=20, type='l')

### Conclusion: It's seems as though less is better. The optimal value seems to be
### 20. However, this conclusion is extremely unreliable, since no k-fold cross validation
### method is used. 





### The following aims to find the optimal value for the number of iterations 
### using 16-fold cross validation and iterations values 10, 20, 30, ..., 250.

X <- wine[1:300]
y <- wine[301][,1]
N = dim(wine)[1]
attributeNames = as.vector(colnames(wine))[1:300]

# Leave-one-out cross validation
CV <- cvFolds(N, K=16)
#K = N
K = 16

# Number of iterations
Iterations = as.vector(1:25 * 10)
L = 25; # Length of the iteration Matrix

# Variable for classification error
Error = array(rep(NA, times=K*L), dim=c(K,L))

### multinom.fit <- multinom(V301 ~ ., data = train, MaxNWts=20000, maxit=1000)
### Conclusion, the model seems to converge after app. 250 iterations. 

for(k in 1:K){ # For each cross validation fold
  print(paste('Crossvalidation fold ', k, '/', CV$NumTestSets, sep=''))
  
  # Extract training and test set
  X_train <- X[CV$subsets[CV$which!=k], ];
  y_train <- y[CV$subsets[CV$which!=k]];
  X_test <- X[CV$subsets[CV$which==k], ];
  y_test <- y[CV$subsets[CV$which==k]];
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  X_testdf <- data.frame(X_test)
  colnames(X_testdf) <- attributeNames
  X_traindf <- data.frame(X_train)
  colnames(X_traindf) <- attributeNames
  
  for(i in Iterations){ # For each number of iterations
    
    # Use MNR to trained for i iterations
    model <- multinom(formula=y_train ~ ., data=X_traindf, MaxNWts =10000000, maxit=i)
    
    ## Compute results on test data
    # Get the predicted output for the test data
    #y_test_est = predict(object=model, newdata=X_testdf, type='probs')
    # Compute number of classification errors
    #Error[k,i/10] = sum(y_test!=y_test_est); # Count the number of errors
    print(paste("We are at position ", k, ':', i/10, "."))
    
    
    
    # Predicting the values for test dataset
    y_test_est <- predict(model, newdata = X_test, "class")
    # Building classification table
    #ctable <- table(y_test, y_test_est)
    # Calculating accuracy - sum of diagonal elements divided by total obs
    #acc = round((sum(diag(ctable))/sum(ctable))*100,2)
    acc = sum(y_test_est==y_test)/length(y_test)
    Error[k,i/10] = sum(y_test!=y_test_est); # Count the number of errors
    print(paste("The model achieved an accuracy of ", acc))
  }
}

plot(colSums(Error)/16, main='Error rate', xlab='Number of iterations', ylab='Classification error rate (%)', pch=20, type='l')
#write.table(Error, 'MultiNorm_Iter_Error.txt')
### Conclusion: The greater the number of iterations, the better. It seems the 
### optimal value lies at 200 iterations. 



























