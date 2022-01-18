##### First model
### This model is the simpler model. It seems to work, but the results are bizzare
### good luck fixing it if you can;)

library(FNN)
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
library(dplyr)

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

K = 200; # Number of neighbors

# Use knn to classify to find the K nearest neighbors
X_train <- data.frame(train[1:300])
X_test <- data.frame(test[1:300])
y_train <- data.frame(train[, -1])
y_train = y_train[,1]
y_test <- data.frame(test[301])
y_test = y_test[,1]


X_testdf <- data.frame(X_test)
colnames(X_testdf) <- attributeNames
X_traindf <- data.frame(X_train)
colnames(X_traindf) <- attributeNames

y_test_est <- knn(X_train, X_test, cl=y_train, k = K, prob = FALSE, algorithm="kd_tree")


tr <- classNames[as.numeric(y_test)]
pr <- classNames[as.numeric(y_test_est)]





##### Second Model
##### This model is the more complex model, since it uses cross validation
##### to determine the optimal value for the hyperparameter number of neighbours.
##### This model is meant to utilize leave-one-out cross validation, but this could
##### be extermely time consuming. Consider setting the value of K equal to 
##### 50, 100 or 200. It's by default set to K = N = 1600. 


rm(list=ls())
library(FNN)
library(dplyr)
library(FNN)
library(cvTools)
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

X <- wine[1:300]
y <- wine[301][,1]
N = dim(wine)[1]
attributeNames = as.vector(colnames(wine))[1:300]

# Leave-one-out cross validation
CV <- cvFolds(N, K=N)
#K = N
K = N

# K-nearest neighbors parameters
L = 300; # Maximum number of neighbors

# Variable for classification error
Error = array(rep(NA, times=K*L), dim=c(K,L))

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
  
  for(l in 1:L){ # For each number of neighbors
    
    # Use knn classify to find the l nearest neighbors
    y_test_est <- knn(X_traindf, X_testdf, cl=y_train, k = l, prob = FALSE, algorithm="kd_tree")
    
    # Compute number of classification errors
    Error[k,l] = sum(y_test!=y_test_est); # Count the number of errors
  }
}

## Plot the classification error rate
plot(colSums(Error)/sum(CV$TestSize)*100, main='Error rate', xlab='Number of neighbors', ylab='Classification error rate (%)', pch=20, type='l')

### Uncomment the next line to save the error matrix. 
###write.table(Error, 'Error.txt')