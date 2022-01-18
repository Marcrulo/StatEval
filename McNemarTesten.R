rm(list=ls())
graphics.off()
library(cvTools)
library(dplyr)
library(nnet)
library(FNN)

mcnemar <- function(y_true, yhatA, yhatB, alpha=0.05){
  nn <- matrix(, nrow = 2, ncol = 2)  
  #c1 = yhatA - y_true == 0
  #c2 = yhatB - y_true == 0  
  ### Subtraction of two factors is not possible, so the following modification
  ### to the code must be made. 
  c1 = (y_test_est_KNN == y_test)
  c2 = (y_test_est_MNR == y_test)
  
  nn[1,1] = sum(c1 & c2)
  nn[1,2] = sum(c1 & !c2)
  nn[2,1] = sum(!c1 & c2)
  nn[2,2] = sum(!c1 & !c2)
  
  n12 = nn[1,2]
  n21 = nn[2,1]
  n <- sum(nn)
  Etheta = (n12-n21)/n
  Q = n**2 * (n+1) * (Etheta+1) * (1-Etheta) / ( (n*(n12+n21) - (n12-n21)**2) )
  p = (Etheta + 1)/2 * (Q-1)
  q = (1-Etheta)/2 * (Q-1)
  
  thetaL =  
    CI <- c( 2*qbeta( alpha/2, p, q)-1, 2*qbeta(1-alpha/2, p, q)-1)
  # thetahat <-  2*p/(p+q) - 1
  
  p <- 2*pbinom(min( n12, n21), size=n12+n21, prob=0.5  )
  
  
  print(paste("Result of McNemars test using alpha=", alpha))
  
  print("Comparison matrix n")
  print(nn)
  if(n12+n21 <= 10){
    print(paste("Warning, n12+n21 is low: n12+n21=",(n12+n21)))
  }
  print("Approximate 1-alpha confidence interval of theta: [thetaL,thetaU] = ")
  print(CI)
  print(paste( "p-value for two-sided test A and B have same accuracy (exact binomial test): p=", p) )
  
  rt = list("thetahat"=Etheta, "CI"=CI, "p"=p)
  return(rt)  
}


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
#### In the following section we train a two MNR and KNN models and compare them
#### using the mcnemar test.  
wine <- as.data.frame(wine)

# Using sample_frac to create 70 - 30 slipt into test and train
#train <- sample_frac(wine, 0.7)
#sample_id <- as.numeric(rownames(train)) # rownames() returns character so as.numeric
#test <- wine[-sample_id,]
set.seed(222)
sample_size = round(nrow(wine)*.65) # setting what is 70%
index <- sample(seq_len(nrow(wine)), size = sample_size)
train <- wine[index, ]
test <- wine[-index, ]


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



model_MNR <- multinom(formula=y_train ~ ., data=X_traindf, MaxNWts =10000000, maxit=200)

# Predicting the values for test dataset
y_test_est_MNR <- predict(model_MNR, newdata = X_test, "class")
y_test_est_KNN <- knn(X_traindf, X_testdf, cl=y_train, k = 25, prob = FALSE, algorithm="kd_tree")


sum(y_test!=y_test_est_MNR)/length(y_test)
sum(y_test!=y_test_est_KNN)/length(y_test)


alpha = 0.05
rt <- mcnemar(y_test, y_test_est_KNN, y_test_est_MNR, alpha=alpha)


### For 0.9 p = 0.5446. MNR = 0.58125, KNN = 0.54375. 
### For 0.8 p = 0.3384. MNR = 0.609375,  KNN = 0.571875. 
### For 0.7 p = 0.0543. MNR = 0.6229167, KNN = 0.5666667.
### For 0.65 p = 0.0482. MNR = 0.6160714, KNN = 0.5607143. 
### For 0.6 p = 0.0106. MNR = 0.621875, KNN = 0.559375. 









#### In the following section we train an MNR and a KNN model and compare them
#### using the mcnemar test. This will be done while applying k-fold cross-validation. 
#### Afterwards the values of p, will be adjusted by the benjamini-hochberg 
#### correction method.


X <- wine[1:300]
y <- wine[301][,1]
N = dim(wine)[1]
attributeNames = as.vector(colnames(wine))[1:300]

# Leave-one-out cross validation
CV <- cvFolds(N, K=16)
K = 16



# Variables for classification errors
ErrorMNR = c(1:K) * 0
ErrorKNN = c(1:K) * 0
pVals = c(1:K) * 0
CIs1 = c(1:K) * 0
CIs2 = c(1:K) * 0
thetas = c(1:K) * 0
alpha = 0.05


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
  
  model_MNR <- multinom(formula=y_train ~ ., data=X_traindf, MaxNWts =10000000, maxit=200)
  
  ### The problem is down here!
  y_test_est_MNR <- predict(model_MNR, newdata = X_testdf, "class")
  y_test_est_KNN <- knn(X_traindf, X_testdf, cl=y_train, k = 25, prob = FALSE, algorithm="kd_tree")
  
  ErrorMNR[k] = sum(y_test!=y_test_est_MNR)
  ErrorKNN[k] = sum(y_test!=y_test_est_KNN)
  
  rt = mcnemar(y_test, y_test_est_KNN, y_test_est_MNR, alpha=alpha)
  pVals[k] = rt$p
  CIs1[k] = rt$CI[1]
  CIs2[k] = rt$CI[2]
  thetas[k] = rt$thetahat 
}


p.adjust(pVals, method = "hochberg")

### IMPORTANT COMMENT: Maybe using a Mcnemar test is not so smart afterall, 
### remember that a lower fold number means a higher p value in the mcnemmar test.



### The following aims to calculate the error rate of the MNR and KNN models. It's 
### done using k-fold cross validation with k = 800. 


X <- wine[1:300]
y <- wine[301][,1]
N = dim(wine)[1]
attributeNames = as.vector(colnames(wine))[1:300]

# Leave-one-out cross validation
CV <- cvFolds(N, K=800)
K = 800



# Variables for classification errors
ErrorMNR = c(1:K) * 0
ErrorKNN = c(1:K) * 0


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
  
  model_MNR <- multinom(formula=y_train ~ ., data=X_traindf, MaxNWts =10000000, maxit=200)
  
  ### The problem is down here!
  y_test_est_MNR <- predict(model_MNR, newdata = X_testdf, "class")
  y_test_est_KNN <- knn(X_traindf, X_testdf, cl=y_train, k = 25, prob = FALSE, algorithm="kd_tree")
  
  ErrorMNR[k] = sum(y_test!=y_test_est_MNR)
  ErrorKNN[k] = sum(y_test!=y_test_est_KNN)
}

### The following is the error rate of the MNR and KNN models: 
mean(ErrorMNR/(N/K)) ### 0.5825
mean(ErrorKNN/(N/K)) ### 0.526875
