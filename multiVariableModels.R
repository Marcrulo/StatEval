load('armdata.RData')
library(cvTools)
library(dplyr)
library(nnet)
library(FNN)

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
wine = matrix(NA,1600,301) # x/y/z
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
wine <- as.data.frame(wine)

wineA= matrix(NA,1600,201) # x/y
for (e in 1:16) { # experiment
  for (p in 1:10) { # person
    for (r in 1:10){ # rep
      for (c in 1:2){ 
        for (x in 1:100){ # step
          wineA[(e-1)*100 + (p-1)*10 + r,(c-1)*100+x]=armdata[[e]][[p]][[r]][x,c] # M[row,column]
          wineA[(e-1)*100 + (p-1)*10 + r, 201] = e
        }
      }
    }
  }
}
wineA <- as.data.frame(wineA)

wineB= matrix(NA,1600,201) # x/z
for (e in 1:16) { # experiment
  for (p in 1:10) { # person
    for (r in 1:10){ # rep
      for (c in 1:2){ 
        if (c==1){
          for (x in 1:100){ # step
            wineB[(e-1)*100 + (p-1)*10 + r,(c-1)*100+x]=armdata[[e]][[p]][[r]][x,1] # M[row,column]
            wineB[(e-1)*100 + (p-1)*10 + r, 201] = e
          }
        }
        else{
          for (x in 1:100){ # step
            wineB[(e-1)*100 + (p-1)*10 + r,(c-1)*100+x]=armdata[[e]][[p]][[r]][x,3] # M[row,column]
            wineB[(e-1)*100 + (p-1)*10 + r, 201] = e
          }
        }
        
      }
    }
  }
}
wineB <- as.data.frame(wineB)

wineC= matrix(NA,1600,201) # y/z
for (e in 1:16) { # experiment
  for (p in 1:10) { # person
    for (r in 1:10){ # rep
      for (c in 1:2){ 
        if (c==1){
          for (x in 1:100){ # step
            wineC[(e-1)*100 + (p-1)*10 + r,(c-1)*100+x]=armdata[[e]][[p]][[r]][x,2] # M[row,column]
            wineC[(e-1)*100 + (p-1)*10 + r, 201] = e
          }
        }
        else{
          for (x in 1:100){ # step
            wineC[(e-1)*100 + (p-1)*10 + r,(c-1)*100+x]=armdata[[e]][[p]][[r]][x,3] # M[row,column]
            wineC[(e-1)*100 + (p-1)*10 + r, 201] = e
          }
        }
        
      }
    }
  }
}
wineC <- as.data.frame(wineC)

wineD = matrix(NA,1600,101)
wineE = matrix(NA,1600,101)
wineF = matrix(NA,1600,101)
for (e in 1:16) { # experiment
  for (p in 1:10) { # person
    for (r in 1:10){ # rep
      for (x in 1:100){ # step
        wineD[(e-1)*100 + (p-1)*10 + r,x]=armdata[[e]][[p]][[r]][x,1] # M[row,column]
        wineD[(e-1)*100 + (p-1)*10 + r, 101] = e
        
        wineE[(e-1)*100 + (p-1)*10 + r,x]=armdata[[e]][[p]][[r]][x,2] # M[row,column]
        wineE[(e-1)*100 + (p-1)*10 + r, 101] = e
        
        wineF[(e-1)*100 + (p-1)*10 + r,x]=armdata[[e]][[p]][[r]][x,3] # M[row,column]
        wineF[(e-1)*100 + (p-1)*10 + r, 101] = e
      }
      
    }
  }
}
wineD <- as.data.frame(wineD)
wineE <- as.data.frame(wineE)
wineF <- as.data.frame(wineF)





X <- wine[1:300]
y <- wine[301][,1]
N = dim(wine)[1]
attributeNames = as.vector(colnames(wine))[1:300]

# Leave-one-out cross validation
CV <- cvFolds(N, K=40)
K = 40



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





XA <- wineA[1:200]
yA <- wineA[201][,1]
N_A = dim(wineA)[1]
attributeNamesA = as.vector(colnames(wineA))[1:200]

# K-fold cross validation
CV <- cvFolds(N_A, K=40)
K = 40


# Variables for classification errors
ErrorMNRA = c(1:K) * 0
ErrorKNNA = c(1:K) * 0


for(k in 1:K){ # For each cross validation fold
  print(paste('Crossvalidation fold ', k, '/', CV$NumTestSets, sep=''))
  
  # Extract training and test set
  X_trainA <- XA[CV$subsets[CV$which!=k], ];
  y_trainA <- yA[CV$subsets[CV$which!=k]];
  X_testA <- XA[CV$subsets[CV$which==k], ];
  y_testA <- yA[CV$subsets[CV$which==k]];
  CV$TrainSize[k] <- length(y_trainA)
  CV$TestSize[k] <- length(y_testA)
  
  X_testdfA <- data.frame(X_testA)
  colnames(X_testdfA) <- attributeNamesA
  X_traindfA <- data.frame(X_trainA)
  colnames(X_traindfA) <- attributeNamesA
  
  model_MNRA <- multinom(formula=y_trainA ~ ., data=X_traindfA, MaxNWts =10000000, maxit=200)
  
  ### The problem is down here!
  y_test_est_MNRA <- predict(model_MNRA, newdata = X_testdfA, "class")
  y_test_est_KNNA <- knn(X_traindfA, X_testdfA, cl=y_trainA, k = 25, prob = FALSE, algorithm="kd_tree")
  
  ErrorMNRA[k] = sum(y_testA!=y_test_est_MNRA)
  ErrorKNNA[k] = sum(y_testA!=y_test_est_KNNA)
}

### The following is the error rate of the MNR and KNN models: 
mean(ErrorMNRA/(Na/K)) ### 
mean(ErrorKNNA/(Na/K)) ### 






XB <- wineB[1:200]
yB <- wineB[201][,1]
N_B = dim(wineB)[1]
attributeNamesB = as.vector(colnames(wineB))[1:200]

# Leave-one-out cross validation
CV <- cvFolds(N_B, K=40)
K = 40


# Variables for classification errors
ErrorMNRB = c(1:K) * 0
ErrorKNNB = c(1:K) * 0


for(k in 1:K){ # For each cross validation fold
  print(paste('Crossvalidation fold ', k, '/', CV$NumTestSets, sep=''))
  
  # Extract training and test set
  X_trainB <- XB[CV$subsets[CV$which!=k], ];
  y_trainB <- yB[CV$subsets[CV$which!=k]];
  X_testB <- XB[CV$subsets[CV$which==k], ];
  y_testB <- yB[CV$subsets[CV$which==k]];
  CV$TrainSize[k] <- length(y_trainB)
  CV$TestSize[k] <- length(y_testB)
  
  X_testdfB <- data.frame(X_testB)
  colnames(X_testdfB) <- attributeNamesB
  X_traindfB <- data.frame(X_trainB)
  colnames(X_traindfB) <- attributeNamesB
  
  model_MNRB <- multinom(formula=y_trainB ~ ., data=X_traindfB, MaxNWts =10000000, maxit=200)
  
  ### The problem is down here!
  y_test_est_MNRB <- predict(model_MNRB, newdata = X_testdfB, "class")
  y_test_est_KNNB <- knn(X_traindfB, X_testdfB, cl=y_trainB, k = 25, prob = FALSE, algorithm="kd_tree")
  
  ErrorMNRB[k] = sum(y_testB!=y_test_est_MNRB)
  ErrorKNNB[k] = sum(y_testB!=y_test_est_KNNB)
}

### The following is the error rate of the MNR and KNN models: 
mean(ErrorMNRB/(N_B/K)) ### 
mean(ErrorKNNB/(N_B/K)) ### 





XC <- wineC[1:200]
yC <- wineC[201][,1]
N_C = dim(wineC)[1]
attributeNamesC = as.vector(colnames(wineC))[1:200]

# Leave-one-out cross validation
CV <- cvFolds(N_C, K=40)
K = 40


# Variables for classification errors
ErrorMNRC = c(1:K) * 0
ErrorKNNC = c(1:K) * 0


for(k in 1:K){ # For each cross validation fold
  print(paste('Crossvalidation fold ', k, '/', CV$NumTestSets, sep=''))
  
  # Extract training and test set
  X_trainC <- XC[CV$subsets[CV$which!=k], ];
  y_trainC <- yC[CV$subsets[CV$which!=k]];
  X_testC <- XC[CV$subsets[CV$which==k], ];
  y_testC <- yC[CV$subsets[CV$which==k]];
  CV$TrainSize[k] <- length(y_trainC)
  CV$TestSize[k] <- length(y_testC)
  
  X_testdfC <- data.frame(X_testC)
  colnames(X_testdfC) <- attributeNamesC
  X_traindfC <- data.frame(X_trainC)
  colnames(X_traindfC) <- attributeNamesC
  
  model_MNRC <- multinom(formula=y_trainC ~ ., data=X_traindfC, MaxNWts =10000000, maxit=200)
  
  ### The problem is down here!
  y_test_est_MNRC <- predict(model_MNRC, newdata = X_testdfC, "class")
  y_test_est_KNNC <- knn(X_traindfC, X_testdfC, cl=y_trainC, k = 25, prob = FALSE, algorithm="kd_tree")
  
  ErrorMNRC[k] = sum(y_testC!=y_test_est_MNRC)
  ErrorKNNC[k] = sum(y_testC!=y_test_est_KNNC)
}

### The following is the error rate of the MNR and KNN models: 
mean(ErrorMNRC/(N_C/K)) ### 
mean(ErrorKNNC/(N_C/K)) ### 









XD <- wineD[1:100]
yD <- wineD[101][,1]
N_D = dim(wineD)[1]
attributeNamesD = as.vector(colnames(wineD))[1:100]

# Leave-one-out cross validation
CV <- cvFolds(N_D, K=40)
K = 40


# Variables for classification errors
ErrorMNRD = c(1:K) * 0
ErrorKNND = c(1:K) * 0


for(k in 1:K){ # For each cross validation fold
  print(paste('Crossvalidation fold ', k, '/', CV$NumTestSets, sep=''))
  
  # Extract training and test set
  X_trainD <- XD[CV$subsets[CV$which!=k], ];
  y_trainD <- yD[CV$subsets[CV$which!=k]];
  X_testD <- XD[CV$subsets[CV$which==k], ];
  y_testD <- yD[CV$subsets[CV$which==k]];
  CV$TrainSize[k] <- length(y_trainD)
  CV$TestSize[k] <- length(y_testD)
  
  X_testdfD <- data.frame(X_testD)
  colnames(X_testdfD) <- attributeNamesD
  X_traindfD <- data.frame(X_trainD)
  colnames(X_traindfD) <- attributeNamesD
  
  model_MNRD <- multinom(formula=y_trainD ~ ., data=X_traindfD, MaxNWts =10000000, maxit=200)
  
  ### The problem is down here!
  y_test_est_MNRD <- predict(model_MNRD, newdata = X_testdfD, "class")
  y_test_est_KNND <- knn(X_traindfD, X_testdfD, cl=y_trainD, k = 25, prob = FALSE, algorithm="kd_tree")
  
  ErrorMNRD[k] = sum(y_testD!=y_test_est_MNRD)
  ErrorKNND[k] = sum(y_testD!=y_test_est_KNND)
}

### The following is the error rate of the MNR and KNN models: 
mean(ErrorMNRD/(N_D/K)) ### 
mean(ErrorKNND/(N_D/K)) ### 


#### IMPORTAN OBSERVATION: The uni-variable models do not seem to be optimized 
#### after 200 iterations. We might want to consider optimizing them further.






XE <- wineE[1:100]
yE <- wineE[101][,1]
N_E = dim(wineE)[1]
attributeNamesE = as.vector(colnames(wineE))[1:100]

# Leave-one-out cross validation
CV <- cvFolds(N_E, K=40)
K = 40


# Variables for classification errors
ErrorMNRE = c(1:K) * 0
ErrorKNNE = c(1:K) * 0


for(k in 1:K){ # For each cross validation fold
  print(paste('Crossvalidation fold ', k, '/', CV$NumTestSets, sep=''))
  
  # Extract training and test set
  X_trainE <- XE[CV$subsets[CV$which!=k], ];
  y_trainE <- yE[CV$subsets[CV$which!=k]];
  X_testE <- XE[CV$subsets[CV$which==k], ];
  y_testE <- yE[CV$subsets[CV$which==k]];
  CV$TrainSize[k] <- length(y_trainE)
  CV$TestSize[k] <- length(y_testE)
  
  X_testdfE <- data.frame(X_testE)
  colnames(X_testdfE) <- attributeNamesE
  X_traindfE <- data.frame(X_trainE)
  colnames(X_traindfE) <- attributeNamesE
  
  model_MNRE <- multinom(formula=y_trainE ~ ., data=X_traindfE, MaxNWts =10000000, maxit=200)
  
  ### The problem is down here!
  y_test_est_MNRE <- predict(model_MNRE, newdata = X_testdfE, "class")
  y_test_est_KNNE <- knn(X_traindfE, X_testdfE, cl=y_trainE, k = 25, prob = FALSE, algorithm="kd_tree")
  
  ErrorMNRE[k] = sum(y_testE!=y_test_est_MNRE)
  ErrorKNNE[k] = sum(y_testE!=y_test_est_KNNE)
}

### The following is the error rate of the MNR and KNN models: 
mean(ErrorMNRE/(N_E/K)) ### 
mean(ErrorKNNE/(N_E/K)) ### 







XF <- wineF[1:100]
yF <- wineF[101][,1]
N_F = dim(wineF)[1]
attributeNamesF = as.vector(colnames(wineF))[1:100]

# Leave-one-out cross validation
CV <- cvFolds(N_F, K=40)
K = 40


# Variables for classification errors
ErrorMNRF = c(1:K) * 0
ErrorKNNF = c(1:K) * 0


for(k in 1:K){ # For each cross validation fold
  print(paste('Crossvalidation fold ', k, '/', CV$NumTestSets, sep=''))
  
  # Extract training and test set
  X_trainF <- XF[CV$subsets[CV$which!=k], ];
  y_trainF <- yF[CV$subsets[CV$which!=k]];
  X_testF <- XF[CV$subsets[CV$which==k], ];
  y_testF <- yF[CV$subsets[CV$which==k]];
  CV$TrainSize[k] <- length(y_trainF)
  CV$TestSize[k] <- length(y_testF)
  
  X_testdfF <- data.frame(X_testF)
  colnames(X_testdfF) <- attributeNamesF
  X_traindfF <- data.frame(X_trainF)
  colnames(X_traindfF) <- attributeNamesF
  
  model_MNRF <- multinom(formula=y_trainF ~ ., data=X_traindfF, MaxNWts =10000000, maxit=200)
  
  ### The problem is down here!
  y_test_est_MNRF <- predict(model_MNRF, newdata = X_testdfF, "class")
  y_test_est_KNNF <- knn(X_traindfF, X_testdfF, cl=y_trainF, k = 25, prob = FALSE, algorithm="kd_tree")
  
  ErrorMNRF[k] = sum(y_testF!=y_test_est_MNRF)
  ErrorKNNF[k] = sum(y_testF!=y_test_est_KNNF)
}

### The following is the error rate of the MNR and KNN models: 
mean(ErrorMNRF/(N_F/K)) ### 
mean(ErrorKNNF/(N_F/K)) ### 






### Evaluation

mean(ErrorMNR/(N/K)) ### 0.5875
mean(ErrorKNN/(N/K)) ### 0.530625

mean(ErrorMNRA/(N_A/K)) ### 0.72125
mean(ErrorKNNA/(N_A/K)) ### 0.78125

mean(ErrorMNRB/(N_B/K)) ### 0.56
mean(ErrorKNNB/(N_B/K)) ### 0.54625

mean(ErrorMNRC/(N_C/K)) ### 0.639375
mean(ErrorKNNC/(N_C/K)) ### 0.67125

mean(ErrorMNRD/(N_D/K)) ### 0.85625
mean(ErrorKNND/(N_D/K)) ### 0.849375

mean(ErrorMNRE/(N_E/K)) ### 0.816875
mean(ErrorKNNE/(N_E/K)) ### 0.813125

mean(ErrorMNRF/(N_F/K)) ### 0.776875
mean(ErrorKNNF/(N_F/K)) ### 0.765625
