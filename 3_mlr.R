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


############ MLR
library(dplyr)

wine <- as.data.frame(wineC)

# Using sample_frac to create 70 - 30 slipt into test and train
#train <- sample_frac(wine, 0.7)
#sample_id <- as.numeric(rownames(train)) # rownames() returns character so as.numeric
#test <- wine[-sample_id,]
set.seed(222)
sample_size = round(nrow(wine)*.70) # setting what is 70%
index <- sample(seq_len(nrow(wine)), size = sample_size)
train <- wine[index, ]
test  <- wine[-index, ]

# Setting the basline 
#train$V301 <- relevel(train$V301, ref = "16")

require(nnet)
# Training the multinomial model
multinom.fit <- multinom(V201 ~ ., data = train, MaxNWts=20000) 

# Checking the model
#summary(multinom.fit)

## extracting coefficients from the model and exponentiate
exp(coef(multinom.fit))

head(probability.table <- fitted(multinom.fit))


# Predicting the values for train dataset
train$precticed <- predict(multinom.fit, newdata = train)

# Building classification table
ctable1 <- table(train$V201, train$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable1))/sum(ctable1))*100,2)


# Predicting the values for test dataset
test$precticed <- predict(multinom.fit, newdata = test, "class")

# Building classification table
ctable2 <- table(test$V201, test$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable2))/sum(ctable2))*100,2)

























