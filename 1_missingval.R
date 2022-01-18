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

for (i in 1:16) {
  for (j in 1:10) {
    for (k in 1:10) {
      for (n in 1:3) {
        for (m in 1:100) {
          if (is.na(armdata[[i]][[j]][[k]][[m,n]]) == T) {
            print(c(i,j,k,n,m))
          }
        }
      }
    }
  }
}
