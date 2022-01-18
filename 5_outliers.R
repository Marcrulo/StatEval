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

library(outliers)

p_values_x <- matrix(0,160,100)
p_values_y <- matrix(0,160,100)
p_values_z <- matrix(0,160,100)

for (e in 1:16) {
  for (p in 1:10) {
    for (x in 1:100) {
      gemmeting_x = matrix(0,1,10)
      gemmeting_y = matrix(0,1,10)
      gemmeting_z = matrix(0,1,10)
      for (r in 1:10) {
        gemmeting_x[1,r] = armdata[[e]][[p]][[r]][x,1]
        gemmeting_y[1,r] = armdata[[e]][[p]][[r]][x,2]
        gemmeting_z[1,r] = armdata[[e]][[p]][[r]][x,3]
      }
      p_values_x[(e - 1) * 10 + p,x] = dixon.test(gemmeting_x[1,],type=11)$p.value
      if (dixon.test(gemmeting_x[1,])$p.value == 0){
        print(c(e,p,x))
        print(gemmeting_x[1,])
      }
      
      
      p_values_y[(e - 1) * 10 + p,x] = dixon.test(gemmeting_y[1,],type=11)$p.value
      p_values_z[(e - 1) * 10 + p,x] = dixon.test(gemmeting_z[1,],type=11)$p.value
    }
  }
}
#saveRDS(p_values_x, "pvalx.rds")
#saveRDS(p_values_y, "pvaly.rds")
#saveRDS(p_values_z, "pvalz.rds")
p_values_x <- readRDS("pvalx.rds")
p_values_y <- readRDS("pvaly.rds")
p_values_z <- readRDS("pvalz.rds")

threshold <- 0.01
reps <- c()
fail_rep = 1
`%!in%` <- Negate(`%in%`)

for (i in 1:100) {
  for (j in 1:160) {
    if (p_values_x[j,i] == 0 | p_values_y[j,i] == 0 | p_values_z[j,i] == 0) {
      
      if (j %!in% reps){
        reps[fail_rep]=j
        fail_rep = fail_rep + 1
      }
    }
  }
}
print(sort(reps))
