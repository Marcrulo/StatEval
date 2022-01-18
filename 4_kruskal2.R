load('armdata.RData')

# Matrices
MT = matrix(0,1600*3,10)
for (p in 1:10) { # person
  for (e in 1:16) { # experiment
    
    for (s in 1:100){ # step
      sum_x=0
      sum_y=0
      sum_z=0
      for (r in 1:10){ # rep
        sum_x = sum_x + armdata[[e]][[p]][[r]][s,1]
        sum_y = sum_y + armdata[[e]][[p]][[r]][s,2]
        sum_z = sum_z + armdata[[e]][[p]][[r]][s,3]
        
      }
      MT[(e-1)*300    +s,p]=sum_x/10 # M[row,column]
      MT[(e-1)*300+100+s,p]=sum_y/10 # M[row,column]
      MT[(e-1)*300+200+s,p]=sum_z/10 # M[row,column]
    }
    
  }
}


e=5
start=(e-1)*300+1
end=e*300
M=as.data.frame(MT[start:end,])

# Shaipro.test if p-value is under 0.05 the data is not normal distributed
shapiro.test(M$V1)
shapiro.test(M$V2)
shapiro.test(M$V3)
shapiro.test(M$V4)
shapiro.test(M$V5)
shapiro.test(M$V6)
shapiro.test(M$V7)
shapiro.test(M$V8)
shapiro.test(M$V9)
shapiro.test(M$V10)

kruskal.test(M)
boxplot(M)

M$V1

