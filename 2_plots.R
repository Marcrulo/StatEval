load('armdata.RData')

### Boxplot
# M[row,column]

M1 = matrix(0,16000,10)
M2 = matrix(0,16000,10)
M3 = matrix(0,16000,10)
for (p in 1:10) { # person
  for (e in 1:16) { # experiment
    for (r in 1:10){ # rep
      for (x in 1:100){ # step
        M1[(e-1)*1000 + (r-1)*100 + x ,p]=armdata[[e]][[p]][[r]][x,1]
        M2[(e-1)*1000 + (r-1)*100 + x ,p]=armdata[[e]][[p]][[r]][x,2]
        M3[(e-1)*1000 + (r-1)*100 + x ,p]=armdata[[e]][[p]][[r]][x,3]
      }
    }
  }
}

boxplot(M)

