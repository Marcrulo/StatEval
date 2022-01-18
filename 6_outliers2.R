load('armdata.RData')

armdata[[5]][[2]][[7]]

new <- matrix(0,100,3)
for (x in 1:100){
  rx=0
  ry=0
  rz=0
  
  for (r in 1:10){
    if (r != 7){
      rx=rx+armdata[[5]][[2]][[r]][x,1]
      ry=ry+armdata[[5]][[2]][[r]][x,2]
      rz=rz+armdata[[5]][[2]][[r]][x,3]
    }
  }
  new[x,1]=rx/9
  new[x,2]=ry/9
  new[x,3]=rz/9
}

armdata[[5]][[2]][[7]]<-new