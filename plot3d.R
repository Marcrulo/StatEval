load('armdata.RData')

# armdata[[exp]][[person]][[rep]]


######
library(rgl)
start_cyl <- cylinder3d(cbind(0, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
target_cyl <- cylinder3d(cbind(60, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl1 <- cylinder3d(cbind(0, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl2 <- cylinder3d(cbind(60, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)

dist <- cbind(15,22.5,30,37.5,45)
size <- cbind(20,27.5,35)

cyl3 <- cylinder3d(cbind(dist[5], 0, seq(0, size[3], length = 10)), radius = c(3,3,3), sides = 10, closed = -2)
shade3d(addNormals(subdivision3d(start_cyl)), col = 'darkgreen')
shade3d(addNormals(subdivision3d(target_cyl)), col = 'darkgreen')
shade3d(addNormals(subdivision3d(cyl1)), col = 'pink')
shade3d(addNormals(subdivision3d(cyl2)), col = 'pink', alpha = 0.5)
shade3d(addNormals(subdivision3d(cyl3)), col = 'lightblue')
surface3d(c(-7, 67), c(-20, 20), matrix(0, 2, 2), col = "brown", alpha = 0.9, specular = "black")

col.list <- c('green','red','blue','yellow','purple','black','brown','pink','lightblue','darkgreen')

#for (i in 1:10){ # Repetition number
#  lines3d(armdata[[1]][[1]][[i]],color=col.list[i])
#}
#for (i in 1:10){ # Person number
#  lines3d(armdata[[1]][[i]][[1]])
#}
#for (i in 1:16){ # Experiment number (person #1)
#  lines3d(armdata[[i]][[1]][[1]])
#}


# Hvis din PC holder til det
#for (i in 1:16){ # Experiment number (person #1)
  for (j in 1:10){ # Person number
    for (k in 1:10){ # Repetition number
      lines3d(armdata[[15]][[j]][[k]],color=col.list[j])
    }
  }
#}


