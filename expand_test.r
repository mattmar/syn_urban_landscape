mmt <- matrix(1, 100, 100)
mmt <-
#Prepare nn matrices
  nn2y<<-matrix(c(0,-1, 0,1), nrow=2)
  nn2x<<-matrix(c(-1,0, 1,0), nrow=2)
  nn8<<-matrix(c(-1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1), nrow=2)
  nn14<<-matrix(c(-2,-1, -2,0, -2,1, -1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1, 2,-1, 2,0, 2,1), nrow=2)
  nn24<<-matrix(c(-2,-2, -2,-1, -2,0, -2,1, -2,2, -1,-2, -1,-1, -1,0, -1,1, -1,2, 0,-2, 0,-1, 0,1, 0,2, 1,-2, 1,-1, 1,0, 1,1, 1,2, 2,-2, 2,-1, 2,0, 2,1, 2,2), nrow=2)

#test1
test1<-multiexpand(mmt,100,cluster.size=9,0,c(n.rows/2,n.cols/2),50000,0,c(0,0),mode="px",nbr.matrix=nn8,debug=1)

par(mfrow=c(2,3))
plot(raster(test1),useRaster=TRUE)
