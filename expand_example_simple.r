# Built a simple matrix
n.rows <- 100
n.cols <- 100
mm <- matrix(1, n.rows, n.cols)

# pixel mode patch
#set.seed(27)
source("Desktop/scripts/multi_expand.r")
example_cell8nn<-multiexpand(mm,50,8,1,c(n.rows/2,n.cols/2),10000,100,c(50,50),mode="ca",nn8)
example_cell16nn<-multiexpand(mm,50,14,1,c(n.rows/2,n.cols/2),10000,100,c(5,5),mode="ca",nn16)
example_cell24nn<-multiexpand(mm,50,24,1,c(n.rows/2,n.cols/2),10000,100,c(5,5),mode="ca",nn24)

library(raster)
set_Polypath(FALSE)
options(bitmapType="cairo")
par(mfrow=c(3,1))
plot(raster(example_cell8nn, xmx=n.cols, ymx=n.rows), col=topo.colors(255))
plot(raster(example_cell16nn, xmx=n.cols, ymx=n.rows), col=topo.colors(255))
plot(raster(example_cell24nn, xmx=n.cols, ymx=n.rows), col=topo.colors(255))

# For testing
#Built a simple matrix
n.rows <- 100
n.cols <- 100
mm <- matrix(1, n.rows, n.cols)

# Options
cluster.number=20
cluster.size=9
cluster.size.prob=0
start=c(n.rows/2,n.cols/2)
count.max=2000
range=1
contiguity=c(10,10)
x=mm
mode="ca"
nbr.matrix=nn8

nn8<-matrix(c(-1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1), nrow=2)
nn14<-matrix(c(-2,-1, -2,0, -2,1, -1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1, 2,-1, 2,0, 2,1), nrow=2)
nn24<-matrix(c(-2,-2, -2,-1, -2,0, -2,1, -2,2, -1,-2, -1,-1, -1,0, -1,1, -1,2, 0,-2, 0,-1, 0,1, 0,2, 1,-2, 1,-1, 1,0, 1,1, 1,2, 2,-2, 2,-1, 2,0, 2,1, 2,2), nrow=2)

example_pixel<-multiexpand(mm,20,cluster.size=15,0,c(n.rows/2,n.cols/2),10000,0,c(10,10),mode="ca",nbr.matrix=nn16)
plot(raster(example_pixel[n.rows:1,] , xmx=n.cols, ymx=n.rows), col=terrain.colors(255)[1], alpha=.4,useRaster=TRUE)
