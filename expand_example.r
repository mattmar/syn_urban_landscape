#Built a simple matrix
n.rows <- 300
n.cols <- 300
mm <- matrix(1, n.rows, n.cols)
# GetColorHexAndDecimal("red")

#Synusoidal river
ab <- sapply(1:n.cols, function(i)
      c(ceiling(n.rows * 0.5 * (1 + exp(-0.5*i/n.cols) * sin(8*i/n.cols))), i))
mm[t(ab)] <- -1; mm[t(ab - c(1,0))] <- -1; mm[t(ab + c(1,0))] <- -1
river<-mm; river[river!=-1] <- NA


#cells selected exponentially, column exponentiated
ij <- sapply(1:n.cols, function(i)
      c(ceiling(n.rows * 0.5 * (1 + exp(0.5*i/n.cols) * sin(0.5*i/n.cols))), i))
mm[t(ij)] <- -1; mm[t(ij - c(1,0))] <- -1; mm[t(ij + c(1,0))] <- -1

#cells selected exponentially, rows exponentiated
ji <- sapply(1:n.rows, function(i)
      c(i,ceiling(n.cols * 0.5 * (1 + exp(0.5*i/n.rows) * sin(0.5*i/n.rows)))))
mm[t(ji)] <- -1; mm[t(ji - c(0,1))] <- -1; mm[t(ji + c(0,1))] <- -1


plot(raster(mm[n.rows:1,] , xmx=n.cols, ymx=n.rows),
  col=c("#000000","#FF0000"),alpha=.8,useRaster=FALSE)

#Run the function to simulate big houses
big_houses<-multiexpand(mm,500,cluster.size=25,0,c(n.rows/2,n.cols/2),10000,100,c(50,50),mode="ca",nbr.matrix=nn24)

plot(raster(big_houses[n.rows:1,] , xmx=n.cols, ymx=n.rows),
  col=c("#000000","#0000FF"),alpha=.8,useRaster=FALSE)

#New matrix to be filled with occupied features
mm1 <- matrix(1, n.rows, n.cols)
mm1[which(mm==-1)] <- -1
mm1[which(big_houses>1)] <- -1

#Run the function to simulate smaller houses
small_houses<-multiexpand(mm1,1000,cluster.size=15,0,c(n.rows/2,n.cols/2),10000,10,c(10,10),mode="ca",nbr.matrix=nn14)

plot(raster(small_houses[n.rows:1,] , xmx=n.cols, ymx=n.rows),
  col=c("#000000","#0000FF"),alpha=.8,useRaster=FALSE)

#New matrix to be filled with occupied features
mm2 <- matrix(1, n.rows, n.cols)
mm2[which(mm==-1)] <- -1
mm2[which(small_houses>1)] <- -1
mm2[which(big_houses>1)] <- -1

#Run the function for green areas
green_areas<-multiexpand(mm2,1000,10,0,c(n.rows/2,n.cols/2),20000,10,c(10,10),mode="pixel",nn24)

plot(raster(green_areas[n.rows:1,] , xmx=n.cols, ymx=n.rows), col=c("#000000","#0000FF"),alpha=.8,useRaster=FALSE)

# Plot clusters

library(raster)
set_Polypath(FALSE)
options(bitmapType="cairo")
plot(raster(mm[n.rows:1,], xmx=n.cols, ymx=n.rows), col=c("black", "white"))
plot(raster(river[n.rows:1,], xmx=n.cols, ymx=n.rows), col=c("blue"),add=T)
plot(raster(big_houses[n.rows:1,] , xmx=n.cols, ymx=n.rows), col=grey.colors(1)[1],alpha=.8,useRaster=TRUE,add=T)
plot(raster(small_houses[n.rows:1,] , xmx=n.cols, ymx=n.rows), col=grey.colors(2)[1],alpha=.3,useRaster=TRUE,add=T)
plot(raster(green_areas[n.rows:1,] , xmx=n.cols, ymx=n.rows),
  col=terrain.colors(255)[1],alpha=.4,useRaster=TRUE,add=T,legend=FALSE)

# For testing
#Built a simple matrix
n.rows <- 10
n.cols <- 10
mm <- matrix(1, n.rows, n.cols)

# Options
cluster.number=1
cluster.size=9
cluster.size.prob=0
start=c(n.rows/2,n.cols/2)
count.max=200
range=1
contiguity=1
x=mm
mode="ca"
nbr.matrix=nn8

nn8<-matrix(c(-1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1), nrow=2)
nn16<-matrix(c(-2,-1,-2,0,-2,1,-1,-1, -1,0, -1,1, 0,-1, 0,1, 0,2, 1,-1, 1,0, 1,1, 2,-1, 2,0, 2,1), nrow=2)
nn24<-matrix(c(-2,-2, -2,-1,-2,0,-2,1,-2,2,-1,2,-1,-1, -1,0, -1,1, -1,2, 0,-2, 0,-1, 0,1, 0,2, 1,2, 1,-1, 1,0, 1,1, 1,2, 2,-2, 2,-1, 2,0, 2,1, 2,2), nrow=2)
