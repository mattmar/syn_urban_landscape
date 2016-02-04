#Built a simple matrix
n.rows <- 300
n.cols <- 300
mma <- matrix(1, n.rows, n.cols)
# GetColorHexAndDecimal("red")

#Synusoidal river
ab <- sapply(1:n.cols, function(i)
      c(ceiling(n.rows * 0.5 * (1 + exp(-0.5*i/n.cols) * sin(8*i/n.cols))), i))
mma[t(ab)] <- -1; mma[t(ab - c(1,0))] <- -1; mma[t(ab + c(1,0))] <- -1
# river<-mma; river[river!=-1] <- NA

#cells selected exponentially, column exponentiated
ij <- sapply(1:n.cols, function(i)
      c(ceiling(n.rows * 0.5 * (1 + exp(0.5*i/n.cols) * sin(0.5*i/n.cols))), i))
mma[t(ij)] <- -1; mma[t(ij - c(1,0))] <- -1; mma[t(ij + c(1,0))] <- -1

#cells selected_a exponentially, rows exponentiated
ji <- sapply(1:n.rows, function(i)
      c(i,ceiling(n.cols * 0.5 * (1 + exp(0.5*i/n.rows) * sin(0.5*i/n.rows)))))
mma[t(ji)] <- -1; mma[t(ji - c(0,1))] <- -1; mma[t(ji + c(0,1))] <- -1

#Run the function to simulate big houses
big_houses_a<-multiexpand(mma,500,cluster.size=25,0,c(n.rows/2,n.cols/2),10000,10,c(50,50),mode="ca",nbr.matrix=nn24)

#New matrix to blled with occupied features
mma1 <- matrix(1, n.rows, n.cols)
mma1[which(mma==-1)] <- -1
mma1[which(big_houses_a>1)] <- -1

#Run the function to simulate smaller houses
small_houses_a<-multiexpand(mma1,1000,cluster.size=15,0,c(n.rows/2,n.cols/2),50000,10,c(10,10),mode="ca",nbr.matrix=nn14)

#New matrix to be filled with occupied features
mma2 <- matrix(1, n.rows, n.cols)
mma2[which(mma==-1)] <- -1
mma2[which(small_houses_a>1)] <- -1
mma2[which(big_houses_a>1)] <- -1

#Run the function for green areas
green_a<-multiexpand(mma2,2,5000,0,c(n.rows/2,n.cols/2),20000,10,c(500,500),mode="pixel",nn24)

# Final rasters
mma3 <- matrix(1, n.rows, n.cols)
mma3[which(green_a>2)] <- 9
mma3[which(big_houses_a>2)] <- 7
mma3[which(small_houses_a>2)] <- 8
mma3[which(mma==-1)] <- 5
mma3[which(river==-1)] <- 6

breakpoints <- c(0,4.9,5.9,6.9,7.9,8.9,9.9)

# Plot clusters
plot(as.factor(raster(mma3[n.rows:1,], xmx=n.cols, ymx=n.rows)), col=c("white","black","blue","grey85","grey80","dark green"),useRaster=TRUE, breaks=breakpoints)

a<-spplot(raster(mma3[n.rows:1,], xmx=n.cols, ymx=n.rows), col.regions=c("white","black","blue","grey85","grey80","dark green"),at= c(0,4.9,5.9,6.9,7.9,8.9,9.9))
