#Built a simple matrix
n.rows <- 500
n.cols <- 500
mm <- matrix(1, n.rows, n.cols)

#Neighboroud matrices
nn2y<-matrix(c(0,-1, 0,1), nrow=2)
nn2x<-matrix(c(-1,0, 1,0), nrow=2)

nn8<-matrix(c(-1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1), nrow=2)
nn14<-matrix(c(-2,-1, -2,0, -2,1, -1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1, 2,-1, 2,0, 2,1), nrow=2)
nn24<-matrix(c(-2,-2, -2,-1, -2,0, -2,1, -2,2, -1,-2, -1,-1, -1,0, -1,1, -1,2, 0,-2, 0,-1, 0,1, 0,2, 1,-2, 1,-1, 1,0, 1,1, 1,2, 2,-2, 2,-1, 2,0, 2,1, 2,2), nrow=2)

#Add landscape shapes
#Road1: cells selected exponentially, column exponentiated
ij <- sapply(1:n.cols, function(i)
  c(ceiling(n.rows * 0.5 * (1 + exp(0.5*i/n.cols) * sin(0.5*i/n.cols))), i))
mm[t(ij)] <- -1; mm[t(ij - c(1,0))] <- -1; mm[t(ij + c(1,0))] <- -999

#Road2: cells selected exponentially, rows exponentiated
ji <- sapply(1:n.rows, function(i)
  c(i,ceiling(n.cols * 0.5 * (1 + exp(0.5*i/n.rows) * sin(0.5*i/n.rows)))))
mm[t(ji)] <- -1; mm[t(ji - c(0,1))] <- -1; mm[t(ji + c(0,1))] <- -1
mm[mm==-1] <- -999

#Big Synusoidal river
ab <- sapply(1:n.cols, function(i)
  c(ceiling(n.rows * 0.5 * (1 + exp(-0.5*i/n.cols) * sin(5*i/n.cols))), i))

for (i in seq(-10,10)) {
    ab1<-rbind(ab[1,]+i,ab[2,])
    mm[t(ab1)] <- -1; mm[t(ab1 - c(1,0))] <- -1; mm[t(ab1 + c(1,0))] <- -1
}

#Add secondary roads
mm1 <- multiexpand(mm,100,1000,0,c(0,0),10000,1,c(10,10),"px",nbr.matrix=nn2x,xnnoise=2,along=TRUE,along.value=-999)

mm2 <- matrix(1, n.rows, n.cols)
mm2[mm == -999 | mm1 > 2] <- -999
mm2[mm == -1 ] <- -1

mm3 <- multiexpand(mm2,100,1000,0,c(n.rows/2,n.cols/2),10000,1,c(10,10),"px",nbr.matrix=nn2y,ynnoise=1,along=TRUE,along.value=-999)

mm4 <- matrix(1, n.rows, n.cols)
mm4[mm == -1] <- -1
mm4[mm2 == -999 | mm3 > 2] <- -999

#Add houses along the roads
houses_road<-multiexpand(mm4,1000,cluster.size=8,0,c(n.rows/2,n.cols/2),50000,50,c(50,50),mode="ca",nbr.matrix=nn8,along=TRUE,along.value=-999)

mm5 <- matrix(1, n.rows, n.cols)
mm5[mm4 == -999 ] <- -1
mm5[mm4 == -1 | houses_road>2] <- -1

#Big public green areas
green_park<-multiexpand(mm4,3,cluster.size=10000,1,c(n.rows/2,n.cols/2),50000,0,c(500,500),mode="px",nbr.matrix=nn8)

mm6 <- matrix(1, n.rows, n.cols)
mm6[mm5 == -1 | houses_road > 2 | green_park > 2] <- -1

#Small filling houses
houses_isolated<-multiexpand(mm6,10000,cluster.size=25,cluster.size.prob=0,start=c(50,400),count.max=100000,range=5,contiguity=c(5,5),mode="ca",nbr.matrix=nn24)

mm7 <- matrix(1, n.rows, n.cols)
mm7[mm6 == -1 | houses_road > 2 | green_park > 2 | houses_isolated >2] <- -1
mm7[houses_road>2] <- -999

small_houses<-multiexpand(mm7,1000,cluster.size=15,0,c(n.rows/2,n.cols/2),500000,5,c(5,5),mode="ca",nbr.matrix=nn14,along=TRUE,along.value=-999)

mm8 <- matrix(1, n.rows, n.cols)
mm8[mm7 == -1 | mm7 == -999 | green_park > 2 | houses_isolated >2 | houses_road>2 | small_houses >2] <- -1
mm8[houses_isolated>2]<--999
mm8[small_houses>2 ] <- -999

#Fill with green areas
green_private<-multiexpand(mm8,5000,cluster.size=5,0,c(n.rows/2,n.cols/2),10000,50,c(50,50),mode="px",nbr.matrix=nn8,along=T,along.value=-999)

mmf <- matrix(1, n.rows, n.cols)
mmf[ which(green_private>2) ] <- 8
mmf[ which(green_park>2) ] <- 7
mmf[ which(small_houses > 2) ] <- 6
mmf[ which(houses_isolated>2) ] <- 5
mmf[ which(houses_road>2) ] <- 4
mmf[ which(mm==-1) ] <- 3
mmf[ which(mm==-999) ] <- 2
mmf[ which(mm1>2) ] <- 2
mmf[ which(mm3>2) ] <- 2


for (i in seq(-10,10)) {
    ab1<-rbind(ab[1,]+i,ab[2,])
    mmf[t(ab1)] <- 3; mmf[t(ab1 - c(1,0))] <- 3; mmf[t(ab1 + c(1,0))] <- 3
}

png("~/Desktop/urb_land_aggregate.png",width = 300, height = 300, units="mm",res=300,pointsize=10)
par(mar = c(5,3,5,7))
plot(raster(mmf[n.rows:1,]),breaks=c(0.9,1.9,2.9,3.9,4.9,5.9,6.9,7.9,8.9,9.9),col=c("white","black","blue","grey85","grey80","grey75","light green","dark green"),legend=FALSE,xlim=c(0,1),useRaster=TRUE,ylim=c(0,1),asp=NA,main="Aggregated urban landscape",cex.main=2)

plot(raster(mmf[n.rows:1,]), legend.only=TRUE, col=c("white","black","blue","grey85","grey80","grey75","light green","dark green"), legend.width=1, legend.shrink=0.9, axis.args=list(at=seq(1, 8, 1), labels=c("Road", "River", "Road houses","Isolated Houses","Small Houses","Public Green areas","Private Green Areas","Others"), cex.axis=1),
   legend.args=list(text='Land use', side=4, font=2, line=1, cex=1),smallplot=c(0.88,0.89, 0.1,0.9),add=T); par(mar = par("mar"))
dev.off()





library(gridExtra)

top <- arrangeGrob(a, b, nrow=2)

plot(grid.arrange(top))

# Options
cluster.number=1
cluster.size=100
cluster.size.prob=0
start=c(n.rows/2,n.cols/2)
count.max=2000
range=1
contiguity=c(5,5)
x=mm
mode="ca"
nbr.matrix=nn8
along.value=-999
ynnoise=1
