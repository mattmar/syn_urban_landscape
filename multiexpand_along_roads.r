#Built a simple matrix
n.rows <- 300
n.cols <- 300
mm <- matrix(1, n.rows, n.cols)

#Neighboroud matrices
nn8<-matrix(c(-1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1), nrow=2)
nn14<-matrix(c(-2,-1, -2,0, -2,1, -1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1, 2,-1, 2,0, 2,1), nrow=2)
nn24<-matrix(c(-2,-2, -2,-1, -2,0, -2,1, -2,2, -1,-2, -1,-1, -1,0, -1,1, -1,2, 0,-2, 0,-1, 0,1, 0,2, 1,-2, 1,-1, 1,0, 1,1, 1,2, 2,-2, 2,-1, 2,0, 2,1, 2,2), nrow=2)

#Add landscape shapes
#Road1: cells selected exponentially, column exponentiated
ij <- sapply(1:n.cols, function(i)
  c(ceiling(n.rows * 0.5 * (1 + exp(0.5*i/n.cols) * sin(0.5*i/n.cols))), i))
mm[t(ij)] <- -1; mm[t(ij - c(1,0))] <- -1; mm[t(ij + c(1,0))] <- -1

#Road2: cells selected exponentially, rows exponentiated
ji <- sapply(1:n.rows, function(i)
  c(i,ceiling(n.cols * 0.5 * (1 + exp(0.5*i/n.rows) * sin(0.5*i/n.rows)))))
mm[t(ji)] <- -1; mm[t(ji - c(0,1))] <- -1; mm[t(ji + c(0,1))] <- -1

mm[mm==-1] <- -999

#Synusoidal river
ab <- sapply(1:n.cols, function(i)
  c(ceiling(n.rows * 0.5 * (1 + exp(-0.5*i/n.cols) * sin(6*i/n.cols))), i))

for (i in seq(-10,10)) {
    ab1<-rbind(ab[1,]+i,ab[2,])
    mm[t(ab1)] <- -1; mm[t(ab1 - c(1,0))] <- -1; mm[t(ab1 + c(1,0))] <- -1
}

#Add houses along the roads
houses_road<-multiexpand(mm,500,cluster.size=9,0,c(n.rows/2,n.cols/2),10000,1,c(5,1),mode="ca",nbr.matrix=nn8,along=TRUE,along.value=-999)

mm[mm==-999] <- -1
mm1 <- matrix(1, n.rows, n.cols)
mm1[which(mm==-1)]<- -1
mm1[which(houses_road>2)]<- -1

#Big public green areas
green_park<-multiexpand(mm1,10,cluster.size=1000,1,c(n.rows/2,n.cols/2),10000,1,c(500,500),mode="pixel",nbr.matrix=nn24)

mm2 <- matrix(1, n.rows, n.cols)
mm2[which(mm==-1)] <- -1
mm2[which(houses_road>2)] <- -1
mm2[which(green_park>2)] <- -1

#Small filling houses
houses_isolated<-multiexpand(mm2,2000,cluster.size=25,cluster.size.prob=0,start=c(30,30),count.max=50000,range=1,contiguity=c(5,5),mode="ca",nbr.matrix=nn24, debug=FALSE)

mm3 <- matrix(1, n.rows, n.cols)
mm3[which(mm==-1)]<--1
mm3[which(houses_road>2)]<--1
mm3[which(houses_isolated>2)]<--1
mm3[which(green_park>2)]<--1

small_houses<-multiexpand(mm3,2000,cluster.size=15,0,c(n.rows/2,n.cols/2),10000,1,c(5,5),mode="ca",nbr.matrix=nn14,along=TRUE,along.value=-1)

mm4 <- matrix(1, n.rows, n.cols)
mm4[which(mm==-1)] <- -1
mm4[which(houses_road>2)] <- -1
mm4[which(houses_isolated>2)] <- -999
mm4[which(small_houses>2)] <- -999
mm4[which(green_park>2)] <- -1

#Fill with green areas
green_private<-multiexpand(mm4,2000,cluster.size=5,0,c(n.rows/2,n.cols/2),50000,1,c(1,1),mode="pixel",nbr.matrix=nn8,along=T,along.value=-999)

mm5 <- matrix(1, n.rows, n.cols)
mm5[which(mm4==1)] <- 9
mm5[which(small_houses > 2)] <- 6
mm5[which(houses_isolated>2)] <- 5
mm5[which(houses_road>2)] <- 4
mm5[which(mm==-1) ] <- 3
mm5[which(green_park>2)] <- 7
mm5[which(green_private>2)] <- 8


for (i in seq(-10,10)) {
    ab1<-rbind(ab[1,]+i,ab[2,])
    mm5[t(ab1)] <- 2; mm5[t(ab1 - c(1,0))] <- 2; mm5[t(ab1 + c(1,0))] <- 2
}

plot(raster(houses_road[n.rows:1,], xmx=n.cols, ymx=n.rows))


b<-
spplot(raster(mm5[n.rows:1,], xmx=n.cols, ymx=n.rows), col.regions=c("blue","black","grey85","grey80","grey75","light green","dark green","white"),at= c(1.9,2.9,3.9,4.9,5.9,6.9,7.9,8.9,9.9), colorkey = list(labels = list( labels = c("River","Road", "Road houses","Isolated Houses","Small Houses","Public Green areas","Private Green Areas"), width = 2, cex = 2, cex.labels=0.5, at = seq(2, 8, 1), col=c("blue","black","grey85","grey80","grey75","light green","dark green","white"))),par.settings=list(fontsize=list(text=5)))

library(gridExtra)

top <- arrangeGrob(a, b, nrow=2)

plot(grid.arrange(top))

# Options
cluster.number=1
cluster.size=10
cluster.size.prob=0
start=c(n.rows/2,n.cols/2)
count.max=2000
range=1
contiguity=c(10,10)
x=mm
mode="ca"
nbr.matrix=nn24
along.value=-999
