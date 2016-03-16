#Built a simple matrix
n.rows <- 500
n.cols <- 500
mm <- matrix(1, n.rows, n.cols)

#Set seeds
set.seed(3003)

#Add landscape shapes
# Main Roads
mm1<-roadsim(mm,n.rows,n.cols,-1,method="row",sl=0,fr=0,scaling=0.3,increase=0)
mm3<-roadsim(mm1,n.cols,n.rows,-1,method="column",sl=0,fr=0,scaling=0.9,increase=2)

#Add big public green areas
green_park<-multiexpand(mm3,6,cluster.size=1500,100,c(n.rows/2,n.cols/2),50000,100,c(5000,5000),mode="ca",ww=16)
 summary(as.factor(green_park>2))
mm3b <- matrix(1, n.rows, n.cols)
mm3b[mm3 == -1 | green_park > 2] <- -1
mm3b[mm1 == -1] <- -999

#Minor roads
mm3d<-roadsim(mm3b,n.cols,n.rows,-1,method="column",sl=0,fr=0,scaling=0.5,increase=0)
mm3f<-roadsim(mm3d,n.cols,n.rows,-999,method="row",sl=0,fr=0.1,scaling=0.8,increase=0)

#Add secondary roads
mm4 <- multiexpand(mm3f,3,100,0,c(n.rows/2,n.cols/2),10000,1,c(500,500),"px",nbr.matrix="nn2x",xnnoise=-1,along=TRUE,along.value=-999)

mm4b <- matrix(1, n.rows, n.cols)
mm4b[ mm3f == -1 ] <- -999
mm4b[ mm3f == -999 | green_park >2 ] <- -1

mm8 <- multiexpand(mm4b,3,100,0,c(n.rows/2,n.cols/2),20000,1,c(500,500),"px",nbr.matrix="nn2y",ynnoise=-1,along=TRUE,along.value=-999)

mm9 <- matrix(1, n.rows, n.cols)
mm9[ mm3f == -999 | mm3f == -1 | mm4 >2 | mm8 >2 ] <- -999
mm9[ green_park > 2 ] <- -1

summary(as.factor(mm9==-999))

#Add houses along the roads
houses_road_b<-multiexpand(mm9,500,cluster.size=101,0,c(n.rows/2,n.cols/2),500000,1,c(0.1,0.1),mode="ca",ww=10,along=TRUE,along.value=-999)

mm11 <- matrix(1, n.rows, n.cols)
mm11[mm9 == -1 ] <- -1
mm11[mm9 == -999 | houses_road_b > 2] <--999

#Small filling houses
houses_road_m<-multiexpand(mm11,1500,cluster.size=64,cluster.size.prob=0,start=c(n.rows/2,n.cols/2),count.max=500000,range=1,contiguity=c(0.1,0.1),mode="ca",ww=8,along=TRUE,along.value=-999)

mm12 <- matrix(1, n.rows, n.cols)
mm12[mm11 == -1] <- -1
mm12[mm11 == -999 | houses_road_m >2] <- -999

houses_road_s<-multiexpand(mm12,3000,cluster.size=16,0,start=c(n.rows/2,n.cols/2),500000,1,c(0.1,0.1),mode="ca",ww=4,along=TRUE,along.value=-999)

mm13 <- matrix(1, n.rows, n.cols)
mm13[mm9 == -1 | mm9 == -999] <- -1
mm13[houses_road_b >2 | houses_road_m >2 | houses_road_s >2] <- -999

#Fill with green areas
green_private<-multiexpand(mm13,7000,cluster.size=5,0,c(250,250),100000,1,c(0.1,0.1),mode="px",ww=4,along=T,along.value=-999)

mmf <- matrix(1, n.rows, n.cols)
mmf[ which(green_private > 2) ] <- 7
mmf[ which(green_park > 2) ] <- 6
mmf[ which(houses_road_b > 2) ] <- 5
mmf[ which(houses_road_m > 2) ] <- 4
mmf[ which(houses_road_s > 2) ] <- 3
mmf[ which(mm9==-999 | mm8 >2 | mm4 >2) ] <- 2

save.image("/incoming/marcantonio/syn_landscape/fresno.Rdata")

# Cells
# 500*100+1500*64+3000*16+7500+25000+18000

# Options
# cluster.number=1
# cluster.size=100
# cluster.size.prob=0
# start=c(n.rows/2,n.cols/2)
# count.max=2000
# range=1
# contiguity=c(5,5)
# x=mm
# mode="ca"
# nbr.matrix=nn8
# along.value=-999
# ynnoise=1
# along=T
# debug=1


