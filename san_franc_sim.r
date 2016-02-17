#Built a simple matrix
n.rows <- 500
n.cols <- 500
mm <- matrix(1, 500, 500)

#Add landscape shapes
#Roads and river
mm1<-roadsim(mm,n.rows,n.cols,-999,method="row")
mm2<-roadsim(mm1,n.rows,n.cols,-999,method="row",sl=2.5,fr=0.6,scaling=0.1)
mm3<-roadsim(mm2,n.cols,n.rows,-999,method="column")
mm4<-roadsim(mm3,n.cols,n.rows,-999,method="column",sl=-3.1,fr=-4.8,scaling=0.8)
mm5<-roadsim(mm4,n.cols,n.rows,-1,method="row",sl=0,fr=1,scaling=0.3,increase=5)

# mm4<-roadsim(mm3,n.cols,n.rows,rvalue=-1,method="column",sl=-0.5,fr=5,scaling=0.5,increase=10)

#Add secondary roads
mm6 <- multiexpand(mm5,100,1000,0,c(n.rows/2,n.cols/2),10000,100,c(50,50),"px",nbr.matrix="nn2x",xnnoise=0,along=TRUE,along.value=-999)

mm7 <- matrix(1, n.rows, n.cols)
mm7[mm5 == -999 | mm6 > 2] <- -999
mm7[mm5 == -1 ] <- -1

mm7b <- multiexpand(mm7,100,1000,0,c(n.rows/2,n.cols/2),10000,100,c(50,50),"px",nbr.matrix="nn2y",ynnoise=0, along=TRUE,along.value=-999)

mm7c <- matrix(1, n.rows, n.cols)
mm7c[mm5 == -999 ] <- -999
mm7c[mm5 == -1 | mm6 >2 | mm7b > 2] <- -1

mm7d <- multiexpand(mm7c,200,1000,0,c(n.rows/2,n.cols/2),10000,100,c(100,100),"px",nbr.matrix="nn2x",xnnoise=-1,along=TRUE,along.value=-999)

mm7e <- matrix(1, n.rows, n.cols)
mm7e[mm7d > 2 ] <- -999
mm7e[mm5 == -999 | mm7c == -1] <- -1

mm8 <- multiexpand(mm7e,200,1000,0,c(n.rows/2,n.cols/2),10000,50,c(50,50),"px",nbr.matrix="nn2y",ynnoise=-1,along=TRUE,along.value=-999)

mm9 <- matrix(1, n.rows, n.cols)
mm9[mm7e == -1 | mm7e == -999 | mm8 > 2] <- -1

#Big public green areas
green_park<-multiexpand(mm9,50,cluster.size=350,350,c(n.rows/2,n.cols/2),50000,200,c(5000,5000),mode="ca",ww=16)

mm9bis <- matrix(1, n.rows, n.cols)
mm9bis[mm9 == -1| green_park > 2] <- -1

green_park1<-multiexpand(mm9bis,2,cluster.size=10000,100,c(0,500),50000,100,c(200,200),mode="ca",ww=40)

mm10 <- matrix(1, n.rows, n.cols)
mm10[mm5 == -1 | green_park > 2 | green_park1 > 2] <- -1
mm10[mm6 >2 | mm7b > 2 | mm7c == -999 | mm7e == -999 | mm8 > 2] <- -999

#Add houses along the roads
houses_road_b<-multiexpand(mm10,10000,cluster.size=36,0,c(n.rows/2,n.cols/2),50000,1,c(0.1,0.1),mode="ca",ww=6,along=TRUE,along.value=-999)

mm11 <- matrix(1, n.rows, n.cols)
mm11[mm10 == -1 ] <- -1
mm11[mm10 == -999 | houses_road > 2] <--999

#Small filling houses
houses_road_b<-multiexpand(mm11,1000,cluster.size=25,cluster.size.prob=0,start=c(n.rows/2,n.cols/2),count.max=100000,range=2,contiguity=c(5,5),mode="ca",nbr.matrix="nn24")

mm12 <- matrix(1, n.rows, n.cols)
mm12[mm11 == -1] <- -1
mm12[mm11 == -999 | houses_road_b >2] <- -999

houses_road_s<-multiexpand(mm12,1000,cluster.size=15,0,start=c(n.rows/2,n.cols/2),50000,50,c(100,100),mode="ca",nbr.matrix="nn14",along=TRUE,along.value=-999)

mm13 <- matrix(1, n.rows, n.cols)
mm13[mm9 == -1 | mm9 == -999] <- -1
mm13[houses_road >2 | houses_road_b >2 | houses_road_s >2] <- -999

#Fill with green areas
green_private<-multiexpand(mm13,5000,cluster.size=5,1,c(n.rows/2,n.cols/2),100000,50,c(50,50),mode="px",nbr.matrix="nn8",along=T,along.value=-999)

mmf <- matrix(1, n.rows, n.cols)
mmf[ which(green_private>2) ] <- 7
mmf[ which(green_park>2) ] <- 6
mmf[ which(houses_road > 2) ] <- 5
mmf[ which(houses_road_b > 2) ] <- 4
mmf[ which(houses_road_s > 2) ] <- 3
mmf[ which(mm9==-1 | mm9==-999) ] <- 2

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
along=T
debug=1
