multiexpand <- function(x,cluster.number,cluster.size,cluster.size.prob=0,start,count.max,range=1,contiguity,mode="px",nbr.matrix=matrix(c(-1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1), nrow=2),xnnoise=0, ynnoise=0, along=FALSE, along.value=0, debug=0) {

#Required packages
  require(msm)

#Prepare output objects and temporary vectors
  n.rows <<- dim(x)[1]
  n.cols <<- dim(x)[2]
  nbrhood <- nbr.matrix
  n <- n.rows * n.cols
  cells.left <- 1:n
  cells.left[x!=1 | x==along.value] <- -1 # Occupancy of cells
  i <- 2 # index for cluester.size comparison
  indices <- c() # vector for occupied cells
  ids <- c()
  out <- matrix(NA, n.rows, n.cols)
  busy_cells <- 0
  fc <- rep(NA,cluster.size)
  state <- data.frame(occupied=which(x==-1 | x==along.value))
  add_sd_storage <- c()
  h <- 1 # counter for synusoidal growth
#
# If `cells_selected` is missing
#
  if( missing(start) ) {
    rst <- which(x==1,arr.ind=T)
    start <- rst[sample(1:dim(rst)[1], 1),]
  }

#Start the loop
  while( i < cluster.number+2 && length(cells.left[x!=-1]) >= cluster.size && count.max > 1 ) {

    if(count.max==2) break("No solution found to build the planned number of clusters")

#
#Check if cluster size exceed the limit
#
      if(cluster.size>=n/2)break("cluster.size must be less then the cells number/2")
#
#Preparation of the starting cell; for every while loop the new seed is randomly drawn from a truncated normal distribution which average=previous column and row coordinates while sd=defined by the user. Furthermore, if "range">0, the new seed is randomly picked from all the neighbourous cells in range.
#
        count.max <- count.max-0.5
      cells_start <- cells.left[which(cells.left > 0)]
      cells_selected <- 0

      if(debug==1 & i>2){
        print(paste("bench_1: 1st loop","fc",length(fc),"cluster",cluster.size-1,"count.max", count.max,start[1],start[2],cells_selected))
      }

      while( cells_selected == 0 | length(fc) < cluster.size-1 & count.max > 0 ) {

        count.max <- count.max-1
        if(count.max<2) next("No solution found: Seed coords constantly outside matrix extent or cluster size not respected")

         if( cluster.size<=1 & cluster.size.prob==0 ) stop("Mean<=1 and SD==0; impossible to draw values from rtnorm")

           cluster.size<-round(rtnorm(1,cluster.size,cluster.size.prob,lower=1, upper=n.rows*n.cols/2),0)

         if( i !=2 ) {
          start=which(tail(state$occupied, 1)==matrix(seq(1,n),nrow=n.rows,ncol=n.cols,byrow=TRUE),arr.ind=T)
        }

        if( along==TRUE & exists("along.value") ) {
          start=which(x==along.value,arr.ind=T)[sample(1:length(which(x==along.value)),range),]
        }

        add_sd <- rbinom(1,10,1/log(ifelse(count.max<2,2,count.max),2))

        if( all(start<=1) & any(contiguity + add_sd==0 )) break("Mean<=1 and SD==0; impossible to draw values from rtnorm")

          start_coords <- round(rtnorm(2, mean=as.numeric(start), sd=contiguity + add_sd, lower=1, upper=min(dim(x))+1),0) #start seed from TND
        matrix_start_coords <- matrix(cells.left,nrow=n.rows,ncol=n.cols,byrow=TRUE)

        if(debug==1){
          print(paste("bench_2: 1st loop","fc",length(fc),"cluster size",cluster.size-1,"count.max", count.max,start_coords[1],start_coords[2]),cells.left)
        }

#
# Check if seeds range is outside x
#
        while( count.max>0 & start_coords[1]+range > dim(x)[1] | start_coords[2]+range > dim(x)[2] | start_coords[1]-range <= 0 | start_coords[2]-range <= 0 ) {

          if(debug==1){
           print(paste("bench_3: 3rd loop","fc",length(fc),"cluster",cluster.size-1,"range",range, "start coords:",start_coords[1],start_coords[2],"count max",count.max))
         }
#
# If the seed gets stuck in not valid coordinates, apply a further sd to the folded normal distribution to help finding valid coordinates; the additional sd is function of count.max
#
         count.max <- count.max-1

         if(count.max<2) break("No solution found: Seed coords constantly outside matrix extent")

           add_sd1 <- rbinom(1,5,1/log(ifelse(count.max<2,2,count.max), 2))

         if( all(start<=1) & any(contiguity + add_sd1==0) ) break("Mean<=1 and SD==0; impossible to draw values from rtnorm")

           start_coords <- round(rtnorm(2, mean=as.numeric(start), sd=contiguity + add_sd1, lower=1, upper=min(dim(x))+1),0)
       }

       cells_selected <- matrix_start_coords[(start_coords[1]-range):(start_coords[1]+range),(start_coords[2]-range):(start_coords[2]+range)] #Neighborhood cell_selected
       cells_selected<-cells_selected[cells_selected>0]
#
#If range>0 randomly pick a seed cell in the neighborhood set
#
       if( length(cells_selected)==0 ) cells_selected<-0;

       if( length(cells_selected)>1 ){
        cells_selected<- sample(cells_selected, 1)
        start_coords<-which(cells_selected==matrix_start_coords,arr.ind=T)
      }
#
# If ca then cluster.size must be respected
#
      if( mode=="ca" & cluster.size.prob==0 ){
        fc <- c((cells_selected-1)%%n.rows+1, floor((cells_selected-1)/n.rows+1)) + nbr.matrix
        fc <- fc[, fc[1,] >= 1 & fc[2,] >= 1 & fc[1,] <= n.rows & fc[2,] <= n.cols,
        drop=FALSE]
        fc <- fc[1,] + (fc[2,]-1)*n.rows
        fc <- fc[matrix_start_coords[fc]!=-1]
        fc <- setdiff(fc,state$occupied)
      } else {fc <- rep(0,cluster.size-1)
    }
  }

  if(count.max<2) break("No solution found: Seed coords constantly outside matrix extent")

#
#Expand a patch randomly within indicator array `x` (1=unoccupied) by `cluster.size` cells beginning at index `start_coords`.
#
#
#Adjoin one more random cell and update `state`, which records (1) the immediately available cells and (2) already occupied cells.
#
    grow <- function(state,cells_selected) {
#
# Find all available neighbors that lie within the extent of `x` and
# are unoccupied.
#

      neighbors <- function(i) {

#
# In case of syn growth
#
        if( xnnoise>0 | ynnoise>0 ){
          nbr <- as.matrix(nbrhood[,nbrhood[1,]>=0 & nbrhood[2,]>=0])
        }

        if( ynnoise>0 ){
          n <- c((i-1)%%n.rows+1, floor((i-1)/n.rows+1)) + nbr # extract nn matrix
          noise<-round(ifelse(ynnoise_r>0, 1 + exp(ynnoise_r*h/h) * sin(h),0))
          noise<-ifelse(noise>1,1,ifelse(noise<-1,-1))
          n[1,] <- if(rbinom(1,1,0.50)==0) {
            n[1,] + noise
          } else{n[1,] - noise # add synusoidal noise
        }
      } else {
        if( xnnoise>0 ) {
          n <- c((i-1)%%n.rows+1, floor((i-1)/n.rows+1)) + nbr # extract nn matrix
          noise<-round(ifelse(xnnoise_r>0, 1 + exp(xnnoise_r*h/h) * sin(h),0))
          noise<-ifelse(noise>1,1,ifelse(noise<-1,-1))
          n[2,] <- if(rbinom(1,1,0.50)==0) {
            n[2,] + noise
          } else{n[2,] - noise # add synusoidal noise
        }
      } else { n <- c((i-1)%%n.rows+1, floor((i-1)/n.rows+1)) + nbrhood
    } # extract nn matrix
  }

  n <- n[, n[1,] >= 1 & n[2,] >= 1 & n[1,] <= n.rows & n[2,] <= n.cols, drop=FALSE] # Remain inside the extent of `x`.
  n <- n[1,] + (n[2,]-1)*n.rows  # Convert to *vector* indexes into `x`.
  n <- n[x[n]==1] # Stick to valid cells in `x`.
  n <- setdiff(n, state$occupied)# Remove any occupied cells.

  return (n)
}
#
# Select one available cell uniformly at random.
# Return an updated state.
#
if( mode=="ca" ){

  if( length(state$occupied)>1 & length(state$available)>1 ) {
    j <- ceiling(runif(1) * length(state$available))
    a <- state$available[j]
    return(list(index=a,
      available = state$available[-j],
      occupied = c(state$occupied, a)))
  }

  a <- state$available
  return(list(index=a,
    available = union(state$available[-1], neighbors(a)),
    occupied = c(state$occupied, a)))
}

if( mode=="px" ){
  j <- ceiling(runif(1) * length(state$available))
  a <- state$available[j]
  return(list(index=a,
    available = union(state$available[-j], neighbors(a)),
    occupied = c(state$occupied, a)))
}
}

state <- list(available=cells_selected, occupied=busy_cells)
#
# Grow for as long as possible and as long as needed.
#
a <- 1
indices.c <- c(NA, cluster.size)
while( length(state$available) >= 1 && a <= cluster.size ) {
  xnnoise_r <- runif(1,0,xnnoise)
  ynnoise_r <- runif(1,0,ynnoise)

  h <- h+1 # counter for syn
  state <- grow(state,cells_selected)
  indices.c[a] <- state$index
  a <- a+1 # index for cluster size comparison
  count.max <- count.max-0.5

  if( debug==1 ) {
    print(paste("bench_4: 4th loop","available cells",length(state$available),"cluster.size",cluster.size-1,a,"cluster n",i,"count max",count.max))
  }
}
#
# Return a grid of generation numbers from 1, 2, ... through cluster.size.
#
indices.c <- indices.c[!is.na(indices.c)]
y <- matrix(NA, n.rows, n.cols)
y[indices.c] <- 1:length(indices.c)

#
# If not syn growth set then check if the cluster respects cluster.size
#
if(xnnoise==0 & ynnoise==0){
  if ( length(y[!is.na(y)])==cluster.size ) {
    i <- i+1
    ids <- c(ids, rep(i, cluster.size))
    indices <- c(indices, which(!is.na(y)))
    cells.left[indices] <- -1 #Indicate occupacy

    cat(paste(i-2,"cluster(s) created. Cluster size=",cluster.size,"x=",start_coords[1],"y=",start_coords[2],length(which(cells.left>0)),"cells unoccupied \n", sep=" "))

  } else {
   next("No cluster created. Cluster smaller than cluster.size. Jumping to the next iteration")
   count.max <- count.max-1
 }
} else {
  i <- i+1
  ids <- c(ids, rep(i, length(y[!is.na(y)])))
  indices <- c(indices, which(!is.na(y)))
  cells.left[indices] <- -1 #Indicate occupacy
  count.max <- count.max -1
  cat(paste(i-2,"cluster(s) created. Cluster size=",length(y[!is.na(y)]),"x=",start_coords[1],"y=",start_coords[2],length(which(cells.left>0)),"cells unoccupied \n", sep=" "))
}
#
#Check if the left cells are enough for the next cluster
#
if( length(which(cells.left!=-1))<=cluster.size ) {
  break(paste("Unoccupied cells are not enough","only",i-2,"cluster(s) created out of",cluster.size,"\n",sep=" "))
  i=cluster.number
  count.max <- count.max-1
}

busy_cells<-indices
}
out[indices] <- ids
out[out %in% NA] <- 1
return(out)
}
