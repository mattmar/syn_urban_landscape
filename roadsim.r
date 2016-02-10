roadsim <- function(x,n1,n2,rvalue=-1,method="row",sl=0.5,fr=0.5,scaling=0.5,increase=0) {
    if( method=="row" & increase==0 ) {
       ij <- sapply(1:n2, function(i)
        c(ceiling(n1 * scaling * (1 + exp(sl*i/n2) * sin(fr*i/n2))), i))
       x[t(ij)] <- rvalue; x[t(ij - c(1,0))] <- rvalue; x[t(ij + c(1,0))] <- rvalue
   } else if( method=="row" & increase!=0 ) {
    ij <- sapply(1:n2, function(i)
        c(ceiling(n1 * scaling * (1 + exp(sl*i/n2) * sin(fr*i/n2))) ,i))
    for (i in seq(-increase,increase)) {
        ij1 <- rbind(ij[1,]+i,ij[2,])
        x[t(ij1)] <- rvalue; x[t(ij1 - c(1,0))] <- rvalue; x[t(ij1 + c(1,0))] <- rvalue
    }
} else if( method=="column" & increase==0 ) {
   ij <- sapply(1:n2, function(i)
    c(i,ceiling(n1 * scaling * (1 + exp(sl*i/n2) *sin(fr*i/n2)))))
   x[t(ij)] <- rvalue; x[t(ij - c(0,1))] <- rvalue; x[t(ij + c(0,1))] <- rvalue
} else if( method=="column" & increase!=0 ) {
    ij <- sapply(1:n2, function(i)
        c(i,ceiling(n1 * scaling * (1 + exp(sl*i/n2) * sin(fr*i/n2)))))
    for (i in seq(-increase,increase)) {
        ij1 <- rbind(ij[1,],ij[2,]+i)
        x[t(ij1)] <- rvalue; x[t(ij1 - c(0,1))] <- rvalue; x[t(ij1 + c(0,1))] <- rvalue
    }
}
return(x)
}

# sc<-0.1;sl<-1.5;j<-0.5; x<-c(); for (i in 1:300){x[i]<-300*sc*(1 + exp(sl*i/300)*sin(j/300)*ifelse(rbinom(1,1,0.2),rnorm(1,1,0.5),1))};plot(x,type="l")
# j<-4; x<-c(); for (i in 1:300){x[i]<-300*j*(1+exp(j*i/300))};plot(x)
# j<-15; x<-c(); for (i in 1:300){x[i]<-300*j*sin(j*i/300)};plot(x)
