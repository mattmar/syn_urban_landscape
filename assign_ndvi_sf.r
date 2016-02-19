# begin with a sintetic landscape and add ndvi values according to the value measured in LS for the same categories
mmndvi <- mmf
mmndvi[mmf==1] <- sapply(1:length(mmf[mmf==1]),function(x) x <- rtnorm(1,-0.11,0.17,-1,1))
mmndvi[mmf==2] <- sapply(1:length(mmf[mmf==2]),function(x) x <- rtnorm(1,-0.11,0.17,-1,1))
mmndvi[mmf==3] <- sapply(1:length(mmf[mmf==3]),function(x) x<-rtnorm(1,-0.004,0.25,-1,1))
mmndvi[mmf==4] <- sapply(1:length(mmf[mmf==4]),function(x) x<-rtnorm(1,-0.004,0.25,-1,1))
mmndvi[mmf==5] <- sapply(1:length(mmf[mmf==5]),function(x) x<-rtnorm(1,-0.004,0.25,-1,1))
mmndvi[mmf==6] <- sapply(1:length(mmf[mmf==6]),function(x) x<-rtnorm(1,0.30,0.24,0,1))
mmndvi[mmf==7] <- sapply(1:length(mmf[mmf==7]),function(x) x<-rtnorm(1,0.14,0.30,0,1))

#Assign projection to the matrix
sf_lulc<-raster(mmf[n.rows:1,], xmx=n.cols, ymx=n.rows)
crs(sf_lulc)<-"+proj=utm  +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
sf_ndvi<-raster(mmndvi[n.rows:1,], xmx=n.cols, ymx=n.rows)
crs(sf_ndvi)<-"+proj=utm  +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#Save image
save.image("/incoming/marcantonio/syn_landscape/sanfrancisco.Rdata")


# Load it locally
load("/incoming/marcantonio/syn_landscape/sanfrancisco.Rdata")

#Plot landscape and ndvi
png("~/Desktop/urb_land_aggregate.png",width = 300, height = 300, units="mm",res=300,pointsize=10)
par(mfrow=c(2,1),mar = c(5,3,5,7))
raster::plot(raster(mmf[n.rows:1,]),breaks=c(0.9,1.9,2.9,3.9,4.9,5.9,6.9,7.9),col=c("brown","black","grey85","grey80","grey75","dark green","light green"),legend=FALSE,xlim=c(0,1),useRaster=TRUE,ylim=c(0,1),main="SF simulated classified",cex.main=2)
raster::plot(raster(mmf[n.rows:1,]), legend.only=TRUE, col=c("black","blue","grey95","grey90","grey75","light green","dark green","brown"), legend.width=1, legend.shrink=0.9, axis.args=list(at=seq(1, 7, length.out=8), labels=c("Road", "River", "Small houses","Medium Houses","Big Houses","Pub Green areas","Priv Green Areas","Barren surface"), cex.axis=1),
   legend.args=list(text='', side=4, font=2, line=1, cex=1),smallplot=c(0.88,0.89, 0.14,0.87),add=T); par(mar = par("mar"))
raster::plot(raster(mmndvi[n.rows:1,]),main="SF simulated NDVI",cex.main=2)
dev.off()

#Write rasters
writeRaster(sf_lulc,"~/Desktop/PhD/topics/urban_veg_albo/syn_matrices/syn_lulc_sf.tif",format="GTiff",overwrite=TRUE)
writeRaster(sf_ndvi,"~/Desktop/PhD/topics/urban_veg_albo/syn_matrices/syn_ndvi_sf.tif",format="GTiff",overwrite=TRUE)
