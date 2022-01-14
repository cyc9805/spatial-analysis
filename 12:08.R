##################################################### 

# ----------- Kernel Density Estimation ------------# 

##################################################### 


# Non-parametric method to estimate the probability density function 

# Data smoothing process 


# mtcar 는 1974년에 여러 브랜드 차종의 도로 주행 테스트에 대한 dataframe임 

mtcars 





# Histogram 

x <- mtcars$mpg 

hist(x) 



# KDE 

d <- density(x) 

plot(d) 





# Using rectangle kernel 
# bandwidth 파라미터로서 커널이 뽀족한 형태(h가 작은 값)인지 완만한 형태(h가 큰 값)인지를 조절하는 파라미터이다

d1 <- density(x, bw=1, kernel="rectangular") 

plot(d1, main="KDE of Miles per galone") 

polygon(d1, col="lightblue", border="blue") 

hist(x, freq=F, border="red", lty=3, col=rgb(1,0,0,0.2), add=T) 

rug(x) 



d2 <- density(x, bw=4, kernel="rectangular") 

plot(d2, main="KDE of Miles per galone") 

polygon(d2, col="lightblue", border="blue") 

hist(x, freq=F, border="red", lty=3, col=rgb(1,0,0,0.2), add=T) 

rug(x) 



# Using gaussian kernel 

d1 <- density(x, bw=1, kernel="gaussian") 

plot(d1) 

polygon(d1, col="lightblue", border="blue") 

hist(x, freq=F, border="red", lty=3, col=rgb(1,0,0,0.2), add=T) 

rug(x) 



d2 <- density(x, bw=4, kernel="gaussian") 

plot(d2) 

polygon(d2, col="lightblue", border="blue") 

hist(x, freq=F, border="red", lty=3, col=rgb(1,0,0,0.2), add=T) 

rug(x) 


# ===================================== 

# ----- KDE in 2D --------------------- 

# ===================================== 



x <- c(3,5,5,6,12,14,15,15,16,17) 

y <- c(3,4,15,5,5,7,10,11,12,15) 



pts <- cbind(x, y) 

plot(x, y, col="blue", asp=1, pch=19, add=T, xlim=c(0,20), ylim=c(0,20)) 



library(MASS) 

pts.kde <- kde2d(pts[,1], pts[,2], h=4, n=40, lims=c(0,20,0,20))   # from MASS package 


# ===================================================== 

# -------------- KDE in Spatial Data ------------------ 

# ===================================================== 





## 'KDE Maps to Compare Forced and Non-Forced Burglary Patterns'---- 

require(GISTools) 

data(newhaven) 



# Set up parameters to create two plots side by side, 

opar = par() 



par(mfrow=c(1,2),mar=c(1,1,2,1)) 

# Compute Density for forced entry burglaries and create plot 

brf.dens <- kde.points(burgres.f,lims=tracts) 

level.plot(brf.dens) 

# Use 'masking' as before 

masker <- poly.outer(brf.dens,tracts,extend=100) 

add.masking(masker) 

plot(tracts,add=TRUE) 

# Add a title 

title("Forced Burglaries") 





# Compute Density for non-forced entry burglaries and create plot 

brn.dens <- kde.points(burgres.n,lims=tracts) 

level.plot(brn.dens) 

# Use 'masking' as before 

masker <- poly.outer(brn.dens,tracts,extend=100) 

add.masking(masker) 

plot(tracts,add=TRUE) 

# Add a title 

title("Non-Forced Burglaries") 



par = opar 

# Contour plot overlaid on heat map image of results 

image(pts.kde, xlim=c(0,20), ylim=c(0,20)) # from base graphics package 

contour(pts.kde, add = T)          # from base graphics package 



class(pts.kde) 

names(pts.kde) 





# base graphics package 

persp(pts.kde, phi = 45, theta = 30, shade = .1, border = NA)  



# RGL interactive plot 

install.packages("rgl") 

library(rgl) 

col2 <- heat.colors(length(pts.kde$z))[rank(pts.kde$z)] 

persp3d(x=pts.kde, col = col2) 



# First, let’s load the data from the website. The data are stored as  

# SpatialPointsDataFrame and SpatialPointsDataFrame objects.  

# Most of the functions used in this exercise work off of these classes.  

# The one exception is the direchlet function which requires a conversion  

# to a ppp object. 


library(rgdal) 

library(tmap) 


# Load precipitation data 

z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/precip.rds")) 

P <- readRDS(z) 



# Load Texas boudary map 

z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/texas.rds")) 

W <- readRDS(z) 



# Replace point boundary extent with that of Texas 

P@bbox <- W@bbox 



tm_shape(W) + tm_polygons() + 
  
  tm_shape(P) + 
  
  tm_dots(col="Precip_in", palette = "RdBu", auto.palette.mapping = FALSE, 
          
          title="Sampled precipitation \n(in inches)", size=0.7) + 
  
  tm_text("Precip_in", just="left", xmod=.5, size = 0.7) + 
  
  tm_legend(legend.outside=TRUE) 







######################################################################### 

# ------------------------- Thiessen polygons --------------------------- 

######################################################################### 

# The Thiessen polygons (or proximity interpolation) can be created using  

#spatstat’s dirichlet function. 



# install packages if necessary 

# install.packages("spatstat") 



library(spatstat)  # Used for the dirichlet tessellation function 

library(maptools)  # Used for conversion from SPDF to ppp 

library(raster)    # Used to clip out thiessen polygons 



# Create a tessellated surface 

th  <-  as(dirichlet(as.ppp(P)), "SpatialPolygons") 



# The dirichlet function does not carry over projection information 

# requiring that this information be added manually 

proj4string(th) <- proj4string(P) 



# The tessellated surface does not store attribute information 

# from the point data layer. We'll use the over() function (from the sp 

# package) to join the point attributes to the tesselated surface via 

# a spatial join. The over() function creates a dataframe that will need to 

# be added to the `th` object thus creating a SpatialPolygonsDataFrame object 

th.z     <- over(th, P, fn=mean) 

th.spdf  <-  SpatialPolygonsDataFrame(th, th.z) 



# Finally, we'll clip the tessellated  surface to the Texas boundaries 

th.clp   <- raster::intersect(W,th.spdf) 



# Map the data 

tm_shape(th.clp) +  
  
  tm_polygons(col="Precip_in", palette="RdBu", auto.palette.mapping=FALSE, 
              
              title="Predicted precipitation \n(in inches)") + 
  
  tm_legend(legend.outside=TRUE) 









######################################################################### 

# ------------------------------ IDW ------------------------------------ 

######################################################################### 



# The IDW output is a raster. This requires that we first create an empty  

# raster grid, then interpolate the precipitation values to each unsampled  

# grid cell. An IDW power value of 2 (idp=2.0) will be used. 



# install packages if necessary 

# install.packages("gstat") 



library(gstat) # Use gstat's idw routine 

library(sp)    # Used for the spsample function 
library(tmap)


# Create an empty grid where n is the total number of cells 

grd              <- as.data.frame(spsample(P, "regular", n=50000)) 

names(grd)       <- c("X", "Y") 

coordinates(grd) <- c("X", "Y") 

gridded(grd)     <- TRUE  # Create SpatialPixel object 

fullgrid(grd)    <- TRUE  # Create SpatialGrid object 



# Add P's projection information to the empty grid 

proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted 

proj4string(grd) <- proj4string(P) 



# Interpolate the grid cells using a power value of 2 (idp=2.0) 

P.idw <- gstat::idw(Precip_in ~ 1, P, newdata=grd, idp=2.0) 



# Convert to raster object then clip to Texas 

r       <- raster(P.idw) 

r.m     <- mask(r, W) 



# Plot 

tm_shape(r.m) +  
  
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE, 
            
            title="Predicted precipitation \n(in inches)") +  
  
  tm_shape(P) + tm_dots(size=0.2) + 
  
  tm_legend(legend.outside=TRUE) 





# ---------------------------------- 

# Fine-tuning the interpolation 

# ---------------------------------- 



# The choice of power function can be subjective.  

# To fine-tune the choice of the power parameter, you can perform  

# a leave-one-out validation routine to measure the error in the  

# interpolated values. 





# Leave-one-out validation routine 

IDW.out <- vector(length = length(P)) 

for (i in 1:length(P)) { 
  
  IDW.out[i] <- idw(Precip_in ~ 1, P[-i,], P[i,], idp=2.0)$var1.pred 
  
} 



# Plot the differences 

OP <- par(pty="s", mar=c(4,3,0,0)) 

plot(IDW.out ~ P$Precip_in, asp=1, xlab="Observed", ylab="Predicted", pch=16, 
     
     col=rgb(0,0,0,0.5)) 

abline(lm(IDW.out ~ P$Precip_in), col="red", lw=2,lty=2) 

abline(0,1) 

par(OP) 





# The RMSE can be computed from IDW.out as follows: 



# Compute RMSE 

sqrt( sum((IDW.out - P$Precip_in)^2) / length(P)) 





# ---------------------------------- 

# Cross-validation 

# ---------------------------------- 

# In addition to generating an interpolated surface, you can create  

# a 95% confidence interval map of the interpolation model. Here we’ll  

# create a 95% CI map from an IDW interpolation that uses a power parameter  

# of 2 (idp=2.0). 





# Implementation of a jackknife technique to estimate  

# a confidence interval at each unsampled point. 



# Create the interpolated surface 

img <- gstat::idw(Precip_in~1, P, newdata=grd, idp=2.0) 

n   <- length(P) 

Zi  <- matrix(nrow = length(img$var1.pred), ncol = n) 



# Remove a point then interpolate (do this n times for each point) 

st <- stack() 

for (i in 1:n){ 
  
  Z1 <- gstat::idw(Precip_in~1, P[-i,], newdata=grd, idp=2.0) 
  
  st <- addLayer(st,raster(Z1,layer=1)) 
  
  # Calculated pseudo-value Z at j 
  
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred 
  
} 



# Jackknife estimator of parameter Z at location j 

Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n ) 



# Compute (Zi* - Zj)^2 

c1 <- apply(Zi,2,'-',Zj)            # Compute the difference 

c1 <- apply(c1^2, 1, sum, na.rm=T ) # Sum the square of the difference 



# Compute the confidence interval 

CI <- sqrt( 1/(n*(n-1)) * c1) 



# Create (CI / interpolated value) raster 

img.sig   <- img 

img.sig$v <- CI /img$var1.pred  



# Clip the confidence raster to Texas 

r <- raster(img.sig, layer="v") 

r.m <- mask(r, W) 



# Plot the map 

tm_shape(r.m) + tm_raster(n=7,title="95% confidence interval \n(in inches)") + 
  
  tm_shape(P) + tm_dots(size=0.2) + 
  
  tm_legend(legend.outside=TRUE) 



######################################################################### 

# ----------------------------- Kriging --------------------------------- 

######################################################################### 



library(rgdal) 

library(tmap) 

library(gstat) 

library(raster) 



# Load precipitation data 

z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/precip.rds")) 

P <- readRDS(z) 



# Load Texas boudary map 

z <- gzcon(url("https://github.com/mgimond/Spatial/raw/main/Data/texas.rds")) 

W <- readRDS(z) 





library(gstat) # Use gstat's idw routine 

library(sp)    # Used for the spsample function 



# Create an empty grid where n is the total number of cells 

grd              <- as.data.frame(spsample(P, "regular", n=50000)) 

names(grd)       <- c("X", "Y") 

coordinates(grd) <- c("X", "Y") 

gridded(grd)     <- TRUE  # Create SpatialPixel object 

fullgrid(grd)    <- TRUE  # Create SpatialGrid object 



# Add P's projection information to the empty grid 

proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted 

proj4string(grd) <- proj4string(P) 





# ---------------------------------- 

# Fit the variogram model 

# ---------------------------------- 

# First, we need to create a variogram model. Note that the variogram model 

# is computed on the de-trended data. This is implemented in the following  

# code by passing the 1st order trend model to the variogram function. 





# Define the 1st order polynomial equation 

# precip = intercept + aX + bY 

f.1 <- as.formula(Precip_in ~ X + Y)  



# Add X and Y to P 

P$X <- coordinates(P)[,1] 

P$Y <- coordinates(P)[,2] 





# Compute the sample variogram; note that the f.1 trend model is one of the 

# parameters passed to variogram(). This tells the function to create the  

# variogram on the de-trended data. 

var.smpl <- variogram(f.1, P, cloud = FALSE, cutoff=1000000, width=89900) 



# Compute the variogram model by passing the nugget, sill and range values 

# to fit.variogram() via the vgm() function. 

dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE, 
                          
                          vgm(psill=14, model="Sph", range=590000, nugget=0)) 



# The following plot allows us to assess the fit 

plot(var.smpl, dat.fit, xlim=c(0,1000000)) 



# ---------------------------------- 

# Generate Kriged surface 

# ---------------------------------- 



# Next, use the variogram model dat.fit to generate a kriged interpolated surface. 

# The krige function allows us to include the trend model thus saving us from  

# having to de-trend the data, krige the residuals, then combine the two rasters. 

# Instead, all we need to do is pass krige the trend formula f.1. 



# Define the trend model 

f.1 <- as.formula(Precip_in ~ X + Y)  



# Perform the krige interpolation (note the use of the variogram model 

# created in the earlier step) 

dat.krg <- krige(f.1, P, grd, dat.fit) 



# Convert kriged surface to a raster object for clipping 

r <- raster(dat.krg) 

r.m <- mask(r, W) 



# Plot the map 

tm_shape(r.m) +  
  
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE,  
            
            title="Predicted precipitation \n(in inches)") + 
  
  tm_shape(P) + tm_dots(size=0.2) + 
  
  tm_legend(legend.outside=TRUE) 



# --------------------------------------------------- 

# Generate the variance and confidence interval maps 

# --------------------------------------------------- 

# The dat.krg object stores not just the interpolated values, but the variance 

# values as well. These can be passed to the raster object for mapping as  

# follows: 



r   <- raster(dat.krg, layer="var1.var") 

r.m <- mask(r, W) 



tm_shape(r.m) +  
  
  tm_raster(n=7, palette ="Reds", 
            
            title="Variance map \n(in squared inches)") +tm_shape(P) + tm_dots(size=0.2) + 
  
  tm_legend(legend.outside=TRUE) 





# Are more readily interpretable map is the 95% confidence interval map which  

# can be generated from the variance object as follows (the map values should  

# be interpreted as the number of inches above and below the estimated rainfall  

# amount). 





r   <- sqrt(raster(dat.krg, layer="var1.var")) * 1.96 

r.m <- mask(r, W) 



tm_shape(r.m) +  
  
  tm_raster(n=7, palette ="Reds", 
            
            title="95% CI map \n(in inches)") +tm_shape(P) + tm_dots(size=0.2) + 
  
  tm_legend(legend.outside=TRUE) 

