rm(list=ls)
## 데이터 가공 (집계, 필터, ...)
library(dplyr)
## R에서 REST API를 이용하기
# 시험엔 안나옴
library(rjson)
# vector 포맷
library(sf)
# 지도 시각화
library(tmap)
# IDW, Kriging
library(gstat)
# raster 포맷
library(raster)
# KDE
library(GISTools)
library(ggplot2)

## 경로 설정
setwd('/Users/yongchanchun/Desktop/데스크탑 - MacBook Pro/3학년2학기/공간분석론/sample3')

## 데이터 로드
air.station <- read.csv("AIR_STATION.csv",fileEncoding = "CP949", encoding = "UTF-8")
air.category <- read.csv("AIR_CATEGORY.csv",fileEncoding = "CP949", encoding = "UTF-8")
air.measure <- read.csv("AIR_HOUR_2020.csv",fileEncoding = "CP949", encoding = "UTF-8")

# 데이터 전처리
air.station[air.station$측정소주소=="서울 노원구 상계로 118 상계2동 주민센터 (23길 17 노원구 원터행복발전소)",
            '측정소주소']='서울 노원구 상계로 118 상계2동 주민센터'

## 브이월드 API를 이용한 지오코딩(주소 등과 같이 좌표를 도출할 수 있는 text)
# 좌표를 저장할 벡터 선언
x <- c()
y <- c()

# 지오코딩
for (i in (1:nrow(air.station))) {
  address <- air.station[i, "측정소주소"]
  url <- paste0("http://api.vworld.kr/req/address?service=address&request=getcoord&version=2.0&crs=epsg:4326&address=", 
                address,
                "&refine=false&simple=false&format=json&type=road",
                "&key=1F4DDEE7-D44A-3260-909E-21B644D2ED2E")
  encode.url <- URLencode(url)
  result <- fromJSON(scan(encode.url, "", sep="\n"))
  
  if(result$response$status == 'OK'){
    x <- rbind(x, as.numeric(result$response$result$point$x))
    y <- rbind(y, as.numeric(result$response$result$point$y))
  }
  else{
    print(address)
  }
}


# 좌표 저장
air.station$X <- x
air.station$Y <- y

# sf(simple feature) 만들기
station.sf <- st_as_sf(air.station, coords = c("X", "Y"), crs = 4326)

# 자치구와 대기오염 측정소
tm_shape(station.sf) +
  tm_dots(shape = 19, alpha = 0.3, size = 0.5)

tmap_mode("view")
gu <-st_read("seoul_gu.shp")
tm_shape(gu) + tm_borders(col="black", lwd=2) +
  tm_shape(station.sf) + tm_dots(col="red")

## 연간 평균 미세먼지 농도
# 측정소 단위 항목별 평균 수치 계산
station.item.avg <- air.measure %>% filter(data_state ==0) %>%
  group_by(loc_code, item_code) %>%
  summarise(avg_value = mean(data_value))
head(station.item.avg)

# '측정소 단위 항목별 평균 수치'와 항목 테이블 조인
# 조인하는 칼럼이 다를때는 두 개 칼럼 이름 모두 넣음
category.join <- inner_join(station.item.avg, air.category, by = c("item_code" = "측정항목코드"))

# 미세먼지 필터
avg.PM10 <- category.join %>% filter(측정항목명=='PM10')
avg.PM10 <- avg.PM10[, 1:4]

# 필터링된 측정소 단위 미세먼지 평균값과 측정소 공간 데이터 조인
station.PM10 <- inner_join(station.sf, avg.PM10, by = c("측정소코드" = "loc_code"))

# 시각화
tmap_mode("plot")
tm_shape(gu) + tm_borders(col="black", lwd=2) +
  tm_shape(station.PM10) + 
  tm_dots("avg_value", shape = 19, size = 0.5, style="pretty", palette=c('Blues'))

### interpolation
## IDW
## interploation을 하려면 반드시 레스터 형태인 빈 그리드를 만들어야 함!!

# 1. 서울시에 대한 빈 그리드 만들기
# 250은 픽셀의 크기
st_bbox(gu)
diff(st_bbox(gu)[c(1, 3)]) / 250
# 148
diff(st_bbox(gu)[c(2, 4)]) / 250
# 121
# 셀의 갯수: 148*121=17908

# sp타입으로 변경
gu.sp <- as_Spatial(gu)
proj4string(gu.sp)


# (포인트 생성 -> 픽셀 타입으로 변환 -> 그리드 타입으로 변환 -> raster()적용) 순서로 반드시 레스터 데이터 생성!
grd <- spsample(gu.sp, "regular", n=17908) # 격자 중심점을 17008개 생성
gridded(grd)     <- TRUE  # 픽셀 타입으로 변환
fullgrid(grd)    <- TRUE  # 그리드 타입 
proj4string(grd) <- proj4string(gu.sp)
proj4string(grd)
plot(grd)
grd.raster <- raster(grd)

tmap_mode('view')
tm_shape(grd.raster) +  
  tm_raster(n=1, col="red", alpha=0.3)

# 2. IDW
st_crs(station.PM10)
station.PM10.utmk <- st_transform(station.PM10, 5179)
station.sp <- as_Spatial(station.PM10.utmk)
proj4string(station.sp) <- proj4string(gu.sp)

pm10.idw <- gstat::idw(avg_value ~ 1, station.sp, newdata=grd, idp=2.0) # station.sp의 avg_value 이용, 
                                                                        # avg_value~1는 avg_value열 하나만 이용한다는 뜻
                                                                      # newdata=grd는 grd위에 IDW를 적용한 값을 생성한다는 뜻 

# GISTools를 이용해 IDW를 수행할때는 반드시 레스터 형태로 작업해야함!
pm10.raster <- raster(pm10.idw)
pm10.seoul <- mask(pm10.raster, gu.sp) # mask(raster, vector) -> 구 영역으로 clip

tmap_mode("plot")
interp.img1 <- 
  tm_shape(pm10.seoul) +  
  tm_raster(n=10,palette = "Reds", 
            title="Predicted PM10") +  
  tm_shape(station.sp) + tm_dots() + 
  tm_legend(legend.outside=TRUE)
interp.img1

# 3. fine-tuning(the choice of the power parameter)
IDW.out <- vector(length = length(station.sp))

# i번째 값을 제외하고 나머지 포인트로 예측한 i번의 예측값 
for (i in 1:length(station.sp)) {
  IDW.out[i] <- idw(avg_value ~ 1, station.sp[-i,], station.sp[i,], idp=9)$var1.pred
}

# Plot the differences

plot(IDW.out ~ station.sp$avg_value, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
# plot(IDW.out,station.sp$avg_value, asp=1, xlab="Observed", ylab="Predicted", pch=16, col=rgb(0,0,0,0.5)) 와 같음

abline(lm(IDW.out ~ station.sp$avg_value), col="red", lw=2,lty=2)
abline(0,1)



# cross-validation
# Implementation of a jackknife technique to estimate 
# a confidence interval at each unsampled point.

# Create the interpolated surface
img <- gstat::idw(avg_value ~ 1, station.sp, newdata=grd, idp=2.0)
n   <- length(station.sp)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)

# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(avg_value~1, station.sp[-i,], newdata=grd, idp=2.0)
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

r <- raster(img.sig, layer="v")
r.m <- mask(r, gu.sp)

# Plot the map
# 신뢰구간이 낮을수록 추정값들을 신뢰할 수 있다!
tm_shape(r.m) + tm_raster(n=7,title="95% confidence interval") +
  tm_shape(station.sp) + tm_dots() +
  tm_legend(legend.outside=TRUE)

## Kriging
## 시험에 안나옴!
# 1st order polynomial fit
# Define the 1st order polynomial equation
# avg_value를 독립변수 X, Y 두개로 설명함
f.1 <- as.formula(avg_value ~ X + Y) 

# Add X and Y to station.sp
station.sp$X <- coordinates(station.sp)[,1]
station.sp$Y <- coordinates(station.sp)[,2]


# Run the regression model
lm.1 <- lm(f.1, data=station.sp)
# Use the regression model output to interpolate the surface
dat.1st <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.1, newdata=grd)))

# Clip the interpolated raster to Texas
r   <- raster(dat.1st)
r.m <- mask(r, gu.sp)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, auto.palette.mapping=TRUE, 
            title="Predicted PM10") +
  tm_shape(station.sp) + tm_dots() +
  tm_legend(legend.outside=TRUE)

# Kriging
# Define the 1st order polynomial equation
f.1 <- as.formula(avg_value ~ X + Y) 

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl <- variogram(f.1, station.sp, c)
plot(var.smpl,pch=20,cex=1.5,col="black")

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill = 4.5, model="Sph", range=7000, nugget=0))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit, xlim=c(0,12000))

# X,Y 열을 grd에 추가
grd <- as.data.frame(spsample(gu.sp, "regular", n=17908)) # 격자 중심점을 17008개 생성
names(grd) <- c('X','Y')
coordinates(grd) <- c('X','Y')
gridded(grd)     <- TRUE  # 픽셀 타입으로 변환
fullgrid(grd)    <- TRUE  # 그리드 타입 

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige(f.1, station.sp, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, gu.sp)

# Plot the map
interp.img2 <- 
  tm_shape(r.m) + 
  tm_raster(n=10,palette = "Reds", 
            title="Predicted PM10") +
  tm_shape(station.sp) + tm_dots() +
  tm_legend(legend.outside=TRUE)


tmap_arrange(interp.img1, interp.img2)

############ KDE ############
## 데이터 로드 및 가시화
cctv <- st_read("CCTV_point.shp")
dong <- st_read("seoul_dong.shp")

tm_shape(dong) + tm_borders(col="black") +
  tm_shape(cctv) + tm_dots(col="red")

## 행정동별로 cctv 개수는 어떻게 분포할까?
st_crs(cctv)
st_crs(dong)

cctv.utmk <- st_transform(cctv, 5179)
dong.cctv <- st_intersection(cctv.utmk, dong)

dong.cctv.count <- dong.cctv %>% group_by(ADM_DR_CD) %>%
  summarise(count = n()) # 행의 개수를 카운트할 때
summary(dong.cctv.count$count)

hist(dong.cctv.count$count)
hist(dong.cctv.count$count, breaks = seq(from=0, to=50, by=2))     # by가 간격을 결정
hist(dong.cctv.count$count, breaks = seq(from=0, to=50, by=2), probability = TRUE)

## 집계를 위한 공간 단위
## 연속적인 분포 형태
## KDE
d <- density(dong.cctv.count$count, bw=10, kernel="gaussian") # bw가 클수록 스무딩이 더 많이 됨
lines(d, col = "red")

# 커널 함수: 관측값이 차지하는 밀도를 계산하는 방법 -> gaussian, uniform, triangle등 다양하게 존재

## 각 cctv 주변으로 얼마나 많은 cctv들이 분포하고 있을까?
cctv.utmk$X <- st_coordinates(cctv.utmk)[,1]
cctv.utmk$Y <- st_coordinates(cctv.utmk)[,2]

# Set up base layer
base <- ggplot(data=cctv.utmk, aes(x=X, y=Y))
# Create the KDE surface -> stat_density 는 2차원 밀도 함수를 생성
kde1 <- base + geom_point(size=0.5) +
  stat_density2d(aes(x = X, y = Y, alpha = ..level..), 
                 size = 0.01, geom = 'polygon') +
  scale_fill_gradient() 
kde1

# using GISTools
cctv.sp <- as_Spatial(cctv.utmk)

kde2 <- kde.points(cctv.sp)
level.plot(kde2)

r <- raster(kde2)
r.m <- mask(r, gu.sp)

map1 <- tm_shape(r.m) + 
  tm_raster(n=10,palette = "Reds") +
  tm_shape(cctv.sp) + tm_dots()

# bandwitdth를 더 작게 설정
kde2 <- kde.points(cctv.sp, h=2000)
level.plot(kde2)

r <- raster(kde2)
r.m <- mask(r, gu.sp)

map2 <- tm_shape(r.m) + 
  tm_raster(n=10,palette = "Reds") +
  tm_shape(cctv.sp) + tm_dots()

tmap_arrange(map1, map2)
