library(geodaData)
library(sf)
library(spdep)
library(tmap)
rm(list=ls())
# 클리브랜드 주택 판매 데이터
clev.points <- clev_pts
class(clev.points)
clev.points <- st_as_sf(clev.points) 
# 시각화
tmap_mode("view") # Interactive maps(배경지도 위에 오버레이)
# tmap_mode("plot") # 일반 모드

tm_shape(clev.points) +
  tm_dots(size = 0.1, shape = 19, col = "sale_price", style="quantile", n=5, palette=c("blue","red"))

# 좌표계 메타 데이터 확인
clev.points$geometry # Projected CRS: NAD83 / Ohio North (ftUS) 좌표계
st_crs(clev.points$geometry)
# 좌표계 재정의가 필요하다면, 
# https://epsg.io/에 접속해서 좌표계 검색
# Projected CRS: NAD83 / Ohio North (ftUS) -> 3734
# 좌표계 재정의
st_crs(clev.points) = 3734

# 1. 공간가중행렬 생성
# https://spatialanalysis.github.io/lab_tutorials/Spatial_Weights_as_Distance_Functions.html

# 거리를 이용한 인접성 정의
# 적당히 가까운 거리에 있는 포인트들을 이웃으로 정의하고, 가중치는 거리에 반비례하도록 
# 적당히 가까운 거리??


# 포인트 좌표 추출
coords <- cbind(clev.points$x,clev.points$y)

# 가장 가까운 포인트까지 거리 계산
clev.points$rname <- rownames(clev.points)
clev.points$rname
tmap_mode('plot')
tm_shape(clev.points) +
  tm_dots(size = 0.1, shape = 19, col = "sale_price", style="quantile", palette=c("blue","red")) +
  tm_text("rname", size=1)

# knearneigh(coords): k개의 근접 이웃 검색, knn 클래스 타입. default는 k=1
# knn2nb: knn 클래스 타입을 nb 클래스(이웃이 정의된) 타입으로 변환
k1 <- knn2nb(knearneigh(coords, k=1))

# nbdists(k1,coords): coords에서 정의된 각 좌표에서 k1에서 정의된 이웃(들)까지 거리 계산 
# unlist: 리스트를 벡터로
nbdists(k1,coords)

# 임계거리 설정
critical.threshold <- max(unlist(nbdists(k1,coords))) # 3600m

# 각 포인트에서 가장 가까운 이웃 포인트는 최대 3598m에 위치함
# 따라서, 특정 포인트로부터 3598m 거리 안에 위치한 포인트들은 이웃들로 정의할 수 있음
# 적당히 가까운 거리에 위치한 이웃 포인트들 찾기 clear

# 모든 이웃 포인트들까지의 거리 계산

# 임계거리 내에 위치한 이웃들 검색
# dnearneigh(좌표, lower bound, upper bound)
nb.dist.band <- dnearneigh(coords, 0, critical.threshold)

# 임계거리 내에 위치한 이웃들까지의 거리계산
distances <- nbdists(nb.dist.band, coords)

# 가중치, 즉, 거리 반비례 값 계산
invd1 <- lapply(distances, function(x) (1/x))
invd1 # 가중치가 너무 작아지는 문제
invd <- lapply(distances, function(x) (100/x)) # 적당한 상수배
invd


# 공간가중행렬 생성
# nb2listw는 주변 이웃들간의 거리를 공간가중행렬로 만들어줌 
# nb2listw(이웃, glist=가중치)
invd.weights <- nb2listw(nb.dist.band, glist = invd, style = "B")
invd.weights$weights

# 2. Global Moran's I 
# Szero computes the total sum of weight matrix
I <- moran(clev.points$sale_price, invd.weights, length(k1), Szero(invd.weights))[1] 
I


# 'I는 0보다 크다'가 대립가설
moran.test(clev.points$sale_price, invd.weights, alternative="greater") 

# 3. local Moran's I
# local moran은 0보다 커질수록 clustered, 작을수록 dispersed
moran <- moran.plot(clev.points$sale_price, invd.weights) 

local <- localmoran(clev.points$sale_price, invd.weights)
local

# attr(데이터, 속성이름)
attr(local, "quadr")

# 클러스터 형성된 정도(quadr)는 attr(local, "quadr")[,3]에 저장
clev.points$quadr <- attr(local, "quadr")[,3]
clev.points$pval <- local[,5]
clev.points$cluster <- ifelse(clev.points$pval < 0.05, as.character(clev.points$quadr), "Not Sig.")
clev.points$cluster 

map1 <- tm_shape(clev.points) +
  tm_dots(size = 0.05, shape = 19, col = "sale_price", style="quantile", palette=c("blue","red"))
map1

map2 <- tm_shape(clev.points) +
  tm_dots(size = 0.05, shape = 19, col = "cluster", )
map2

tmap_arrange(map1, map2)

