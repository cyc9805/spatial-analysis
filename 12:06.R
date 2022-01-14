library(sf)
library(spdep)
library(dplyr)
library(tmap)
rm(list=ls())
setwd('/Users/yongchanchun/Desktop/데스크탑 - MacBook Pro/3학년2학기/공간분석론/sample2')

## 데이터 로드
# shp 파일 리드
gu <- st_read('seoul_gu.shp')
dong <- st_read('seoul_dong.shp')
pop <- st_read('seoul_pop_500m.shp')

# csv파일 리드(데이터프레임으로 읽음)
bicycle <- read.csv('seoul_bicycle_2106.csv',fileEncoding = "CP949", encoding = "UTF-8")

## 시각화
tmap_mode('view')
# 자치구 경계
tm_shape(gu)+tm_borders()+tm_text('SIGUNGU_NM')
# 행정동 경계
tm_shape(dong)+tm_borders()
# 자치구와 행정동
tm_shape(gu) + tm_borders(col='red', lwd=2)  + tm_shape(dong) + tm_borders(col='black',lwd=1) 
# 인구
tm_shape(pop) + tm_polygons(col='value', style='pretty', palette='Reds')
qtm(pop, fill='value')
?qtm


pop$geometry
# 공공자전거 대여소
# st_as_sf(df, coords = c(x좌표, y좌표), crs=좌표계ID)
bicycle.sf <- st_as_sf(bicycle, coords=c('x','y'), crs=4326)
tmap_mode('plot')

# by color 
tm_shape(bicycle.sf) + tm_dots(col='count', shape=19, alpha=0.3, size=0.5, style='quantile',
                               palette=c('gray','black'))

# by size
tm_shape(bicycle.sf) +
  tm_bubbles('count', scale=2, shape=19, alpha=0.3, col='red')

# 자치구와 공공자전거 대여소
tm_shape(gu) + tm_borders(col='gray', lwd=2) +
  tm_shape(bicycle.sf) +
  tm_bubbles('count', scale=2, shape=19, alpha=0.3, col='red')

# 자치구와 공공자전거 대여소
tm_shape(pop) + tm_polygons(col='value', style='equal', palette='Reds', alpha=0.8) +
  tm_shape(bicycle.sf) +
  tm_bubbles('count', scale=2.5, shape=19, alpha=0.2, col='black') +
  tm_shape(gu) + tm_borders(col='blue', lwd=2)

### 인구가 공공자전거 이용에 미치는 효과
# 종속변수: 공공자전거 이용 현황
# 독립변수: 인구
# 공간 단위의 통일 -> 동 단위 집계

## 모든 공간 데이터가 동일한 좌표계로 설정되어 있는가?
gu$geometry # Korea 2000 / Unified CS
dong$geometry # Korea 2000 / Unified CS
pop$geometry # Korea 2000 / Unified CS
bicycle.sf$geometry # WGS84

## 좌표변환
# 공공자전거 대여소 좌표계 변환 (WGSS84 -> UTM-K)
bicycle.sf.utmk <- st_transform(bicycle.sf, 5179)

### 공간 데이터 가공
# 공공자전거 대여소에 동코드를 할당하자
bicycle.join <- st_intersection(bicycle.sf.utmk, dong)

# 격자 인구에 동코드를 할당하자
pop.cnt <- st_centroid(pop)
tm_shape(pop.cnt) + tm_bubbles('value', scale=1.5, shape=19, alpha=0.3, col = 'red')
pop.join <- st_intersection(pop.cnt, dong)


# 동 단위로 인구와 공공자전거 이용 횟수를 집계
# summarise(생성할_열이름=집계함수('열이름'))  

dong.bicycle <- bicycle.join %>% group_by(ADM_DR_CD) %>%
  summarise(count=sum(count))
qtm(dong.bicycle, fill='count')
dong.bicycle$geometry

# 인구
dong.pop <- pop.join %>% group_by(ADM_DR_CD) %>%
  summarise(pop=sum(value))
dong.pop$geometry

# 테이블만 추출 -> df
df.dong.bicycle <- st_drop_geometry(dong.bicycle)
df.dong.pop <- st_drop_geometry(dong.pop)

## 조인
var <- inner_join(df.dong.bicycle, df.dong.pop,'ADM_DR_CD' )
dong.join <- inner_join(dong,var,'ADM_DR_CD')
qtm(dong.join, fill='count')
qtm(dong.join, fill='pop')
