## 21.11.25
## 상관관계(선형적 관계, pearson), 회귀분석(단순 선형회귀), 가설검정

## 1. 피어슨 상관관계 분석
# 두 변수 x, y의 선형 관계성을 계량화한 수치
# 1에 가까울수록, 강한 양의 상관성
# -1에 가까울수록, 강한 음의 상관성
# 0에 가까울수록, 상관성이 없음

y <- c(87,95,72,37,44,24,40,55,55,38,88,34,41,30,26,35,38,24,14,56,37,34,8,18,49,44,51,67,17,37,55,25,33,32,59,54)
x  <- c(72,75,85,29,58,30,50,60,49,46,84,23,21,46,22,42,45,14,19,36,48,23,8,29,38,47,52,52,22,48,58,40,46,38,35,55) 
plot(x,y)
cor(x, y, method="pearson")

## 2. 단순 선형회귀
model <- lm(y~x)
summary(model)
lines(x, rep(mean(y), length(y)))
lines(x, model$fitted.values)
model$coefficients
model$residuals
model$fitted.values


# 결정계수(R^2)
# 모형이 현상(x와 y의 관계)을 얼마만큼 설명할 수 있는가?
# SST: 실측값의 변량들의 제곱합, y가 흩어진 정도
# SSR: 모형이 설명할 수 있는 변량들의 제곱합, 실측값이 흩어진 정도를 모델이 얼마만큼 설명하는가
# SSE: 모형이 설명하지 못하는 변량들의 제곱합
# R^2: SSR/SST = 1-SSE/SST
# Adjusted_R^2: 모델을 설명하는 변수인 R^2에서 독립변수의 수로 받는 영향을 없앤다.

# 표준오차(std. error)
# 표본들의 표준편차
# 추정치의 표준편차
# Residual standard error: 
r.se <- sqrt(sum((model$residuals-mean(model$residual))^2)/(length(y)-2)) 
# 회귀계수의 표준편차: 잔차의 표준편차 / sqrt(독립변수의 총 변량)
# b1의 표준편차: 잔차의 표준편차 / sqrt(x의 편차의 제곱합)
b1.se <- r.se / sqrt(sum((x-mean(x))^2))

# 가설 검정
# 확률변수가 어떠한 분포를 따르는가
# 해당 분포에 맞는 검정통계량
# 모델의 적합성 검증
# 회귀계수의 통계적 유의성 검정
# 귀무가설: b1=0 / 대립가설: b1!=0
# 회귀계수의 검정통계량: (bx-0) / bx의 표준오차
# t-분포는 모집단의 분산(혹은 표준편차)이 알려져 있지 않은 경우에 정규분포 대신 이용하는 확률분포
b.t <- (model$coefficients[2] - 0) / b1.se
c1.t <- qt(0.025, 34)
c2.t <- -c1.t
p <- 2*(1-pt(8.703, 34))
p

# rnorm: 정규분포로 확률변수 생성
# dnorm: 확률밀도함수
# pnorm: 누적확률밀도
# qnorm: 해당 누적확률이 나오는 z값

# 모델의 적합성 검정
# 귀무가설: b0, b1, .... bx 모든 회귀계수가 0이다.
# 대립가설: 적어도 하나의 회귀계수는 0이 아니다.
# 회귀제곱평균 (MSR): SSR / 독립변수의 수, 모형이 설명할 수 있는 수치 (분산)
# 잔차제곱평균 (MSE): SSE / 자유도(n-2), 모형이 설명할 수 없는 수치 (분산)
# F 분포, 서로 다른 집단의 분산의 차이가 있는가? 분산/분산 -> F 통계량을 따른다
# 검정통계량: MSR / MSE, 두 분산의 비율
m.f <- sum((model$fitted.values - mean(y))^2) / (sum(model$residuals^2)/(34))

# F분포는 단측검정 
# rf(뽑는갯수, 분자의 자유도, 분모의 자유도)
f.sample <- rf(1000, 1, 34)
plot(density(f.sample))
c1 <- qf(0.95, 1, 34)
# c1보다 m.f가 더 크므로 대립가설 채택

# p-value가 유의수준보다 낮으면 귀무가설 기각
p <- 1-pf(75.74, 1, 34)


# 라이브러리 
library(raster)

# 가상의 데이터 생성
x <- c(87,95,72,37,44,24,40,55,55,38,88,34,41,30,26,35,38,24,14,56,37,34,8,18,49,44,51,67,17,37,55,25,33,32,59,54)
y  <- c(72,75,85,29,58,30,50,60,49,46,84,23,21,46,22,42,45,14,19,36,48,23,8,29,38,47,52,52,22,48,58,40,46,38,35,55) 


# 벡터를 행렬로 저장
mx <- matrix(ncol=6, nrow=6, x)
my <- matrix(ncol=6, nrow=6, y)

# 행렬을 래스터 데이터로 변환
rx <- raster(mx)
ry <- raster(my)

# 1행 2열로 그래프 구성하기
par(mfrow=c(1,2))
plot(rx,main="Independent raster (x)")
text(rx) # 레이블
plot(ry,main="Dependent raster (y)")
text(ry) # 레이블

# 종속변수
ry@data@values # 또는 간단히 ry[]
# 독립변수
rx@data@values # 또는 간단히 rx[]


## MAUP test
# test1
# 산점도 그리기
plot(rx[], ry[])
# 피어슨 상관계수 구하기 (두 변수가 선형적으로 상관성이 존재하는가?)
cor1 <- cor(rx[], ry[], method="pearson")
cor1
# 회귀분석 (x와 y의 관계, x를 이용하여 y를 예측하기)
model1 <- lm(ry[] ~ rx[])
summary(model1)

## test2
# Aggregate raster cells
# 집계 기준: fact=c(1,2) -> X축 1칸, Y축 2칸 크기로 집계 
rx1 <- aggregate(rx, fact=c(1,2), fun=mean, expand=T)
ry1 <- aggregate(ry, fact=c(1,2), fun=mean, expand=T)

par(mfrow=c(1,2))
plot(rx1,main="Independent raster (x1)")
text(rx1)
plot(ry1,main="Dependent raster (y1)")
text(ry1)
#
plot(rx1[], ry1[])
cor2 <- cor(rx1[], ry1[], method="pearson")
cor2
#
model2 <- lm(ry1[] ~ rx1[])
summary(model2)

## test3
# Aggregate raster cells
# 집계 기준: fact=c(2,1) -> X축 2칸, Y축 1칸 크기로 집계 
rx2 <- aggregate(rx, fact=c(2,1), fun=mean, expand=T)
ry2 <- aggregate(ry, fact=c(2,1), fun=mean, expand=T)

par(mfrow=c(1,2))
plot(rx2,main="Independent raster (x2)")
text(rx2)
plot(ry2,main="Dependent raster (y2)")
text(ry2)

#
plot(rx2[], ry2[])
cor3 <- cor(rx2[], ry2[], method="pearson")
cor3
#
model3 <- lm(ry2[] ~ rx2[])
summary(model3)
















