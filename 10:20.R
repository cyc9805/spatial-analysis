rm(list=ls())

# 재귀함수로 팩토리얼 표현
sumfunc<-function(num) {
  if(num==1) return(1)
  return(num+sumfunc(num-1))
}
sumfunc(10)


# 히스토그램 그리기

dist<-runif(1000,1,5)
hist(dist,col=gray(0.9),main="uniform on [1,5]")
hist(dist,col=gray(0.9),probability = TRUE)
curve(dunif(x,1,5),from=0,to=6,add=TRUE)
# add=TRUE는 hold on 과 같은 역할


# 정규분포 그래프에 표현하기
v1 = rnorm(n = 100) 
v2 = rnorm(n = 10000)
hist(v1, probability = TRUE, col = gray(0.9), main = "normal mu=0,sigma=1") 
curve(dnorm, add = TRUE)
hist(v2, probability = TRUE, col = gray(0.9), main = "normal mu=0,sigma=1") 
curve(dnorm, add = TRUE)

# rnorm: 정규분포에서 난수 생성
# dnorm: 정규분포에서의 확률밀도함수
# pnorm: 정규분포에서의 누적확률함수
# qnorm: 특정 누적확률에 대한 난수 값

x <- rnorm(1000000)
mean(x)
sd(x)
curve(dnorm(x),from=-8, to=8)

# 0에 대한 확률밀도함수
dnorm(0)

# -0.1~0.1 사이를 뽑을 확률
integrate(dnorm, -0.1, 0.1)

# -1~1 사이를 뽑을 확률 (적분을 이용)
integrate(dnorm, -1, 1)

# -1~1 사이를 뽑을 확률 (누적확률함수를 이용)
pnorm(1)-pnorm(-1)

# 특정 누적확률에 대한 난수 값
qnorm(0.5)

pnorm(3.8,3,0.5)-pnorm(2.18,3,0.5)
