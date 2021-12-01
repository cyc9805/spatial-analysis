# 이항분포
## 20문제 시험을 모두 10회 본다면 각 몇 문제가 맞출것인가? 무작위 실험
x <- rbinom(10,20,0.25)
mean(x)
sd(x)
hist(x,col=gray(0.9), probability = TRUE)
curve(dnorm(x,mean=5,sd=2), from=0, to=20, add=TRUE)

## 시행 횟수를 늘리면 히스토그램과 커브가 서로 일치해짐
x <- rbinom(1000,20,0.25)
mean(x)
sd(x)
hist(x,col=gray(0.9), probability = TRUE)
curve(dnorm(x,mean=5,sd=2), from=0, to=20, add=TRUE)



# 포아송 분포
rm(list=ls())
y <- rpois(1000,3)
mean(y)
sd(y)
hist(y, col=gray(0.9), probability = TRUE)
curve(dnorm(x, mean=3, sd=2), from=0, to=20, add=TRUE)


# iris 데이터
iris <- iris
unique(iris$Species)
head(iris)

## iris 데이터에서 무작위 다섯개 데이터 추출
iris[sample(1:nrow(iris),5),]


# 동전 던지기 함수 cointoss
cointoss <- function(n) {
count <- 0
for (i in 1:n) count <- count + ifelse(runif(1) <= 0.5, 1, 0) 
return(count/n)}

cointoss(100)
cointoss(1000)
cointoss(10000)

sapply(10^(1:5), cointoss)
plot(sapply(10^(1:6), cointoss))


# 파이 구하기 함수
getpi <- function(n){
  count <- 0
  for(i in 1:n){
    coord <- runif(2, -1, 1)
    if((coord[1]^2+coord[2]^2)<=1) count <- count + 1
  }
  return (4 * count / n)
}

sapply(10^(1:5), getpi)
