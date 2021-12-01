# data
head(iris)
dataset<-iris
levels(iris$Species)

# 산점도 (scatter plot)
plot(x=iris$Petal.Width, y=iris$Petal.Length, main='꽃잎에 따른 데이터 분포',
     xlab = '꽃잎의 너비', ylab='꽃잎의 길이',
     pch=20, col = 'red')

# 꽃잎의 종류에 따라 색깔과 모양을 다르게 표시함
plot(x=iris$Petal.Width, y=iris$Petal.Length, main='꽃잎에 따른 데이터 분포',
     xlab = '꽃잎의 너비', ylab='꽃잎의 길이',
     pch=as.numeric(iris$Species), col = iris$Species)

# legend
legend('topleft', legend=levels(iris$Species), 
       pch=19, col=factor(levels(iris$Species)))


# 이산 확률 변수의 히스토그램
# 이산 확률 변수
y <- rbinom(10,20,0.25)

# bar plot을 이용한 이산 확률 변수의 Frequency 표현
sample <- table(y)

# 정답을 맞춘 횟수/카운트
sample <- as.data.frame(table(y))
sample

# barplot
barplot(height=sample$Freq, names.arg = sample$y, 
        main='맞춘 정답별 횟수', 
        xlab='정답 카운트',
        ylab='Frequency')

barplot(height=table(y), names.arg = names(table(y)), 
        main='맞춘 정답별 횟수', 
        xlab='정답 카운트',
        ylab='Frequency')

names(table(y))
# 도수분포

