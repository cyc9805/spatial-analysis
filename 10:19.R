gender <- c('M','F','F','M','F') 
df.score <- data.frame(df.score, gender)
df.score
tapply(df.score$eng, df.score$gender, mean)

install.packages('STAT')
library(STAT)
aggregate()


# 사용자 정의 함수
tofaren = function(celcius) {
  farenheit = (9/5) * celcius + 32
  return (farenheit)
}
tofaren(21)

toCelcius = function(farenheit) {
  celcius = (farenheit-32) * (5/9)
  celcius = round(celcius, digits=2)
  return (celcius)
}
toCelcius(72:79)
toCelcius(80)


# 반복문을 사용하지 않고 짝수만 추출
v<-1:1000
v[v%%2==0]

# 평균열 추가
kor<-sample(50:100, 5)
eng<-sample(50:100, 5)
df.score<-data.frame(kor,eng)

df.score$avg <- rowMeans(df.score)
df.score$grade <- ifelse(df.score$avg>70,'pass','fail')
df.score


# 앞에서 만든 avg, grade 열을 제거한다. 
df.score$avg <- NULL 
df.score$grade <- NULL
apply(df.score, 1, sum)
apply(df.score, 1, mean)
apply(df.score, 2, mean)


# 사용자 정의 함수 사용
apply(df.score, 1, function(x) x)
apply(df.score, 1, function(x) sum(x))
apply(df.score, 1, function(x) mean(x))

apply(df.score, 2, function(x) x)
apply(df.score, 2, function(x) sum(x))
apply(df.score, 2, function(x) mean(x))

apply(df.score, 1, function(x) ifelse(x > 70, "Pass", "Fail"))
apply(df.score, 1, function(x) ifelse(mean(x) > 70, "Pass", "Fail"))
apply(df.score, 2, function(x) length(x[x > 70]))
