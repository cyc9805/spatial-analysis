kor <- sample(50:100, 5) 
eng <- sample(50:100, 5)
df.score <- data.frame(kor, eng) 
df.score
if(df.score$kor[1] > 70) cat(df.score$kor[1],':pass\n')

x <- sample(0:100, 500, replace=TRUE)

# round(input, 소수점 자리수): 반올림 함수
y <- round(runif(500,0,100),0)

# rnorm(샘플 수, 평균, 표준편차): 정규분포를 따르는 난수 생성 
z <- rnorm(500, 50, 5)

plot(density(x, bw = 100))
plot(density(y))
plot(density(z))

if(df.score$kor[1] > 70) cat(df.score$kor[1], ': Pass\n')


if(df.score$kor[5]>70) {
  cat("Pass\n")
} else{
    cat("Fail\n")
}

for(i in 1:length(kor)) {
  if(kor[i]>70) cat("kor",kor[i],":Pass",'\n')
  else cat("kor",kor[i],":Fail",'\n')
}

