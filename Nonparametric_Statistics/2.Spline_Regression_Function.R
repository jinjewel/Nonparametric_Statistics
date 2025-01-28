### 3주차 

### R실습 1 : 불연속 1차 조각다항식을 보통 기저함수로 적합
rm(list = ls())

## 함수 만들기
my_function = function(x)
{  
  f = NULL
  for(i in 1:length(x))
  {
    if(x[i] < 0)
      f[i] = -1 - 4 * x[i]
    else
      f[i] = 8 + 3 * x[i]
  }
  return(f)
}

## 데이터 만들기
n = 200
x = seq(-3, 3, length = n)
f = my_function(x) # 함수 f 표시
y = f + rnorm(n) # 데이터 생성

## 그래프 그리기
# 방법1 : f가 불연속이라 점으로 표현
plot(x, y, col = "gray") # 데이터 좌표 표시
points(x, f, cex = 0.1) # 함수 그리기
# 방법 : f가 불연속이라 선으로 그리려면 구간을 나눠야 함
plot(x, y, col = "gray") # 데이터 좌표 표시
lines(x[x<0], f[x<0], cex = 0.1) # 함수 그리기
lines(x[x>0], f[x>0], cex = 0.1) # 함수 그리기

## 1차 조각다항식 기저함수 생성
B = matrix(0, n, 4) # 0으로 초기화한 n*4 기저행렬 생성 
B[, 1][x < 0] = 1
B[, 2][x < 0] = x[x < 0]
B[, 3][x >= 0] = 1
B[, 4][x >= 0] = x[x >= 0]

## 기저함수 그래프에 표시
# 방법1 : 점으로 그리기
plot(0, 0, xlim = c(-3,3), ylim = c(-3, 3))
points(x, B[, 1], col = "black")
points(x, B[, 2], col = "red")
points(x, B[, 3], col = "green")
points(x, B[, 4], col = "blue")
# 방법2 : 구간별로 선으로 그리기
plot(0, 0, xlim = c(-3,3), ylim = c(-3, 3), cex=0.01)
lines(x[x<0], B[, 1][x<0], col = "black")
lines(x[x>=0], B[, 1][x>=0], col = "black")
lines(x[x<0], B[, 3][x<0], col = "green")
lines(x[x>=0], B[, 3][x>=0], col = "green")
plot(0, 0, xlim = c(-3,3), ylim = c(-3, 3), cex=0.01)
lines(x[x<0], B[, 2][x<0], col = "red")
lines(x[x>=0], B[, 2][x>=0], col = "red")
lines(x[x<0], B[, 4][x<0], col = "blue")
lines(x[x>=0], B[, 4][x>=0], col = "blue")
plot(0, 0, xlim = c(-3,3), ylim = c(-3, 3), cex=0.01)
lines(x[x<0], B[, 1][x<0], col = "black")
lines(x[x>=0], B[, 1][x>=0], col = "black")
lines(x[x<0], B[, 2][x<0], col = "red")
lines(x[x>=0], B[, 2][x>=0], col = "red")
lines(x[x<0], B[, 3][x<0], col = "green")
lines(x[x>=0], B[, 3][x>=0], col = "green")
lines(x[x<0], B[, 4][x<0], col = "blue")
lines(x[x>=0], B[, 4][x>=0], col = "blue")
# 방법3 : matplot() 함수 사용하여 그리기
matplot(x, B, type= "l", lwd = 2) # 선 타입과 색상 등을 다르게 표현해준다.

## 적합하기
# lm() 함수를 이용한 적합
fit_lm = lm(y ~ x)
summary(fit_lm)
# 직접 만든 기저 함수(행렬)를 이용한 적합
fit_B = lm(y ~ B - 1) # y 절편은 기저함수를 통해 표시 했으므로 빼야해서 -1을 한다.
summary(fit_B) # 적합한 결과(각 기저 함수의 절편을 보기 위해)
## CDA 함수를 구현하여 적합하기
CDA = function(x, y, epsilon = 10^(-5), max = 10000)
{
  B = matrix(0, n, 4) # 0으로 초기화한 n*4 기저행렬 생성 
  B[, 1][x < 0] = 1
  B[, 2][x < 0] = x[x < 0]
  B[, 3][x >= 0] = 1
  B[, 4][x >= 0] = x[x >= 0]
  beta = c(0, 0, 0, 0) # 초기 bete값 생성
  residual = y # 가능
  R_beta_old = Inf # 점점 작아질 예정이므로 초기값은 무한대로 설정
  
  for(r in 1:max) # stopping rule에 해당될 때까지 반복하기 위해 max로 설정
  {
    for(j in 1:4) # 1차 방정식의 기저가 2개 이므로[ B1() = 1, B2() = x ] j는 2까지 반복
    {
      partial_residual = residual + beta[j] * B[, j] # 부분잔차 계산, y_ij(r)
      beta[j] = sum(partial_residual * B[, j]) / sum(B[, j]^2)
      residual = partial_residual - beta[j] * B[, j]
    }
    
    # stopping rule
    R_beta = 0.5 * sum( (y - B %*% beta)^2 ) # 잔차 = y - B * 베타
    
    if(abs(R_beta - R_beta_old) < epsilon)
      break
    else
      R_beta_old = R_beta
  }
  cat(c("number of iterations = ", r, "\n"))
  return(beta)  
}
fit_CDA = CDA(x, y, epsilon = 1e-10)
fit_CDA

## 적합한 직선을 그래프에 표현
plot(x, y, col = "gray") # 데이터 표현
points(x, f, col = "black", cex = 0.1) # 실제 함수 표현
points(x, fit_lm$fitted.values, col = "green", cex = 0.1) # lm() 함수로 적합한 선
points(x, fit_B$fitted.values, col = "red", cex = 0.1) # 1차 조각다항식 기저함수로 적합한 선
lines(x, B %*% fit_CDA, col = "blue", cex = 0.1) # CDA로 적합한 선
legend("topleft", legend = c("true f", "fit_lm", "fit_B_lm", "fit_B_CDA")
                , col = c("black", "green", "red", "blue")
                , lty = c(3,3,3,1))




### R실습 2 : 2차함수를 절단(함수) 기저함수로 적합
rm(list = ls())

## 데이터 생성
n = 100
x = seq(0.01, 1, length = n)
f = 10 - 15 * (x - 0.5)^2 # 실제 함수(연속된 1차 조각다항식)
y = f + rnorm(n) # 데이터 생성

## 그래프에 데이터 및 함수 표시 
plot(x, y, col = "gray")
lines(x, f)

## 절단함수를 이용한 1차 조각다항식 기저함수 생성
B = matrix(0, n, 3) # 0으로 초기화한 n*3 행렬 생성
B[, 1] = 1 # 전범위에 영향을 미침
B[, 2] = x # 전범위에 영향을 미침
B[, 3][x >= 0.5] = x[x >= 0.5] - 0.5 # 부분(x>=0.5)에만 영향을 미침

## 그래프에 기저함수 표기
matplot(x, B, type = "l", lwd = 2, ylim = c(-0.25, 1.25), xlim = c(-0.25, 1.25))
abline(h=0, v=0)

## 회귀적합
# 1차 lm() 함수를 적합
fit_lm_1 = lm(y ~ x)
summary(fit_lm_1)
# 2차 lm() 함수를 적합
fit_lm_2 = lm(y ~ cbind(x,x^2))
summary(fit_lm_2)
# 절단함수를 이용한 기저함수로 적합
fit_B = lm(y ~ B - 1) # y 절편은 기저함수를 통해 표시 했으므로 빼야해서 -1을 한다.
summary(fit_B)

## 그래프에 적합한 회귀직선을 표현
plot(x, y, col = "gray")
lines(x, f)
lines(x, fit_lm_1$fitted.values, col = "red")
lines(x, fit_B$fitted.values, col = "blue")
points(x, fit_lm_2$fitted.values, cex=0.2, col = "black")
legend("topright", legend = c("true f", "fit_lm_1", "fit_B", "fit_lm_2")
                 , col = c("black", "red", "blue", "pink")
                 , lty = c(1,1,1,3))



### R실습 3 : cos함수와 데이터 생성
rm(list = ls())
## 데이터 생성
n = 200
x = seq(-2, 2, length = n) # length를 붙여야지 길이로 인식
f = cos(pi * x)
e = rnorm(n, 0, sd = 0.3)
y = f + e
## 그래프에 생성한 데이터 및 함수 그리기
plot(x, y, col="gray")
lines(x, f)



### R실습 4 : 매듭점이 (-1,0,1)인 1차 스플라인 함수의 기저함수 생성 
## 기존의 방법을 이용한 기저함수 생성
B1 = matrix(0, n, 5)
B1[,1] = 1
B1[,2] = x
B1[,3][x >= -1] = x[x >= -1] + 1 
B1[,4][x >= 0] = x[x >= 0]
B1[,5][x >= 1] = x[x >= 1] - 1
matplot(x, B1, type="l", lwd=2)
legend("topleft", legend = c("1", "x", "(x+1)_+", "(x)_+", "(x-1)_+")
       , lty = c(1,2,3,4,5)
       , lwd = c(2,2,2,2,2)
       , col = c("black","red","green","blue","skyblue"))

## 절단함수를 만들어서 기저함수 생성
plus = function(x)
{
  plus_x = x
  plus_x[plus_x < 0] = 0
  return(plus_x)
}
B2 = cbind(1, x, plus(x + 1), plus(x), plus(x - 1))
matplot(x, B2, type="l", lwd=2)
legend("topleft", legend = c("1", "x", "(x+1)_+", "(x)_+", "(x-1)_+")
       , lty = c(1,2,3,4,5)
       , lwd = c(2,2,2,2,2)
       , col = c("black","red","green","blue","skyblue"))



### R실습 5 : 만들어진 기저함수를 통해 데이터에 적합 
## B1 기저함수를 통한 적합
fit_B1 = lm(y ~ B1 - 1) # y안에서 인터셉터가 1로 설정이 되어있기 때문에 1을 빼준다.
fit_B1$coefficients
# 1번째 B1는 1번째 직선의 y절편 값
# 2번째 B2는 1번째 직선의 기울기
# 3번째 B3는 1번째 직선의 기울기에서 2번째 직선의 기울기까지의 변화량
# 4번째 B4는 2번째 직선의 기울기에서 3번째 직선의 기울기까지의 변화량
# 5번째 B5는 3번째 직선의 기울기에서 4번째 직선의 기울기까지의 변화량
## 적합한 회귀직선을 그래프에 표현
plot(x, y, col="gray")
lines(x, f, type="l", lwd=2) # 실제 f 함수
lines(x, fit_B1$fitted.values, col="blue") # 기저함수로 적합
legend("bottomright", legend = c("f_real", "f_linear_B1")
       , lty = c(1,1)
       , lwd = c(2,2)
       , col = c("black","blue"))

## B2 기저함수를 통한 적합
fit_B2 = lm(y ~ B2 - 1) # y안에서 인터셉터가 1로 설정이 되어있기 때문에 1을 빼준다.
fit_B2$coefficients
# 1번째 B는 1번째 직선의 y절편 값
# 2번째 Bx는 1번째 직선의 기울기
# 3번째 B는 1번째 직선의 기울기에서 2번째 직선의 기울기까지의 변화량
# 4번째 B는 2번째 직선의 기울기에서 3번째 직선의 기울기까지의 변화량
# 5번째 B는 3번째 직선의 기울기에서 4번째 직선의 기울기까지의 변화량
## 적합한 회귀직선을 그래프에 표현
plot(x, y, col="gray")
lines(x, f, type="l", lwd=2) # 실제 f 함수
lines(x, fit_B2$fitted.values, col="red") # 기저함수로 적합
legend("bottomright", legend = c("f_real", "f_linear_B2")
       , lty = c(1,1)
       , lwd = c(2,2)
       , col = c("black","red"))

## 적합한 회귀직선 2개를 그래프에 표현
plot(x, y, col="gray")
lines(x, f, type="l", lwd=2) # 실제 f 함수
points(x, fit_B1$fitted.values, cex=0.5,  col="blue") # 기저함수로 적합
lines(x, fit_B2$fitted.values, col="red") # 기저함수로 적합
legend("bottomright", legend = c("f_real", "f_linear_B1", "f_linear_B2")
       , lty = c(1,3,1)
       , lwd = c(2,2,2)
       , col = c("black","blue","red"))



### R실습 6: cubic spline(3차) 조각다항식을 이용하여 적합
rm(list = ls())
## 데이터 생성
n = 200
x = seq(-2, 2, length = n)
f = cos(pi * x)
e = rnorm(n, 0, sd=0.3)
y = f + e
## 그래프에 생성한 데이터 및 함수 그리기
plot(x , y, col="gray")
lines(x, f, lwd = 2)
## 기저 함수를 만들기 위한 절단 함수 생성
plus = function(x)
{
  plus_x = x
  plus_x[plus_x < 0] = 0
  return(plus_x)
}
## 기저함수 생성
# 매듭점은 -1, 0, 1 총 3개로 설정한다.
# 총 기저 함수는 7개가 필요하다( 3 + 1 + 3)
B = cbind(1, x, x^2, x^3, plus(x+1)^3, plus(x)^3, plus(x-1)^3) # P차 이므로 plus에 ^p을 꼭 해야한다.
matplot(x, B, type="l", lwd=2)
legend("topleft", legend = c("1", "x", "x^2", "x^3", "{(x+1)_+}^3", "{(x)_+}^3", "{(x-1)_+}^3")
       , lty = c(1,2,3,4,5,1,2)
       , lwd = c(2,2,2,2,2,2,2)
       , col = c("black","red","green","blue","skyblue","purple","black"))
# 기본 기저 함수(3+1개)
matplot(x, B[, 1:4], type="l", lwd=2)
legend("topleft", legend = c("1", "x", "x^2", "x^3")
       , lty = c(1,2,3,4)
       , lwd = c(2,2,2,2)
       , col = c("black","red","green","purple"))
# 스플라인 기저 함수(3개)
matplot(x, B[, 5:7], type="l", lwd=2)
legend("topleft", legend = c("{(x+1)_+}^3", "{(x)_+}^3", "{(x-1)_+}^3")
       , lty = c(1,2,3)
       , lwd = c(2,2,2)
       , col = c("black","red","green"))
## 생성한 기저함수를 이용하여 적합
fit_B1 = lm(y ~ B - 1)
fit_B1$coefficients
# 1번째 B는 1번째 직선의 y절편 값
# 2번째 Bx는 1번째 직선의 기울기
# 3번째 B는 1번째 직선의 기울기에서 2번째 직선의 기울기까지의 변화량
# 4번째 B는 2번째 직선의 기울기에서 3번째 직선의 기울기까지의 변화량
# 5번째 B는 3번째 직선의 기울기에서 4번째 직선의 기울기까지의 변화량
# 6번째 ...
# 7번째 ...
## 적합한 회귀직선을 그래프에 표기
plot(x, y, col="gray")
lines(x, f, lwd=2)
lines(x, fit_B1$fitted.values, col="blue")
legend("bottomright", legend = c("f_real", "f_linear_B")
       , lty = c(1,1)
       , lwd = c(2,2)
       , col = c("black","blue"))



### R실습 7 : 또 다른 매듭점을 설정하여 cubic spline(3차) 조각다항식 적합
## 기저함수 생성
# 매듭점은 -1.5, 0, 1.5 총 3개로 설정한다.
# 총 기저 함수는 7개가 필요하다( 3 + 1 + 3)
B2 = cbind(1, x, x^2, x^3, plus(x+1.5)^3, plus(x)^3, plus(x-1.5)^3) # P차 이므로 plus에 ^p을 꼭 해야한다.
matplot(x, B2, type="l", lwd=2)
legend("topleft", legend = c("1", "x", "x^2", "x^3", "{(x+1.5)_+}^3", "{(x)_+}^3", "{(x-1.5)_+}^3")
       , lty = c(1,2,3,4,5,1,2)
       , lwd = c(2,2,2,2,2,2,2)
       , col = c("black","red","green","blue","skyblue","purple","black"))
# 기본 기저 함수(3+1개) 
matplot(x, B2[, 1:4], type="l", lwd=2)
legend("topleft", legend = c("1", "x", "x^2", "x^3")
       , lty = c(1,2,3,4)
       , lwd = c(2,2,2,2)
       , col = c("black","red","green","purple"))
# 스플라인 기저 함수(3개)
matplot(x, B2[, 5:7], type="l", lwd=2)
legend("topleft", legend = c("{(x+1.5)_+}^3", "{(x)_+}^3", "{(x-1.5)_+}^3")
       , lty = c(1,2,3)
       , lwd = c(2,2,2)
       , col = c("black","red","green"))
## 새롭게 생성한 기저함수를 이용하여 적합
fit_B2 = lm(y ~ B2 - 1)
fit_B2$coefficients
## 적합한 회귀직선을 그래프에 표기
plot(x, y, col="gray")
lines(x, f, lwd=2)
lines(x, fit_B1$fitted.values, col="blue", lwd=2)
lines(x, fit_B2$fitted.values, col="red", lwd=2) # 매듭점을 잘못 설정하면 적합이 이상하게 된다.
legend("bottomright", legend = c("f_real", "f_linear_B","f_err_linear_B")
       , lty = c(1,1,1)
       , lwd = c(2,2,2)
       , col = c("black","blue","red"))

