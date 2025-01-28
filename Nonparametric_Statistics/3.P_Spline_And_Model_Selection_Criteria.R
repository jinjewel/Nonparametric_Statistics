
# 하나의 차수, 한 종류의 매듭점의 개수를 이용한 스플라인을 이용한 함수 평활
rm(list = ls())

# 데이터 선언
# install.packages("loon.data")
library(loon.data)
data(bone)
x = bone$age
y = bone$rspnbmd
n = length(x)
plot(x, y, main = "Bone Data ") # 데이터 그래프


# 단순 선형회귀분석 적합
fit = lm(y~x)
lines(x, fit$fitted.values, col = "red")


# 성별로 데이터 구분
x_male = x[bone$sex == "male"]
y_male = y[bone$sex == "male"]
x_female = x[bone$sex == "female"]
y_female = y[bone$sex == "female"]


# 기저함수를 만드는 함수
Plus = function(x)
{
  x_plus = x
  x_plus[x < 0] = 0
  return(x_plus)
}


# 각 데이터 별 기저함수를 저장할 행열변수 생성
B = cbind(1, x) # 전체 데이터
B_m = cbind(1, x_male) # 남자 데이터
B_f = cbind(1, x_female) # 여자 데이터


# 기저 함수 생성
t = 11:24 # 예비 매듭점 개수(knot) 
K = length(t) # 예비 매듭점의 총 개수
for(k in 1:K)
{
  B = cbind(B, Plus(x - t[k])) # 전체 데이터
  B_m = cbind(B_m, Plus(x_male - t[k])) # 남자 데이터
  B_f = cbind(B_f, Plus(x_female - t[k])) # 여자 데이터
}


# 각 스플라인 기저함수에서 단순 선형회귀분석 적합
fit_spline = lm(y ~ B - 1)
fit_spline_m = lm(y_male ~ B_m - 1)
fit_spline_f = lm(y_female ~ B_f - 1)


# 데이터 산점도 그리기
plot(x, y, col = "gray", cex = 1.5, bty = "n", xlab = "age", ylab = "bone mineral density") # 전체 데이터
points(x_male, y_male, col = "skyblue") # 남성 데이터
points(x_female, y_female,col = "pink") # 여성 데이터


# knot 위치 표시
abline(v = t, col = "gray", lty = 2)
# 전체 데이터에 대한 단순선형회귀
lines(x, fit$fitted.values, col = "black")
# 남자에 대한 1차 스플라인 기저함수 적함
lines(sort(x_male), fit_spline_m$fitted.values[order(x_male)], col = "blue")
# 여자에 대한 1차 스플라인 기저함수 적함
lines(sort(x_female), fit_spline_f$fitted.values[order(x_female)], col = "red")
# 성별구분없이 적합된 1차 스플라인 기저함수 적함
lines(sort(x), fit_spline$fitted.values[order(x)], col = "purple")
legend("topright", legend = c("Real_Regresstion","Male_Spline_Regresstion","Female_Spline_Regresstion","total_Spline_Regresstion")
       , lty = c(1,1,1,1), lwd=c(2,2,2,2), col=c("black","blue","red","purple"))






# 하나의 차수, 여러 종류의 매듭점의 개수를 이용한 스플라인을 이용한 함수 평활
rm(list = ls())

# 데이터 선언
# install.packages("loon.data")
library(loon.data)
data(bone)
x = bone$age
y = bone$rspnbmd
n = length(x)
plot(x, y, main = "Bone Data ") # 데이터 그래프


# 단순 선형회귀분석 적합
fit = lm(y~x)
lines(x, fit$fitted.values, col = "red")


# 성별로 데이터 구분
x_male = x[bone$sex == "male"]
y_male = y[bone$sex == "male"]
x_female = x[bone$sex == "female"]
y_female = y[bone$sex == "female"]


# 각 차수별 스플라인 기저함수 회귀적합 생성
par(mfrow = c(2, 2))
degree = 3 # 차수 설정(0,1,2,3차 설정)
nk_candidate = c(3, 5, 10, 30) # 매듭점의 예비 총 개수 설정  
for(k in 1:length(nk_candidate)) # 매듭점의 예비 총 개수 4번 반복
{
  num_knots = nk_candidate[k] # 매듭점의 예비 총 개수 설정
  t = seq(9, 26, length = num_knots) # 예비 매듭점의 위치 생성
    
  # 기저함수 생성 함수
  Plus = function(x, degree = 1)
  {
    x_plus = rep(0, length(x))
    x_plus[x >= 0] = x[x >= 0]^degree
    return(x_plus)
  }
      
  K = length(t) # 예비 매듭점의 총 개수 설정
  B = NULL # 총 데이터에 대한 기저함수를 저장하기 위한 행열 변수 생성
  B_m = NULL # 남자 데이터에 대한 기저함수를 저장하기 위한 행열 변수 생성
  B_f = NULL # 여자 데이터에 대한 기저함수를 저장하기 위한 행열 변수 생성
  
  # 예비 매듭점의 개수 만큼 반복하면서 각 데이터 별 기저함수 생성
  for(k in 1:K)
  {
    B = cbind(B, Plus(x - t[k], degree))
    B_m = cbind(B_m, Plus(x_male - t[k], degree))
    B_f = cbind(B_f, Plus(x_female - t[k], degree))
  }
  
  # 0차일 때 기저함수 생성
  if(degree == 0)
  {
    B = cbind(1, B)
    B_m = cbind(1, B_m)
    B_f = cbind(1, B_f)
  }
  
  # 1차 이상일 때 기저함수 생성
  if(degree > 0)
  {
    B = cbind(1, poly(x, degree), B)
    B_m = cbind(1, poly(x_male, degree), B_m)
    B_f = cbind(1, poly(x_female, degree), B_f)
  }
  
  # 남자와 여자 데이터를 사용하여 적합한 단순 회귀직선 
  fit_m = lm(y_male ~ x_male)
  fit_f = lm(y_female ~ x_female)
  
  # 총함, 남자, 여자 데이터를 사용하여 스플라인 기저함수 회귀 적합한다.
  fit_spline = lm(y ~ B - 1)
  fit_spline_m = lm(y_male ~ B_m - 1)
  fit_spline_f = lm(y_female ~ B_f - 1)
  
  # 데이터 표시
  plot(x, y, col = "gray", cex = 1.5, bty = "n", xlab = "age", ylab = "bone mineral density")
  points(x_male, y_male, col = "skyblue") # 남자 데이터 표시
  points(x_female, y_female,col = "pink") # 여자 데이터 표시
  
  # knot 위치 표시
  abline(v = t, col = "gray", lty = 2)
  
  # 단순 데이터에 대한 단순선형회귀 적합
  #lines(x, fit$fitted.values, col = "black") # 전체 데이터에 대한 선형회귀
  #lines(x_male, fit_m$fitted.values, col = "blue") # 남자 데이터에 대한 선형회귀
  #lines(x_female, fit_f$fitted.values, col = "red") # 여자 데이터에 대한 선형회귀
  
  # 스플라인 기저함수에 대한 단순선형회귀 적합
  lines(sort(x_male), fit_spline_m$fitted.values[order(x_male)], col = "blue") # 남자 데이터에 대한 1차 스플라인
  lines(sort(x_female), fit_spline_f$fitted.values[order(x_female)], col = "red") # 남자 데이터에 대한 1차 스플라인
  lines(sort(x), fit_spline$fitted.values[order(x)], col = "purple") # 전체 데이터에 대한 1차 스플라인
}






# 여러 차수, 여러 종류의 매듭점의 개수를 이용한 스플라인을 이용한 함수 평활
par(mfrow = c(4, 4))
for(degree in 0:3) # 차수를 설정
{
  degree = 3 # 차수 설정(0,1,2,3차 설정)
  nk_candidate = c(3, 5, 10, 30) # 매듭점의 예비 총 개수 설정  
  for(k in 1:length(nk_candidate)) # 매듭점의 예비 총 개수 4번 반복
  {
    num_knots = nk_candidate[k] # 매듭점의 예비 총 개수 설정
    t = seq(9, 26, length = num_knots) # 예비 매듭점의 위치 생성
    
    # 기저함수 생성 함수
    Plus = function(x, degree = 1)
    {
      x_plus = rep(0, length(x))
      x_plus[x >= 0] = x[x >= 0]^degree
      return(x_plus)
    }
    
    K = length(t) # 예비 매듭점의 총 개수 설정
    B = NULL # 총 데이터에 대한 기저함수를 저장하기 위한 행열 변수 생성
    B_m = NULL # 남자 데이터에 대한 기저함수를 저장하기 위한 행열 변수 생성
    B_f = NULL # 여자 데이터에 대한 기저함수를 저장하기 위한 행열 변수 생성
    
    # 예비 매듭점의 개수 만큼 반복하면서 각 데이터 별 기저함수 생성
    for(k in 1:K)
    {
      B = cbind(B, Plus(x - t[k], degree))
      B_m = cbind(B_m, Plus(x_male - t[k], degree))
      B_f = cbind(B_f, Plus(x_female - t[k], degree))
    }
    
    # 0차일 때 기저함수 생성
    if(degree == 0)
    {
      B = cbind(1, B)
      B_m = cbind(1, B_m)
      B_f = cbind(1, B_f)
    }
    
    # 1차 이상일 때 기저함수 생성
    if(degree > 0)
    {
      B = cbind(1, poly(x, degree), B)
      B_m = cbind(1, poly(x_male, degree), B_m)
      B_f = cbind(1, poly(x_female, degree), B_f)
    }
    
    # 남자와 여자 데이터를 사용하여 적합한 단순 회귀직선 
    fit_m = lm(y_male ~ x_male)
    fit_f = lm(y_female ~ x_female)
    
    # 총함, 남자, 여자 데이터를 사용하여 스플라인 기저함수 회귀 적합한다.
    fit_spline = lm(y ~ B - 1)
    fit_spline_m = lm(y_male ~ B_m - 1)
    fit_spline_f = lm(y_female ~ B_f - 1)
    
    # 데이터 표시
    plot(x, y, col = "gray", cex = 1.5, bty = "n", xlab = "age", ylab = "bone mineral density")
    points(x_male, y_male, col = "skyblue") # 남자 데이터 표시
    points(x_female, y_female,col = "pink") # 여자 데이터 표시
    
    # knot 위치 표시
    abline(v = t, col = "gray", lty = 2)
    
    # 단순 데이터에 대한 단순선형회귀 적합
    #lines(x, fit$fitted.values, col = "black") # 전체 데이터에 대한 선형회귀
    #lines(x_male, fit_m$fitted.values, col = "blue") # 남자 데이터에 대한 선형회귀
    #lines(x_female, fit_f$fitted.values, col = "red") # 여자 데이터에 대한 선형회귀
    
    # 스플라인 기저함수에 대한 단순선형회귀 적합
    lines(sort(x_male), fit_spline_m$fitted.values[order(x_male)], col = "blue") # 남자 데이터에 대한 1차 스플라인
    lines(sort(x_female), fit_spline_f$fitted.values[order(x_female)], col = "red") # 남자 데이터에 대한 1차 스플라인
    lines(sort(x), fit_spline$fitted.values[order(x)], col = "purple") # 전체 데이터에 대한 1차 스플라인
  }
}






# 모든 모델에 대한 결정계수, 수정된 결정계수, AIC, BIC를 계산하여 가장 결과가 좋은 모델을 선택 
rm(list = ls())

# 기저함수를 만들기 위한 함수
Plus = function(x, degree = 1)
{
  x_plus = rep(0, length(x))
  x_plus[x >= 0] = x[x >= 0]^degree
  return(x_plus)
}


# 데이터 선언
# install.packages("loon.data")
library(loon.data)
data(bone)
x = bone$age
y = bone$rspnbmd
plot(x, y)
n = length(x)


# 전체 데이터에 대한 기본 단순선형회귀분석 적합
fit = lm(y~x)
lines(x, fit$fitted.values, col = "red")


# 성별로 데이터 구분
x_male = x[bone$sex == "male"]
y_male = y[bone$sex == "male"]
x_female = x[bone$sex == "female"]
y_female = y[bone$sex == "female"]


degree = 1 # 차수 선언
nk_candidate = 3:30 # 예비 매듭점의 개수 선언
R_square_list = NULL # 각 모델의 결정계수를 저장하기 위해 리스트 변수 선언
Adj_R_square_list = NULL # 각 모델의 수정된 결정계수를 저장하기 위해 리스트 변수 선언
AIC_list = NULL # 각 모델의 AIC를 저장하기 위해 리스트 변수 선언
BIC_list = NULL # 각 모델의 BIC를 저장하기 위해 리스트 변수 선언


# 기본함수와 스플라인기저함수의 회귀 적합
par(mfrow = c(4, 7))
for(l in 1:length(nk_candidate)) # 예비 매듭점의 총 개수만큼 반복
{
  
  num_knots = nk_candidate[l] # 예비 매듭점의 개수 저장
  t = seq(9, 26, length = num_knots) # 예비 매듭점의 위치 저장
  
  # 각 데이터에 대한 기저함수를 저장할 행열변수 생성
  B = NULL # 전체 데이터
  B_m = NULL # 남자 데이터
  B_f = NULL # 여자 데이터
  
  # 스플라인 기저함수 생성 및 추가
  for(k in 1:num_knots)
  {
    B = cbind(B, Plus(x - t[k], degree)) # 전체 데이터
    B_m = cbind(B_m, Plus(x_male - t[k], degree)) # 남자 데이터
    B_f = cbind(B_f, Plus(x_female - t[k], degree)) # 여자 데이터
  }
  # 차수가 0일 때 기본기저함수 생성 및 추가
  if(degree == 0)
  {
    B = cbind(1, B)
    B_m = cbind(1, B_m)
    B_f = cbind(1, B_f)
  }
  # 차수가 0초과일 때 기본기저함수 생성 및 추가
  if(degree > 0)
  {
    B = cbind(1, poly(x, degree), B)
    B_m = cbind(1, poly(x_male, degree), B_m)
    B_f = cbind(1, poly(x_female, degree), B_f)
  }
  
  # 회귀함수 적합
  fit_m = lm(y_male ~ x_male) # 남자 데이터에 대한 단순선형회귀 적합
  fit_f = lm(y_female ~ x_female) # 여자 데이터에 대한 단순선형회귀 적합
  fit_spline = lm(y ~ B - 1) # 전체 데이터에 대한 스플라인 단순선형회귀 적합
  fit_spline_m = lm(y_male ~ B_m - 1) # 남자 데이터에 대한 스플라인 단순선형회귀 적합
  fit_spline_f = lm(y_female ~ B_f - 1) # 여자 데이터에 대한 스플라인 단순선형회귀 적합
  
  # 전체 데이터에 대한 스플라인 단순선형회귀적합의 R-square & Adjusted R-square 계산
  sum_fit_spline = summary(fit_spline)
  R_square_list[l] = sum_fit_spline$r.squared
  Adj_R_square_list[l] = sum_fit_spline$adj.r.squared
  
  # 전체 데이터에 대한 스플라인 단순선형회귀적합의 AIC, BIC 계산
  AIC_list[l] = n * log(mean(fit_spline$residuals^2)) + 2 * num_knots
  BIC_list[l] = n * log(mean(fit_spline$residuals^2)) + log(n) * num_knots
  
  # 데이터 산점도 그래프 그리기
  plot(x, y, col = "gray", cex = 1.5, bty = "n", xlab = "age", ylab = "bone mineral density")
  points(x_male, y_male, col = "skyblue")
  points(x_female, y_female,col = "pink")
  title(num_knots)
  
  # 단순선형회귀 적합 그리기
  #lines(x, fit$fitted.values, col = "black") # 전체 데이터
  #lines(x_male, fit_m$fitted.values, col = "blue") # 남자 데이터에 대한 기본함수
  #lines(x_female, fit_f$fitted.values, col = "red") # 여자 데이터에 대한 기본함수
  lines(sort(x), fit_spline$fitted.values[order(x)], col = "purple") # 전체 데이터에 대한 적합된 1차 스플라인
  lines(sort(x_male), fit_spline_m$fitted.values[order(x_male)], col = "blue") # 남자 데이터에 대한 1차 스플라인
  lines(sort(x_female), fit_spline_f$fitted.values[order(x_female)], col = "red") # 여자 데이터에 대한 1차 스플라인
  abline(v = t, col = "gray", lty = 2) # 매듭점(knot) 위치 표시
}






par(mfrow = c(2, 2)) 


# 가장 좋은 결정 계수를 이용하여 적합
R_square_index = which.max(R_square_list) # 결정 계수들 중에서 가장 큰(결과가 가장 좋음) 값의 인덱스 저장
Adj_R_square_index = which.max(Adj_R_square_list) # 수정된 결정 계수들 중에서 가장 큰(결과가 가장 좋음) 값의 인덱스 저장


# 결정계수들의 값을 표시하고 가장 좋은 값을 표시
plot(nk_candidate, R_square_list, type = "o", bty = "n", xlab = "number of knots", ylab = "R-square", main="R-square plot")
points(nk_candidate[R_square_index], max(R_square_list), col = "red", pch = 16) # 가장 좋은 결정 계수의 포인트 표시
abline(v = nk_candidate[R_square_index], col = "gray", lty = 3) # 가장 좋은 결정 계수의 수직선 표시


# 가장 좋은 결정 계수를 이용하여 데이터 셋에 회귀 적합
num_knots_R = nk_candidate[R_square_index] # 가장 좋은 결정 계수를 보이는 매듭점의 개수 저장
t = seq(9, 26, length = num_knots_R) # 매듭점의 지점 설정


# 기저함수를 저장할 변수 생성
B = NULL 
B_m = NULL
B_f = NULL


# 스플라인 기저함수 생성
for(k in 1:num_knots_R)
{
  B = cbind(B, Plus(x - t[k], degree))
  B_m = cbind(B_m, Plus(x_male - t[k], degree))
  B_f = cbind(B_f, Plus(x_female - t[k], degree))
}
# 0차일 때 기본기저함수 생성
if(degree == 0)
{
  B = cbind(1, B)
  B_m = cbind(1, B_m)
  B_f = cbind(1, B_f)
}
# 1차이상 일 때 기본기저함수 생성
if(degree > 0)
{
  B = cbind(1, poly(x, degree), B)
  B_m = cbind(1, poly(x_male, degree), B_m)
  B_f = cbind(1, poly(x_female, degree), B_f)
}


# 데이터 그래프 
plot(x, y, col = "gray", cex = 1.5, bty = "n", xlab = "age", ylab = "bone mineral density", main="Best model by R-square")
points(x_male, y_male, col = "skyblue") # 남자 데이터
points(x_female, y_female,col = "pink") # 여자 데이터


# 단순선형회귀 적합
abline(v = t, col = "gray", lty = 2) # 매듭점(knot) 위치 표시
fit_spline_m = lm(y_male ~ B_m - 1)
lines(sort(x_male), fit_spline_m$fitted.values[order(x_male)], col = "blue") # 남자 데이터에 대한 1차 스플라인 적합
fit_spline_f = lm(y_female ~ B_f - 1)
lines(sort(x_female), fit_spline_f$fitted.values[order(x_female)], col = "red") # 여자 데이터에 대한 1차 스플라인 적합 
fit_spline = lm(y ~ B - 1)
lines(sort(x), fit_spline$fitted.values[order(x)], col = "purple") # 전체 데이터에 대한 적합된 1차 스플라인 적합






# 수정된 결정계수들의 값을 표시하고 가장 좋은 값을 표시
plot(nk_candidate, Adj_R_square_list, type = "o", bty = "n", xlab = "number of knots", ylab = "Adjusted R-square", main="Adjusted R-square plot")
points(nk_candidate[Adj_R_square_index], max(Adj_R_square_list), col = "red", pch = 16) # 가장 좋은 수정된 결정 계수의 포인트 표시
abline(v = nk_candidate[Adj_R_square_index], col = "gray", lty = 3) # 가장 좋은 수정된 결정 계수의 수직선 표시


# 가장 좋은 수정된 결정 계수를 이용하여 데이터 셋에 회귀 적합
num_knots_Adj_R = nk_candidate[Adj_R_square_index] # 가장 좋은 수정된 결정 계수의 인덱스 저장
t = seq(9, 26, length = num_knots_Adj_R) # 매듭점의 위치 설정


# 기저함수를 저장할 변수 생성
B = NULL
B_m = NULL
B_f = NULL
# 스플라인 기저함수 생성
for(k in 1:num_knots_Adj_R)
{
  B = cbind(B, Plus(x - t[k], degree))
  B_m = cbind(B_m, Plus(x_male - t[k], degree))
  B_f = cbind(B_f, Plus(x_female - t[k], degree))
}
# 0차일 때 기본 기저함수 생성 
if(degree == 0)
{
  B = cbind(1, B)
  B_m = cbind(1, B_m)
  B_f = cbind(1, B_f)
}
# 0차 이상일 때 기본기저함수 생성
if(degree > 0)
{
  B = cbind(1, poly(x, degree), B)
  B_m = cbind(1, poly(x_male, degree), B_m)
  B_f = cbind(1, poly(x_female, degree), B_f)
}


# 데이터 그래프
plot(x, y, col = "gray", cex = 1.5, bty = "n", xlab = "age", ylab = "bone mineral density", main="Best model by Adjusted R-square")
points(x_male, y_male, col = "skyblue")
points(x_female, y_female,col = "pink")


# 단순선형회귀
abline(v = t, col = "gray", lty = 2) # 매듭점(knot)의 위치 표시
fit_spline_m = lm(y_male ~ B_m - 1)
lines(sort(x_male), fit_spline_m$fitted.values[order(x_male)], col = "blue") # 남자 데이터에 대한 1차 스플라인 회귀 적합
fit_spline_f = lm(y_female ~ B_f - 1)
lines(sort(x_female), fit_spline_f$fitted.values[order(x_female)], col = "red") # 여자 데이터에 대한 1차 스플라인 회귀 적합
fit_spline = lm(y ~ B - 1)
lines(sort(x), fit_spline$fitted.values[order(x)], col = "purple") # 전체 데이터에 대한 1차 스플라인 회귀 적합






par(mfrow = c(2, 2))

# AIC들의 값을 표시하고 가장 좋은 값을 표시
plot(nk_candidate, AIC_list, type = "o", bty = "n", xlab = "number of knots", ylab = "AIC", main="AIC plot")
points(nk_candidate[AIC_index], min(AIC_list), col = "red", pch = 16)
abline(v = nk_candidate[AIC_index], col = "gray", lty = 3)


#  AIC를 이용하여 데이터 셋에 회귀 적합
AIC_index = which.min(AIC_list) # AIC가 가장 작을때(값이 가장 좋을 때)의 인덱스 저장
num_knots_AIC = nk_candidate[AIC_index] # AIC가 가장 작을때 매듭점 개수 저장
t = seq(9, 26, length = num_knots_AIC) # 매듭점의 위치 설정


# 기저함수를 저장할 변수 생성
B = NULL
B_m = NULL
B_f = NULL
# 스플라인 기저함수 새성
for(k in 1:num_knots_AIC)
{
  B = cbind(B, Plus(x - t[k], degree))
  B_m = cbind(B_m, Plus(x_male - t[k], degree))
  B_f = cbind(B_f, Plus(x_female - t[k], degree))
}
# 0차일 때 기본 기저함수 생성
if(degree == 0)
{
  B = cbind(1, B)
  B_m = cbind(1, B_m)
  B_f = cbind(1, B_f)
}
# 0차 이상일 때 기본 기저함수 생성
if(degree > 0)
{
  B = cbind(1, poly(x, degree), B)
  B_m = cbind(1, poly(x_male, degree), B_m)
  B_f = cbind(1, poly(x_female, degree), B_f)
}


# 데이터 그래프 
plot(x, y, col = "gray", cex = 1.5, bty = "n", xlab = "age", ylab = "bone mineral density", main="Best model by AIC")
points(x_male, y_male, col = "skyblue")
points(x_female, y_female,col = "pink")


# 단순선형회귀
abline(v = t, col = "gray", lty = 2) # 매듭점(knot)의 위치 표시
fit_spline_m = lm(y_male ~ B_m - 1)
lines(sort(x_male), fit_spline_m$fitted.values[order(x_male)], col = "blue") # 남자 데이터에 대한 1차 스플라인
fit_spline_f = lm(y_female ~ B_f - 1)
lines(sort(x_female), fit_spline_f$fitted.values[order(x_female)], col = "red") # 여자 데이터에 대한 1차 스플라인
fit_spline = lm(y ~ B - 1)
lines(sort(x), fit_spline$fitted.values[order(x)], col = "purple") # 전체 데이터에 대한 1차 스플라인 회귀 적합






# BIC들의 값을 표시하고 가장 좋은 값을 표시
plot(nk_candidate, BIC_list, type = "o", bty = "n", xlab = "number of knots", ylab = "BIC", main="BIC plot")
BIC_index = which.min(BIC_list) # BIC가 가장 작을때(값이 가장 좋을 때)의 인덱스 저장
points(nk_candidate[BIC_index], min(BIC_list), col = "red", pch = 16)
abline(v = nk_candidate[BIC_index], col = "gray", lty = 3)


#  BIC를 이용하여 데이터 셋에 회귀 적합
num_knots_BIC = nk_candidate[BIC_index]
t = seq(9, 26, length = num_knots_BIC)


# 기저함수를 저장할 변수 생성
B = NULL
B_m = NULL
B_f = NULL
# 스플라인 기저함수를 생성
for(k in 1:num_knots_BIC)
{
  B = cbind(B, Plus(x - t[k], degree))
  B_m = cbind(B_m, Plus(x_male - t[k], degree))
  B_f = cbind(B_f, Plus(x_female - t[k], degree))
}
# 0차일 때 기본 기저함수를 생성
if(degree == 0)
{
  B = cbind(1, B)
  B_m = cbind(1, B_m)
  B_f = cbind(1, B_f)
}
# 0차 이상일 때 기본 기저함수를 생성
if(degree > 0)
{
  B = cbind(1, poly(x, degree), B)
  B_m = cbind(1, poly(x_male, degree), B_m)
  B_f = cbind(1, poly(x_female, degree), B_f)
}


# 데이터 그래프
plot(x, y, col = "gray", cex = 1.5, bty = "n", xlab = "age", ylab = "bone mineral density", main="Best model by BIC")
points(x_male, y_male, col = "skyblue")
points(x_female, y_female,col = "pink")


# 단순선형회귀
abline(v = t, col = "gray", lty = 2) # 매듭점(knot)의 위치 표시 
fit_spline_m = lm(y_male ~ B_m - 1)
lines(sort(x_male), fit_spline_m$fitted.values[order(x_male)], col = "blue") # 남자 데이터에 대한 1차 스플라인
fit_spline_f = lm(y_female ~ B_f - 1)
lines(sort(x_female), fit_spline_f$fitted.values[order(x_female)], col = "red") # 여자 데이터에 대한 1차 스플라인
fit_spline = lm(y ~ B - 1)
lines(sort(x), fit_spline$fitted.values[order(x)], col = "purple") # 전체 데이터에 대한 1차 스플라인 회귀 적합






# 데이터에 대한 스플라인 기저함수 생성 및 단순회귀직선을 적합하고 수정된 결정계수 계산
rm(list = ls())


# 데이터 선언 
# install.packages("loon.data")
library(loon.data)
data(bone)
x = bone$age
y = bone$rspnbmd
n = length(x)


# 성별로 데이터 구분
x_male = x[bone$sex == "male"]
y_male = y[bone$sex == "male"]
x_female = x[bone$sex == "female"]
y_female = y[bone$sex == "female"]


# 기저함수를 생성하기 위한 함수 생성
Plus = function(x)
{
  x_plus = x
  x_plus[x < 0] = 0
  return(x_plus)
}


# 기저함수를 저장하기 위한 변수 생성
t = 11:24 # 매듭점의 위치 설정
K = length(t) # 매듭점의 개수 
B = cbind(1, x)
B_m = cbind(1, x_male)
B_f = cbind(1, x_female)


# 기본기저함수와 스플라인 기저함수 생성
for(k in 1:K)
{
  B = cbind(B, Plus(x - t[k]))
  B_m = cbind(B_m, Plus(x_male - t[k]))
  B_f = cbind(B_f, Plus(x_female - t[k]))
}


# 데이터 산점도
plot(x, y, col = "gray", cex = 1.5, bty = "n", xlab = "age", ylab = "bone mineral density")
points(x_male, y_male, col = "skyblue")
points(x_female, y_female,col = "pink")


# 선형회귀분석 적합
abline(v = t, col = "gray", lty = 2) # 매듭점(knot)의 위치 표시
fit = lm(y~x) # 전체 데이터에 대한 단순 선형회귀 적합
lines(x, fit$fitted.values, col = "black")
fit_spline_m = lm(y_male ~ B_m - 1) # 남자 데이터에 대한 1차 스플라인 회귀 적합
lines(sort(x_male), fit_spline_m$fitted.values[order(x_male)], col = "blue")
fit_spline_f = lm(y_female ~ B_f - 1) # 여자 데이터에 대한 1차 스플라인 회귀 적합
lines(sort(x_female), fit_spline_f$fitted.values[order(x_female)], col = "red")
fit_spline = lm(y ~ B - 1) # 전체 데이터에 대한 1차 스플라인 회귀 적합
lines(sort(x), fit_spline$fitted.values[order(x)], col = "purple")


# 수정된 결정계수 계산
sum_fit_spline = summary(fit_spline)
sum_fit_spline$r.squared
sum_fit_spline$adj.r.squared

