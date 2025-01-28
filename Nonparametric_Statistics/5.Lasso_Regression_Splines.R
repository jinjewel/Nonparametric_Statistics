rm(list = ls())
# Lasso 스플라인 회귀를 적합하기 위한 CDA 함수 생성 및 실제 적합 예제 생성


# 기저함수를 만들기 위한 함수 선언
Plus = function(x, degree)
{
  # (x)_+^degree
  # 차수가 0인경우, 기본 기저 함수가 없음
  if(degree == 0) 
  {
    x_plus = rep(0, length(x))
    x_plus[x > 0] = 1
    return(x_plus)
  }
  # 차수가 1 이상인 경우
  x_plus = x
  x_plus[x < 0] = 0
  return(x_plus^degree)
}

# Soft Thresholding function : Lasso 회귀를 적합하기 위한 함수 선언
ST = function(z, lambda)
{
  if(z > lambda) # z값이 기준(람다)보다 크면
    return(z - lambda) # 람다를 빼서 0에 한정
  if(z < -lambda) # z값이 기준(람다)보다 작으면
    return(z + lambda) # 람다를 더해서 0에 한정
  return(0) # z값이 기준과 비슷한 범위에 있으면 0으로 생각
}

# Lasso Regression Splines using CDA
# Lasso 회귀를 계산하기 위한 CDA 함수
LRS = function(x, y, degree = 1, lambda = 0, number_knots = 10, epsilon = 1e-7, maxiter = 10000)
{
  # 자료의 개수
  n = length(x)
  
  # knots 계산 : (t_1, t_2, ..., t_K), K = number_knots
  # n 부터 max까지 양 끝 점을 포함하여 number_knots + 2개 표시
  knots = seq(min(x), max(x), length = number_knots + 2) 
  # number_knots개의 매듭점 개수를 만들기 위해 양 끝점을 제거
  knots = knots[2:(number_knots + 1)] # 방법 1
  # knots = knots[- c(1, number_knots + 2)] # 방법 2
  
  # 기저함수 생성
  # polynomial basis : 1, x, x^2, ..., x^p, p = degree
  # 차수가 1 이상인 경우
  if(degree > 0)
  {
    # 절편항을 제외한 기본기저 함수 값을 저장하기 위해 n × degree 행렬 선언 
    polynomial_basis = matrix(0, n, degree)
    # 차수 만큼을 반복하며 해당 차수에 해당하는 기본기저함수 값을 계산
    for(j in 1:degree)
      polynomial_basis[, j] = x^j
    # 절편항까지 포함
    polynomial_basis = cbind(1, polynomial_basis)
  }
  # 차수가 0인 경우
  if(degree == 0)
    # 기본기저 함수는 절편항만 존재하는 n × 1 행렬이다.
    polynomial_basis = matrix(1, n, 1)
  
  # 스플라인 기저함수 생성, spline basis : (x - t_k)_+^p
  spline_basis = matrix(0, n, number_knots)
  # 매듭점의 수만큼 반복
  # Plus함수를 이용해 각 지점에서의 스플라인 기저함수 계산
  for(k in 1:number_knots)
    spline_basis[, k] = Plus(x - knots[k], degree)
  
  # 통합 기저행렬 : n x ((p + 1) + K)
  B = cbind(polynomial_basis, spline_basis)
  
  # J = 회귀계수의 총 개수 : p + 1 + K
  J = ncol(B)
  
  # 회귀계수의 초기값
  beta = rep(0, J)
  
  # 초기 잔차 (잔차 = y - yhat)
  # yhat은 현재 초기값이 0으로 설정했으므로 yhat = 0
  residuals = y
  
  # 부분잔차와 Soft Thresholding function함수를 사용하기 위해
  # a = sum(B[,j]^2) / n 을 계산
  BB = colSums(B^2) / n
  
  # 초기 목적함수 값
  object_old = 0.5 * mean(residuals^2)
  
  # CDA start
  for(iter in 1:maxiter)
  {
    # 좌표별 업데이트
    for(j in 1:J)
    {
      # 부분잔차 계산
      partial_residuals = residuals + beta[j] * B[, j] 
      
      # a 계산
      # a = sum(B[, j]^2) / n # 원래는 for문 안에서 계산했으나
      a = BB[j] # 성능 개선을 위해 밖에서 계산하고 가져다 쓰기로 바꿈
      
      # b 계산
      b = sum(partial_residuals * B[, j]) / (a * n)
      
      # coefficient update
      if(j < (degree + 2)) # 기본기저 함수, 즉 gamma_j가 존재하는 않는 경우
        beta[j] = b
      else # 스플라인 기저함수, 즉 gamma_j가 존재하는 경우
        beta[j] = ST(b, lambda / a)
      
      # 잔차를 다시계산
      residuals = partial_residuals - beta[j] * B[, j]
    }
    
    # stop rule
    rss = 0.5 * mean(residuals^2) # 평범한 최소제곱합 계산
    
    # 패널티 항을 계산하기 위한 gamma의 절대값들의 합
    penalty = sum(abs(beta[-(1:(degree + 1))]))
    
    # 벌점항이 추가된 목적함수
    object = rss + lambda * penalty
    
    if(abs(object - object_old) < epsilon) # 편차가 설정한 기준(epsilon)보다 작으면
      break # 중지
    object_old = object # 설정한 기준보다 크면 오차를 재설정하고 다시 반복
  }
  
  # return 값 출력
  # fhat = B %*% beta
  fhat = y - residuals
  return(list(fhat = fhat, beta = beta))
}


# 다른 파일에 만들어서 실행하는 경우 실행
# rm(list = ls())
# # 코드 불러오기
# source("LassoRegressionSplines.R")

# data 선언
n = 100
x = seq(0, 1, length = n)
f = sin(2 * pi * cos(4 * x))
y = f + rnorm(n, sd = 0.2)

# 그래프 및 함수 출력
plot(x, y, col = "gray", bty = "n")
lines(x, f)

# Lasso 회귀 적합
fit = LRS(x, y, degree = 1, lambda = 0.000, number_knots = 20, epsilon = 1e-7, maxiter = 10000)
fit$beta

# Lasso 회귀 추세선 적합
plot(x, y, col = "gray", bty = "n")
lines(x, f)
lines(x, fit$fhat, col = "blue")
  
  
  
  
  


