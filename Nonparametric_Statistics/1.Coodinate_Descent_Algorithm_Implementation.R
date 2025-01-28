### 2주차 

### R실습 1 : CDA 알고리즘 구현
rm(list = ls())

## 1차 방정식 데이터 구현 
n = 200 # 데이터 갯수
x = seq(0, 1, length = n) # 0.005단위로 추출
f = 2 * x - 3 # 일차 방정식 구현
e = rnorm(n, sd = 0.3) # 오차 구현
y = f + e # 데이터 구현

plot(x, y, col = "gray") # 좌표(그림)에 데이터 출력
lines(x, f) # 실제 적합 라인 출력

## lm()함수를 이용하여 회귀직선 예측하기
fit_lm = lm(y ~ x)
fit_lm$coefficients # 적합된 y절편과 기울기 확인
lines(x, fit_lm$fitted.values, col ="red") # 적합된 회귀직선 출력

## CDA 알고리즘 함수 구현
CDA = function(x, y, epsilon = 10^(-5), max = 10000)
{
  
  B = cbind(1, x) # 기저 함수 생성
  class(B)
  
  beta = c(0, 0) # 초기 bete값 생성
  class(beta)
  
  dim(beta[1] * B[, 1])
  dim(B[, 1] * beta[1])
  dim(beta[1] %*% B[, 1])
  # B[, 1] %*% beta[1] 는 오류
  # B[, 1] %*% beta[1]에서 다음과 같은 에러가 발생했습니다:적합한 인자들이 아닙니다
  
  dim(beta * B)
  dim(B * beta)
  # beta %*% B 는 오류
  # beta %*% B에서 다음과 같은 에러가 발생했습니다:적합한 인자들이 아닙니다
  dim(B %*% beta)
  
  residual = y # 가능
  class(residual)
  dim(residual)
  
  resid = y - B * beta # 불가능
  class(resid)
  dim(resid)
  
  resid = y - B %*% beta # 가능
  class(resid)
  dim(resid)
  
  R_beta_old = Inf # 점점 작아질 예정이므로 초기값은 무한대로 설정
  
  for(r in 1:max) # stopping rule에 해당될 때까지 반복하기 위해 max로 설정
  {
    for(j in 1:2) # 1차 방정식의 기저가 2개 이므로[ B1() = 1, B2() = x ] j는 2까지 반복
    {
      # 아래의 beta[j] * B[, j]는 이전 반복에 게산된 해당좌표계수의 적합
      partial_residual = residual + beta[j] * B[, j] # 부분잔차 계산, y_ij(r)
      # partial_residual = residual + B[, j] * beta[j] # 부분잔차 계산, y_ij(r)
      # partial_residual = residual + beta[j] %*% B[, j] # 부분잔차 계산, y_ij(r)
      
      # beta[j]를 최솟값으로 다시 갱신, beta[j] = 틸다_베타_j(r)
      beta[j] = sum(partial_residual * B[, j]) / sum(B[, j]^2)
      
      # 아래의 beta[j] * B[, j]는 직전 갱신된 이전 좌표 계수의 적합
      residual = partial_residual - beta[j] * B[, j]
      # residual = partial_residual - B[, j] * beta[j]
      # residual = partial_residual - beta[j] %*% B[, j]
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

## 생성한 CDA를 이용하여 적합
fit_CDA = CDA(x, y, epsilon = 1e-10) # epsilon을 작게 하면 할수록 적합률이 올라간다.
fit_lm$coefficients
# yhat 추정
yhat_CDA = cbind(1, x) %*% fit_CDA

## 그래프에 cDA를 이용하여 적합한 직선그리기
lines(x, yhat_CDA, col = "blue", lty = 3)

