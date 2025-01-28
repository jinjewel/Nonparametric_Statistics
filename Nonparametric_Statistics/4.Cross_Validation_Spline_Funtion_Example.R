rm(list = ls())
# 스플라인 함수에 교차 타당법을 적용하여 성능의 차이를 알아보는 예제


### 스플라인 함수를 만들기 위한 기저백터 함수 생성
Plus = function(x, degree = 1)
{
  x_plus = rep(0, length(x))
  x_plus[x >= 0] = x[x >= 0]^degree # 0 이상일때 차수(degree)만큼 제곱해줌
  return(x_plus)
}


### 함수 및 변수를 생성
set.seed(1) # 시드값 생성
n = 100 # 개체 수 생성
x = seq(0, 1, length = n) # 개체 수 만큼의 0과 1사이의 x값 생성
f = sin(2 * pi * x^2)^3 


### training data 생성
e_train = rnorm(n, sd = 0.2)
y_train = f + e_train


### test data 생성
e_test = rnorm(n, sd = 0.2)
y_test = f + e_test


### 각 데이터 별 그래프 
par(mfrow = c(1, 2))
plot(x, y_train, bty = "n", col = "gray")
lines(x, f)
title("Training data")
plot(x, y_test, bty = "n", col = "gray")
lines(x, f)
title("Test data")
par(mfrow = c(1, 1))


### 매듭점
degree = 1 # 차수
num_knots = 60 # 매듭점의 수
t = seq(0, 1, length = num_knots) # 매듭점 수에 대한 위치 생성
B = NULL # 기저함수 저장


#### 매듭점의 수만큼의 반복을 진행
for(k in 1:num_knots)
{
  # 기저함수를 사용한 스플라인 함수 적합 (변수-매듭점)^(차수)
  B = cbind(B, Plus(x - t[k], degree))
}
# 차수가 0인 경우, 기본기저함수를 포함하지 않음
if(degree == 0)
{
  B = cbind(1, B)
}
# 차수가 1이상인 경우, 기본기저함수를 포함
if(degree > 0)
{
  B = cbind(1, poly(x, degree, raw = T), B)
}


### 기저함수 그래프
matplot(x, B, type = "l")


### 기저함수를 이용한 training, test data 회귀적합
fit_spline = lm(y_train ~ B - 1) # train데이터에 대한 회귀 적합
train_error = mean((y_train - fit_spline$fitted.values)^2)
test_error = mean((y_test - fit_spline$fitted.values)^2)


### training data
par(mfrow = c(1, 2))
plot(x, y_train, col = "gray", cex = 1, bty = "n") # training data 
lines(x, f, col = "black", lty = 3) # 실제 함수 적합
lines(x, fit_spline$fitted.values, col = "blue") # 회귀 적합 추세선 표현
title(paste0("Train Error = ", round(train_error, 4))) # train_error 표현


### test data
plot(x, y_test, col = "gray", cex = 1, bty = "n") # test data 
lines(x, f, col = "black", lty = 3) # 실제 함수 적합
lines(x, fit_spline$fitted.values, col = "blue") # 회귀 적합 추세선 표현
title(paste0("Test Error = ", round(test_error, 4))) # test_error 표현
par(mfrow = c(1, 1))


### 매듭점 개수의 후보군
nk_candidate = 3:80
num_nk_candidate = length(nk_candidate) # 매듭점 후보군의 개수를 저장


### 매듭점의 개수마다 training data Error를 저장할 변수 선언
train_error_list = NULL 


### 매듭점의 개수마다 test data Error를 저장할 변수 선언
test_error_list = NULL


### 후보 매듭점의 개수만큼 반복
for(l in 1:num_nk_candidate)
{
  num_knots = nk_candidate[l] # 인덱스에 해당하는 예비 매듭점의 개수를 지정
  t = seq(0, 1, length = num_knots) # 매듭점에 해당하는 x값 생성
  B = NULL 
  
  
  ### 예비 매듭점만큼의 반복을 하면서
  for(k in 1:num_knots)
  {
    # 기저함수를 사용한 스플라인 함수 적합 (변수-매듭점)^(차수)
    B = cbind(B, Plus(x - t[k], degree))
  }
  # 차수가 0인 경우, 기본기저함수를 포함하지 않음
  if(degree == 0)
  {
    B = cbind(1, B)
  }
  # 차수가 1이상인 경우, 기본기저함수를 포함
  if(degree > 0)
  {
    B = cbind(1, poly(x, degree), B)
  }
  # training data를 이용하여 회귀모델 생성
  fit_spline = lm(y_train ~ B - 1)
  
  
  ### 적합한 회귀모델에 대한 각 데이터별 Error 계산
  train_error_list[l] = train_error = mean((y_train - fit_spline$fitted.values)^2)
  test_error_list[l] = test_error = mean((y_test - fit_spline$fitted.values)^2)
  
  
  # ### training data 출력
  # plot(x, y_train, col = "gray", cex = 1, bty = "n") # training data 그래프 표시
  # #lines(x, f, col = "black", lty = 3)
  # lines(x, fit_spline$fitted.values, col = "blue") # 적합된 회귀선 표시
  # title(paste0(num_knots, "Train Error = ", round(train_error, 4))) # 적합된 매듭점 수와 에러 표시
  # 
  # 
  # ### test data 출력
  # plot(x, y_test, col = "gray", cex = 1, bty = "n") # test data 그래프 표시
  # #lines(x, f, col = "black", lty = 3)
  # lines(x, fit_spline$fitted.values, col = "blue") # 적합된 회귀선 표시
  # title(paste0(num_knots, "Test Error = ", round(test_error, 4))) # 적합된 매듭점 수와 에러 표시
  
}


### 매듭점의 수 마다 생성된 error 저장
tt_error_list = cbind(train_error_list, test_error_list)
train_index = which.min(train_error_list) # train error 중에서 가장 작은 값의 인덱스 출력
test_index = which.min(test_error_list) # test error 중에서 가장 작은 값의 인덱스 출력


### 각 매듭점의 수마다 error를 그래프에 표시 및 최적의 매듭점의 수 표시
par(mfrow = c(1, 1))
matplot(nk_candidate, tt_error_list, bty = "n", col = c("blue", "red"), type = "o", cex = 0.5, pch = 4, xlab = "number of knots", ylab = "traing and test errors")
points(nk_candidate[train_index], min(train_error_list), col = "blue", cex = 1.5, pch = 16)
points(nk_candidate[test_index], min(test_error_list), col = "red", cex = 1.5, pch = 16)
legend("topright", legend = c("traing error", "test error"), col = c("blue", "red"), pch = 4, lty = 1, bty = "n", cex = 1.5)


### best fit by train
par(mfrow = c(2, 2))
num_knots = nk_candidate[train_index] # error가 가장 작은 매듭점의 수로 선정
t = seq(0, 1, length = num_knots) # 매듭점의 위치 표시
B = NULL # 기저함수 저장
for(k in 1:num_knots) # 매듭점의 수만큼 반복
{
  # 기저함수를 사용한 스플라인 함수 적합 (변수-매듭점)^(차수)
  B = cbind(B, Plus(x - t[k], degree))
}
# 차수가 0인 경우, 기본기저함수를 포함하지 않음
if(degree == 0)
{
  B = cbind(1, B)
}
# 차수가 1이상인 경우, 기본기저함수를 포함
if(degree > 0)
{
  B = cbind(1, poly(x, degree), B)
}
# train 데이터에 대한 최적의 회귀직선 적합
fit_spline = lm(y_train ~ B - 1)
# 이때 추정된 직선과 각 데이터별 error 생성
train_error = mean((y_train - fit_spline$fitted.values)^2)
test_error = mean((y_test - fit_spline$fitted.values)^2)


### 생성된 error를 각 데이터 별 표시
# Training data
plot(x, y_train, col = "gray", cex = 1, bty = "n")
#lines(x, f, col = "black", lty = 3)
lines(x, fit_spline$fitted.values, col = "blue")
title(paste0(num_knots, "Train Error = ", round(train_error, 4)))
# Test data
plot(x, y_test, col = "gray", cex = 1, bty = "n")
#lines(x, f, col = "black", lty = 3)
lines(x, fit_spline$fitted.values, col = "blue")
title(paste0(num_knots, "Test Error = ", round(test_error, 4)))


### fit by test
num_knots = nk_candidate[test_index] # error가 가장 작은 매듭점의 수로 선정
t = seq(0, 1, length = num_knots) # 매듭점의 위치 표시
B = NULL # 기저함수 저장
# 매듭점의 수만큼 반복
for(k in 1:num_knots)
{
  # 기저함수를 사용한 스플라인 함수 적합 (변수-매듭점)^(차수)
  B = cbind(B, Plus(x - t[k], degree))
}
# 차수가 0인 경우, 기본기저함수를 포함하지 않음
if(degree == 0)
{
  B = cbind(1, B)
}
# 차수가 1이상인 경우, 기본기저함수를 포함
if(degree > 0)
{
  B = cbind(1, poly(x, degree), B)
}
# train 데이터에 대한 최적의 회귀직선 적합
fit_spline = lm(y_train ~ B - 1)
# 이때 추정된 직선과 각 데이터별 error 생성
train_error = mean((y_train - fit_spline$fitted.values)^2)
test_error = mean((y_test - fit_spline$fitted.values)^2)


### 생성된 error를 각 데이터 별 표시
# Training data
plot(x, y_train, col = "gray", cex = 1, bty = "n")
#lines(x, f, col = "black", lty = 3)
lines(x, fit_spline$fitted.values, col = "red")
title(paste0(num_knots, "Train Error = ", round(train_error, 4)))
# Test data
plot(x, y_test, col = "gray", cex = 1, bty = "n")
#lines(x, f, col = "black", lty = 3)
lines(x, fit_spline$fitted.values, col = "red")
title(paste0(num_knots, "Test Error = ", round(test_error, 4)))



rm(list = ls())

### 스플라인 함수를 만들기 위한 기저백터 함수 생성
Plus = function(x, degree = 1) 
{
  x_plus = rep(0, length(x))
  x_plus[x >= 0] = x[x >= 0]^degree # 0 이상일때 차수(degree)만큼 제곱해줌
  return(x_plus)
}


### 함수 및 변수를 생성
set.seed(25) # 시드값 생성
n = 500 # 개체 수 생성
K = 5 # 군집 수 설정
x = seq(0, 1, length = n) # 개체 수 만큼의 0과 1사이의 x값 생성
f = sin(2 * pi * x^2)^3 # 실제 함수 생성
e = rnorm(n, sd = 0.2) # 오차 생성
y = f + e # 실제 함수값에 오차를 더해서 y값 생성


### 군집당 할당할 개수 생성
fold_index = sample(1:500) # 랜덤하게 1:500을 나열
a = floor(n / K) # K 만큼의 군집을 생성하기 위해 한 군집에 들어갈 갯수 계산
fold_index_list = x_list = y_list = list() # 빈리스트 생성


### 각 군집에 랜덤하게 나뉜 데이터를 할당
for(k in 1:K) # k(군집의 수)만큼 돌면서 수행
{
  # 이중 list "fold_index_list"에 랜덤하게 생성한 변수들의 인덱스를 군집당 할당할 개수 만큼 저장
  fold_index_list[[k]] = fold_index[(a * (k - 1) + 1) : (a * k)]
  # 마지막 군집에는 나머지 랜덤 변수들의 인덱스를 모두 저장
  if(k == K)
    fold_index_list[[k]] = fold_index[(a * (k - 1) + 1) : n]
  
  x_list[[k]] = x[fold_index_list[[k]]] # 랜덤하게 지정된 인덱스에 해당하는 x값을 이중 리스트 'x_list'에 저장
  y_list[[k]] = y[fold_index_list[[k]]] # 랜덤하게 지정된 인덱스에 해당하는 y값을 이중 리스트 'y_list'에 저장
}


### 6개의 그래프를 한 화면에 보기 위해 선언
par(mfrow = c(2, 3))
# par(mfrow = c(1, 1))

### 실제 함수 그래프
plot(x, y, bty = "n", col = "gray", cex = 1)


### 각 군집에 해당되는 x와 y를 다른색으로 매칭하여 총 5개의 그래프를 겹쳐서 생성
for(k in 1:K)
{
  points(x_list[[k]], y_list[[k]], bty = "n", col = k + 1, cex = 1.0)
  rug(x_list[[k]], col = k + 1, lwd = 0.3) # 데이터의 밀도에 따른 띠 모양의 1차원 정보를 좌표축에 표시
}


### 실제 함수를 그래프에 적합
lines(x, f)
title("Full data")


### 각 군집에 해당하는 값들로 plot형성
for(k in 1:K)
{
  # paste0() : 문자열을 합쳐주는 함수
  # 원데이터의 해당하는 x와 y으로 plot 생성
  plot(x, y, bty = "n", col = "gray", cex = 0.1, xlab = expression(x), ylab = paste0(k, "-th fold  ", expression(y)))
  # k번째 군집의 해당하는 데이터들의 x와 y으로 plot 생성
  points(x_list[[k]], y_list[[k]], bty = "n", col = k + 1, xlab = expression(x), ylab = paste0(k, "-th fold  ", expression(y)))
  # 실제 함수를 그래프에 적합
  lines(x, f)
  # 데이터의 밀도에 따른 띠 모양의 1차원 정보를 좌표축에 표시
  rug(x, col = "gray", lwd = 0.3)
  rug(x_list[[k]], col = k + 1)
  # title 생성
  title(paste0(k, "-th fold data"))
}


### 매듭점 개수에 대한 CV값을 찾기 위한 변수 생성
degree = 3 # 차수 생성
nk_candidate = 3:80 # 매듭점 후보(3~80개)
num_nk_candidate = length(nk_candidate) # 매듭점 후보의 총 개수 


### 각 군집에 해당하는 편차제곱평균값을 저장하기 위해 (예비 매듭점의수)*(군집의 수) 만큼의 행렬 생성
CV_error_list = matrix(0, num_nk_candidate, K)


### 반복을 통하여 각 군집, 매듭점당 CV_error를 구한다.
for(l in 1:num_nk_candidate) # 예비 매듭점의 개수 만큼 반복
{
  num_knots = nk_candidate[l] # 인덱스에 해당하는 예비 매듭점의 개수를 지정
  t = seq(0, 1, length = num_knots) # 매듭점에 해당하는 x값 생성
  t = t[-c(1, length(t))] # 시작점과 끝점(0, 1)을 제거
  num_knots = num_knots - 2 # 시작점과 끝점을 제거했으므로 예비 매듭점의 개수에서 -2를 진행
  
  for(a in 1:K) # 군집의 수만큼 반복
  {
    # K번째를 제외한 군집을 training data로 선정
    x_train = x[- fold_index_list[[a]]]
    y_train = y[- fold_index_list[[a]]]
    
    # K번째 해당하는 군집의 x와 y 값을 정렬하여 저장
    x_test = x_list[[a]]
    x_test = sort(x_test)
    y_test = y_list[[a]][order(x_test)]
    
    # training data에 대한 기저함수를 저장할 변수 생성
    B = NULL
    # test data에 대한 기저함수를 저장할 변수 생성
    B_test = NULL
    
    # 예비 매듭점만큼의 반복을 하면서 
    for(k in 1:num_knots)
    {
      # 기저함수를 사용한 스플라인 함수 적합 (변수-매듭점)^(차수)
      B = cbind(B, Plus(x_train - t[k], degree))
      B_test = cbind(B_test, Plus(x_test - t[k], degree))
    }
    
    # 차수가 0인 경우, 기본기저함수를 포함하지 않음
    if(degree == 0)
    {
      B = cbind(1, B)
      B_test = cbind(1, B_test)
    }
    
    # 차수가 1이상인 경우, 기본기저함수를 포함
    if(degree > 0)
    {
      # poly()함수 : P차 다항식 모델에 해당하는 변수들을 자동적으로 생성하는 함수
      B = cbind(1, poly(x_train, degree, raw = T), B)
      B_test = cbind(1, poly(x_test, degree, raw = T), B_test)
    }
    
    # training data를 이용하여 회귀모델 생성
    fit_spline = lm(y_train ~ B - 1)
    
    # test data를 이용하여 위에 만든 모델에 추정값을 생성
    fit_test = as.vector(B_test %*% fit_spline$coefficients)
    
    # 편차 제곱의 평균을 CV_error_list 행렬에 저장
    CV_error_list[l, a] = mean((y_test - fit_test)^2)
    
  }
}


### matplot() 함수를 이용하여 매듭점의 개수에 따라 표시한 행렬의 해당하는 추세를 한번에 그래프로 표현
par(mfrow = c(1, 2))
matplot(nk_candidate, CV_error_list, type = "l", col = 2:6, lwd = 0.5, bty = "n", xlab = "number of knots", ylab = "CV errors")
lines(nk_candidate, rowMeans(CV_error_list), lwd = 2) # 전체 군집의 평균값도 적합


### 1SD 값 구하기
# 복잡성을 줄이기 위한 방법으로 매듭점을 줄이기 위해 1SD까진 편차를 허용
CV_error = rowMeans(CV_error_list) # 전체 군집의 평균값을 저장
CV_sd = apply(CV_error_list, 1, sd) / sqrt(K) # CV_error_list 데이터의 행(1) 단위로 sd계산 후 루트(군집 수)로 나눠서 SD 생성
CV_upper = CV_error + CV_sd # 평균 CV_error 에서 1SD에 해당하는 값을 더해서 상한 선을 구함
CV_lower = CV_error - CV_sd # 평균 CV_error 에서 1SD에 해당하는 값을 빼서 하한 선을 구함


### 매듭점의 개수에 따라 CV_error_list의 추세를 표현하고 평균, 상한, 하한의 추체를 표현
matplot(nk_candidate, CV_error_list, type = "n", col = 2:6, lwd = 0.5, bty = "n", xlab = "number of knots", ylab = "CV error") # type = "n" 사용
lines(nk_candidate, rowMeans(CV_error_list), lwd = 2)
lines(nk_candidate, CV_upper, col = "blue", lwd = 1, lty = 1)
lines(nk_candidate, CV_lower, col = "red", lwd = 1, lty = 1)


### CV_error 중에서 가장 작은 error의 위치를 표시
CV_index = which.min(CV_error)
abline(v = nk_candidate[CV_index], col = "green", lty = 2)
abline(h = CV_upper[CV_index], col = "orange", lty = 1)
CV_1se_index = min(which(CV_error <= CV_upper[CV_index])) # 상한선(CV_Error + 1SD)보다 작은 값에 해당하는 매듭점의 개수중 가장 작은 개수 선택
abline(v = nk_candidate[CV_1se_index], col = "green", lty = 2)


### best fit by CV
num_knots = nk_candidate[CV_index] # 상한선이 아닌 평균 에러(CV_Error)에 해당하는 매듭점의 개수 저장
t = seq(0, 1, length = num_knots) # 위에서 구한 매듭점의 개수를 사용하여 매듭점의 위치 선정 
B = NULL # 기저함수를 저장할 행렬 생성

# 기저함수 생성
for(k in 1:num_knots)
{
  # 해당하는 (x - 매듭점의 위치)^(차수)의 기저함수를 생성
  B = cbind(B, Plus(x - t[k], degree))
}

# 차수가 0인경우 기본기저함수 추가 안함
if(degree == 0)
{
  B = cbind(1, B)
}

# 차수가 1 이상 경우 기본기저함수 추가
if(degree > 0)
{
  B = cbind(1, poly(x, degree, raw = T), B)
}

# 구해진 기저함수를 이용하여 회귀 적합 진행
fit_spline = lm(y ~ B - 1)

# 적합된 회귀 직선을 실제 데이터와 함수에 적합
plot(x, y, col = "gray", cex = 1, bty = "n")
lines(x, f, lwd = 0.6)
lines(x, fit_spline$fitted.values, col = "red")
title("Fit by CV")


### best fit by CV_1se
num_knots = nk_candidate[CV_1se_index] # 상한선 에러(CV_Error+1SD)보다 작은 CV_Error를 가지는 매듭점의 수중 가장 작은 매듭점의 수를 선택 
t = seq(0, 1, length = num_knots) # 위에서 구한 매듭점의 개수를 사용하여 매듭점의 위치 선정 
B = NULL # 기저함수를 저장할 행렬 생

# 기저함수 생성
for(k in 1:num_knots)
{
  # 해당하는 (x - 매듭점의 위치)^(차수)의 기저함수를 생성
  B = cbind(B, Plus(x - t[k], degree))
}

# 차수가 0인경우 기본기저함수 추가 안함
if(degree == 0)
{
  B = cbind(1, B)
}

# 차수가 1 이상 경우 기본기저함수 추가
if(degree > 0)
{
  B = cbind(1, poly(x, degree, raw = T), B)
}

# 구해진 기저함수를 이용하여 회귀 적합 진행
fit_spline = lm(y ~ B - 1)

# 적합된 회귀 직선을 실제 데이터와 함수에 적합
plot(x, y, col = "gray", cex = 1, bty = "n")
lines(x, f, lwd = 0.6)
lines(x, fit_spline$fitted.values, col = "red")
title("Fit by CV-1se")

