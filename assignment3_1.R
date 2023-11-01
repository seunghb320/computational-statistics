# 1a번
gaussianelimination = function(A,b){
  n = nrow(A)
  for (k in (1:(n-1))){
    for (i in ((k+1):n)){
      mik = A[i,k]/A[k,k]
      A[i,k]=0
      b[i] = b[i] - mik*b[k]
      for (j in ((k+1):(n))){
        A[i,j] = A[i,j] - mik*A[k,j]
      }
    }
  }
  print(b)
  return(A)
}

GJinv = function(Ab){
  n = nrow(Ab)
  I = diag(n)
  for (k in (1:(n-1))){
    for (i in ((k+1):n)){
      mik = Ab[i,k]/Ab[k,k]
      for (j in ((k-1):(n))){
        Ab[i,j] = Ab[i,j] - mik*Ab[k,j]
        I[i,j] = I[i,j] - mik*I[k,j]
      }
    }
  }
  for (k in (n:2)){
    for (i in ((k-1):1)){
      mik = 1/Ab[k,k]
      Ab[k,k] = mik*Ab[k,k]
      I[k,] = mik*I[k,]
      mik = Ab[i,k]/Ab[k,k]
      for (j in ((1):(n))){
        Ab[i,j] = Ab[i,j] - mik*Ab[k,j]
        I[i,j] = I[i,j] - mik*I[k,j]
      }
    }
  }
  return(I)
}

backwardsub = function(U,b){
  x = c(0)
  n = nrow(U)
  for (i in (n:1)){
    x[i] = b[i]
    if (i < n){
      for (j in ((i+1):n)){
        x[i] = x[i] - U[i,j]*x[j]
      }
    }
    x[i] = x[i]/U[i,i]
  }
  return(cbind(x))
}

A = matrix(c(1,2,3,-1,3,2,2,1,-2), byrow = T, ncol = 3)
b= matrix(c(10,5,6), byrow = F, ncol = 1)
 
GJinv(A) %*% b
gaussianelimination(A,b)
b= matrix(c(10,15,-5), byrow = F, ncol = 1)
backwardsub(gaussianelimination(A,b),b)

#1b번
# LU 분해 
lufactorization = function(A){
  n = nrow(A)
  L = matrix(0,nrow=n,ncol=n)
  for (k in (1:(n-1))){
    for (i in ((k+1):n)){
      L[i,k] = A[i,k]/A[k,k]
      A[i,k] = 0
      for (j in ((k+1):n)){
        A[i,j] = A[i,j] - L[i,k]*A[k,j]
      }
    }
  }
  for (k in (1:n)) L[k,k] = 1
  return(cbind(L,A))
}
A = matrix(c(1,2,3,-1,3,2,2,1,-2), byrow = T, ncol = 3)
b= matrix(c(10,5,6), byrow = F, ncol = 1)
A=lufactorization(A)
L=matrix(A[1:9], ncol=3)
L
U=matrix(A[10:18], ncol=3)
U
library(limSolve)
y=Solve(L,b)
y
x=Solve(U,y)
x

#2번
library(limSolve)
gaussianelimination2 = function(Ab,b){
  n = nrow(Ab)
  for (k in (1:(n-1))){
    for (i in ((k+1):n)){
      mik = Ab[i,k]/Ab[k,k]
      Ab[i,k]=0
      for (j in ((k+1):(n))){
        Ab[i,j] = Ab[i,j] - mik*Ab[k,j]
      }
      b[i] = b[i] - mik*b[k]
    }
  }
  return(Solve(Ab,b))
}
dv = function(n){
  v = matrix(0,nrow=n,ncol=n)
  count = 2
  for (i in 1:n){
    for (j in 1:n){
      v[i,j] = count**(j-1)
    }
    count = count+1
  }
  return(v)
}

db = function(n){
  b = matrix(0,nrow=1,ncol=n)
  for (i in 1:n){
    b[i] = ((1+i)**n-1)/i
  }
  return(b)
}

norm = matrix(0,nrow=1,ncol=9)

for (i in 2:10){
  a = gaussianelimination2(dv(i),db(i))
  print(a)
  a=a-1
  a=matrix(a)
  n = nrow(a)
  d=0
  for (i in 1:n){
    d = d + a[i]**2
  }
  norm[i-1] = sqrt(d)
}
print(norm)

plot(2:10,norm,type='b',xlab='n',ylab='||xn - x0||',main='norm 그래프')