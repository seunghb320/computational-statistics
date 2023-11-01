#1번
# 이분법
Bisection = function(x0, x1, epsilon = 1e-4)
{
  fx0 = f(x0)
  fx1 = f(x1)
  if (fx0 * fx1 >0)  
    return("wrong initial values")
  error = abs(x1 - x0)
  N = 1
  while (error > epsilon)
  {
    N = N + 1
    error = error / 2
    x2 = (x0 + x1) / 2
    fx2 = f(x2)
    if (fx0 * fx2 < 0)
    {
      x1 = x2; fx1 = fx2
    } else
    {
      x0 = x2; fx0 = fx2
    }
  }
  
  return(list(x = x2, n = N))
}

f = function(x) {x^4-4*x-1}
result = Bisection(1,2)
result

# 뉴턴법
Newton = function(x0, epsilon = 1e-5, n = 100)
{
  e = 1
  N = 1
  d = epsilon
  while (e > epsilon)
  {
    N = N + 1
    if (N > n) 
      return("not converge after 100 iterations")
    x1 = x0 - f(x0) * d / (f(x0 + d) - f(x0))#수치미분으로 한거임
    e = abs(x1 - x0)
    x0 = x1
  }
  
  return(list(x = x1, n = N))
}

#Newton(1)

f = function(x) {acos(4/x)}

Newton(1)

