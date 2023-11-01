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

#3번

f = function(x) {exp(-x^2) + exp(-1/x^2)/x^2}

#직사각형법
Integral = function(a, b, n)
{
  integral = 0
  h = (b - a) / n
  for (i in 1:n)
    integral = integral + h * f(a + (i-1/2) * h)
  
  return(integral)
}
Integral(0,1,20)

# 사다리꼴법
Trapezoid = function(a, b, n)
{
  h = (b - a) / n
  integral = (f(a) + f(b)) / 2
  
  x = a
  n1 = n - 1
  for (i in 1:n1)
  {
    x = x + h
    integral = integral + f(x) 
  }
  integral = integral * h
  
  return(integral)
}

Trapezoid(0,1,20)

# 심슨 적분법
Simpson = function(a, b, n)
{
  h = (b - a) / n
  integral = f(a) + f(b)
  x2 = a
  x3 = a + h
  even = 0
  odd = f(x3)
  h2 = 2 * h
  n1 = n / 2 - 1
  for (i in 1:n1)
  {
    x2 = x2 + h2
    x3 = x3 + h2
    even = even + f(x2)
    odd = odd + f(x3)
  }
  integral = (integral + 4 * odd + 2 * even) * h / 3
  
  return(integral)
}

Simpson(0,1,2)
