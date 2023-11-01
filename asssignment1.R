x = 1
taylor = 1/2 - x^2/24 + x^4/720
original = (1-cos(x))/x^2
sin = (sin(x/2))^2/(x^2/2)
print(original)
print(taylor)
print(sin)

x = 1e-2
taylor = 1/2 - x^2/24 + x^4/720
original = (1-cos(x))/x^2
sin = (sin(x/2))^2/(x^2/2)
print(original)
print(taylor)
print(sin)

x = 1e-4
taylor = 1/2 - x^2/24 + x^4/720
original = (1-cos(x))/x^2
sin = (sin(x/2))^2/(x^2/2)
print(original)
print(taylor)
print(sin)

x = 1e-6
taylor = 1/2 - x^2/24 + x^4/720
original = (1-cos(x))/x^2
sin = (sin(x/2))^2/(x^2/2)
print(original)
print(taylor)
print(sin)

x = 1e-8
taylor = 1/2 - x^2/24 + x^4/720
original = (1-cos(x))/x^2
sin = (sin(x/2))^2/(x^2/2)
print(original)
print(taylor)
print(sin)

x = 1e-10
taylor = 1/2 - x^2/24 + x^4/720
original = (1-cos(x))/x^2
sin = (sin(x/2))^2/(x^2/2)
print(original)
print(taylor)
print(sin)

s = 0
for (i in (1:1e7)){
  a = 1/(i*(i+1))
  s = s + a
}
s2 = 0
for (i in (1e7:1)){
  a = 1/(i*(i+1))
  s2 = s2 + a
}
sum = 1e7/(1e7+1)

0.1 == 3-2.9
all.equal(0.1, 3-2.9)

library(Rmpfr)
help(Rmpfr)
exp(10)
exp(-10)
mpfr(exp(10), precBits=120)
mpfr(exp(-10), precBits=120)
