def fib(n):
 if n == 0 or n == 1 : #123 
  return n
 elif n >=1:
  return fib(n-1) + fib(n-2)