
f <- function(x = 5) {
  y <- 2 * x
  sys.function()
}
f
f()

f <- function(x, y = 2 * x) formals()
params <- f(1, 2)
class(params)
params

f <- function(x, y = 2 * x) { 
  z <- x - y
  body()
}
f(2)

f <- function() {
  x <- 1
  y <- 2
  z <- 3
  environment()
}
env <- f()
as.list(env)

f <- function() {
  x <- 1
  y <- 2
  z <- 3
  parent.env(environment())
}
f()

f <- function(x = 1:3) {
  print(formals()$x)
  x
}
f(x = 4:6)


f <- function(x = 1:3) substitute(x)
f()
x <- 5
f(5 * x)

eval(f())
eval(f(5 * x))
x <- 2
eval(f(5 * x))


f <- function(x = 1:3, y = x) substitute(x + y)
f()
f(y = 5 * x)

x <- 5
eval(f())
eval(f(5 * x))

f <- function(x = 1:3, y = x) substitute(y)
f()
f(5 * x)

eval(f(5 * x))
eval(f(5 * x), environment(f))

f <- function(x, y, z) {
  print(substitute(x))
  print(substitute(y))
  print(substitute(z))
}
f(2, 4, sin(2 + 4))

f <- function(x, y, z) {
  as.list(match.call())
}
f(2, 4, sin(2 + 4))
g <- f
g(2, 4, sin(2 + 4))

