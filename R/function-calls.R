
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
f(5 * x)
f(foo + bar)

eval(f())
eval(f(5 * x))
x <- 2
eval(f(5 * x))


f <- function(x = 1:3, y = x) substitute(x + y)
f()
f(y = 5 * x)

x <- 5
f(x = 5 * x)
eval(f(x = 5 * x))

g <- function(x = 1:3, y = x) x + y
g(x = 5 * x)


f <- function(x) {
  cat(deparse(substitute(x)), "==", x)
}
f(2 + x)

class(substitute(5))
f <- function(x) substitute(x)
class(f(5))

f <- function(x) {
  y <- 2*x
  substitute(y)
}
f(5)
class(f(5))

f <- function(x) {
  y <- 2*x
  substitute(x + y)
}
f(5)
class(f(5))

f <- function(x, y = x) substitute(y)
f(5)
class(f(5))
f(5, 5)
class(f(5, 5))

f <- function(x, y) substitute(x + y)
f(5, 5)
class(f(5, 5))

my_call <- f(5, 5)
as.list(my_call)
eval(my_call)

rm(x) ; rm(y)
my_call <- f(x, y)
as.list(my_call)

eval(my_call)

eval(my_call, list(x = 5, y = x))
x <- 5; y <- 2
eval(my_call)

my_call[[1]] <- `-`
eval(my_call)

length(my_call)

(my_call <- call("+", 2, 2))
eval(my_call)

(my_call <- call("+", x, y))
(my_call <- call("+", x - y, x + y))
eval(my_call)

f <- function(x, y, z) {
  match.call()
}
(my_call <- f(2, 4, x + y))
class(my_call)
eval(my_call)
x <- call("f", 2, 4, 6)
x[[1]] <- `sum`
eval(x)
x
