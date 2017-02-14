subset2 <- function(x, condition) {
  print(deparse(substitute(condition)))
  condition_call <- substitute(condition)
  r <- eval(condition_call, x, parent.frame())
  x[r,]
}

x <- data.frame(a = 1:5, b = 1:5)
subset2(x, a > 3)

scramble <- function(x) x[sample(nrow(x)),]
subscramble <- function(x, condition) {
  print(deparse(substitute(condition)))
  scramble(subset2(x, condition))
}

subscramble(x, a > 3)


quote(x + y)
bquote(x + y)

bquote(.(2+2) + y)
call("+", 2+2, quote(y))

bquote((.(2+2) + x) * y)
call("*", call("(", call("+", 2+2, quote(x))), quote(y))

deparse(quote(x + y))
parse(text = "x + y")

(expr <- parse(text = "x + y; z * x"))
expr[[1]]
expr[[2]]

f <- function(x) deparse(x)
g <- function(x) deparse(substitute(x))

x <- 1:4; y <- x**2
f(x + y)
g(x + y)

substitute(x + y)
f <- function(x, y) substitute(x + y)
f(2, 3)

y - 3
f <- function(x) substitute(x + y)
f(2)

f <- function(x) function(y) substitute(x + y)
g <- f(2)
g(3)

e <- new.env(parent = emptyenv())
e$x <- 2
e$y <- 3
substitute(x + y, e)

substitute(x + y, list(x = 2, y = 3))

x <- 2 ; y <- 3
e <- new.env(parent = globalenv())
substitute(x + y, e)

e <- new.env(parent = globalenv())
e$x <- 2
e$y <- 3
e2 <- new.env(parent = e)
substitute(x + y, e2)


expr <- quote(x + y)
substitute(expr)
substitute(expr, list(expr = expr))
substitute(substitute(expr, list(expr = expr)), list(y = 2))
eval(substitute(substitute(expr, list(expr = expr)), list(y = 2)))

substitute(substitute(expr, list(y = 2)), list(expr = expr))
eval(substitute(substitute(expr, list(y = 2)), list(expr = expr)))

f <- function() {
  expr <- quote(x + y)
  substitute(expr)
}
f()

f <- function() {
  expr <- quote(x + y)
  y <- 2
  substitute(expr)
}
f()

f <- function() {
  expr <- quote(x + y)
  substitute(expr, list(y = 2))
}
f()

substitute(expr, globalenv())


f <- function(x) function(y = x) substitute(y)
g <- f(2)
g()
x <- 4
g(x)




f <- function(expr) substitute(expr)
f(x + y)
g <- function(expr) f(expr)
g(x + y)

x <- 2; y <- 3
eval(f(x + y))
eval(g(x + y))

x <- 2; y <- 3
f <- function(x) promise_info(x)
f(x + y)

g <- function(x) {
  print(promise_info(x))
  force(x)
  promise_info(x)
}
g(x + y)


eval(quote(x + y), list(x = 2, y = 3))

d <- data.frame(x = 1:2, y = 3:3)
eval(quote(x + y), d)

x <- 2; y <-  3
eval(x + y, d)

my_with <- function(df, expr) {
  eval(substitute(expr), df)
}
d <- data.frame(x = rnorm(5), y = rnorm(5))
my_with(d, x + y)

z <- 1
with(d, x + y + z)
my_with(d, x + y + z)

f <- function(z) with(d, x + y + z)
f(2)
g <- function(z) my_with(d, x + y + z)
g(2)

my_with <- function(df, expr) {
  eval(substitute(expr), df, parent.frame())
}

f <- function(z) with(d, x + y + z)
f(2)
g <- function(z) my_with(d, x + y + z)
g(2)



x <- 2; y <- 3
f <- function(d, expr) my_with(d, expr)
f(d, x + y)
g <- function(d, expr) my_with(d, substitute(expr))
g(d, x + y)

my_with_q <- function(df, expr) {
  eval(expr, df, parent.frame())
}
my_with <- function(df, expr) my_with_q(d, substitute(expr))

g <- function(d, expr) my_with_q(d, substitute(expr))
g(d, x + y)
my_with(d, x + y)



d <- data.frame(x = 1:2, y = 3:4)
macro_q <- function(expr, ...) {
  eval(substitute(substitute(expr, list(...))))
}
macro <- function(expr, ...) {
  eval(eval(substitute(macro_q(expr, ...))), parent.frame())
}
macro_q(d$x <- val, val = 4:5)
d$x
macro(d$x <- val, val = 4:5)
d$x
