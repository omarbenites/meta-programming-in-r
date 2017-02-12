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


expr <- quote(x + y)
substitute(expr)
substitute(expr, list(expr = expr))
substitute(substitute(expr, list(expr = expr)), list(y = 2))
substitute(substitute(expr, list(y = 2)), list(expr = expr))
eval(substitute(substitute(expr, list(y = 2)), list(expr = expr)))
