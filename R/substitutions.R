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

make_param_names <- function(params) {
  param_names <- names(params)
  if (is.null(param_names)) param_names <- rep("", length(params))
  for (i in seq_along(param_names)) {
    if (param_names[i] == "") {
      param_names[i] <- paste(params[[i]])
    }
  }
  param_names
}

make_macro <- function(..., body) {
  body <- substitute(body)
  params <- eval(substitute(alist(...)))
  
  # Construct macro
  f <- eval(substitute(
    function() eval(substitute(body), parent.frame())
  ))

  # Set macro arguments
  param_names <- make_param_names(params)
  names(params) <- param_names
  params <- as.list(params)
  formals(f) <- params
  
  f
}

(m <- make_macro(body = x + y))
x <- 2; y <- 4
m()

set_NA_val <- make_macro(df, var, na_val, 
                         body = df$var[df$var == na_val] <- NA)

(d <- data.frame(x = c(1,-9,3,4), y = c(1,2,-9,-9)))
set_NA_val(d, x, -9); d
set_NA_val(d, y, -9); d

set_NA_val_fun <- function(df, var, na_val) {
  df[df[,var] == na_val, var] <- NA
  df
}
(d <- data.frame(x = c(1,-9,3,4), y = c(1,2,-9,-9)))
(d <- set_NA_val_fun(d, "x", -9))
(d <- set_NA_val_fun(d, "y", -9))

library(magrittr)
d <- data.frame(x = c(1,-9,3,4), y = c(1,2,-9,-9)) %>%
  set_NA_val_fun("x", -9) %>% set_NA_val_fun("y", -9)
d

rm(z)

e <- list2env(list(x = 2, y = 3))
eval(quote(z <- x + y), e)
as.list(e)

l <- list(x = 2, y = 3)
eval(quote(z <- x + y), l)
l



library(pryr)
f <- function(x, y) function(z = x + y) promise_info(z)
g <- f(2, 3)
g()
x <- 4; y <- 5
g(x + y)


x <- 2; y <- 3
f <- function(x) promise_info(x)
f(x + y)

g <- function(x) {
  cat("=== Before evaluation =====")
  print(promise_info(x))
  force(x)
  cat("=== After evaluation ======")
  promise_info(x)
}
g(x + y)

f <- function(x, y) function(expr = x + y) {
  pi <- promise_info(expr)
  expr <- eval(substitute(
    substitute(expr, list(y = quote(2 * y)))))
  value <- eval(expr, pi$env)
  list(expr = expr, value = value)
}
g <- f(2, 2)
g()
x <- y <- 4
g(x + y)
z <- 4
g(z)

f <- function(x, y) function(expr = x + y) {
  pi <- promise_info(expr)
  expr <- eval(substitute(substitute(code, list(y = quote(2 * y))), list(code = pi$code)))
  value <- eval(expr, pi$env)
  list(expr = expr, value = value)
}
g <- f(2, 2)
g()
x <- y <- 4
g(x + y)
z <- 4
g(z)

rm(y)
g(z)



f <- function(x, y) function(expr = x + y) {
  pi <- promise_info(expr)
  expr <- eval(substitute(
    substitute(expr, list(y = 2 * y))))
  value <- eval(expr, pi$env)
  list(expr = expr, value = value)
}
g <- f(2, 2)
g()
x <- y <- 4
g(x + y)
z <- 4
g(z)
