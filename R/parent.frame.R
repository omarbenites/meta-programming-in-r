
nested <- function(x) {
  function(local) {
    if (local) x
    else get("x", parent.frame())
  }
}

f <- nested(2)
f(TRUE)
x <- 1
f(FALSE)

nested <- function(x) {
  y <- 2
  function(local) {
    z <- 2
    expr <- expression(x + y + z)
    if (local) eval(expr)
    else eval(expr, envir = parent.frame())
  }
}

f <- nested(2)
x <- y <- z <- 1
f(TRUE)
f(FALSE)