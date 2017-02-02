
x <- quote(if (foo) bar)
length(x)
x[[1]]
x[[2]]
x[[3]]

y <- quote(if (foo) bar else baz)
length(y)
y[[1]]
y[[2]]
y[[3]]
y[[4]]

z <- quote(for (x in 1:4) print(x))
length(z)
z[[1]]
z[[2]]
z[[3]]
z[[4]]

eval(z)

z[[3]] <- 1:2
eval(z)

z[[4]] <- quote(print(x + 2))
eval(z)

z[[2]] <- quote(y)
z[[4]] <- quote(print(y))
eval(z)

x <- quote(sin(2))
eval(x)
x[[1]] <- quote(cos)
eval(x)
x[[2]] <- 0
eval(x)


f <- function(expr, indent = "") {
  if (is.atomic(expr) || is.name(expr)) { # basic case
    print(paste0(indent, expr))
    
  } else if (is.call(expr)) { # a function call / sub-expression
    print(paste0(indent, expr[[1]]))
    n <- length(expr)
    if (n > 1) {
      new_indent <- paste0(indent, "  ")
      for (i in 2:n) {
        f(expr[[i]], new_indent)
      }
    }
    
  } else {
    print(paste0(indent, "Unexpected expression: ", expr[[1]]))
  }
}

f(quote(2 + 3*(x + y)))
