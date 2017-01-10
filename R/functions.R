
f <- function(x) x

formals(f)
environment(f)
body(f)

eval(body(f))
eval(body(f), list(x = 2))

f <- function(x = 2) x
formals(f)
eval(body(f), formals(f))


formals(f) <- list(x = 3)
f
eval(body(f), formals(f))

body(f) <- 6
f
eval(body(f))

f()
f(x = 12)

f <- function(x = y, y = 2) y*x
f()

eval(body(f), formals(f))

formals(f) <- list(x = 3, y = quote(x))
f()




g <- function(x = 1, y = 2, z = 3) x + y + z
parameters <- formals(g)
for (param in names(parameters)) {
  cat(param, " => ", '"', parameters[[param]], '"', "\n", sep = "")
}

g <- function(x, y, z = 3) x + y + z
parameters <- formals(g)
for (param in names(parameters)) {
  cat(param, " => ", '"', parameters[[param]], '"',
      " (", class(parameters[[param]]), ")\n", sep = "")
}

f <- function(x = y, y = 5) {
  x + y
}

eval(body(f), formals(f))

fenv <- new.env()
parameters <- formals(f)
for (param in names(parameters)) {
  delayedAssign(param, parameters[[param]], fenv, fenv)
}
eval(body(f), fenv)

f <- function(x = y, y = 3) {
  typeof(x)
}
f()




enclosing <- function() {
  z <- 2
  function(x, y = x) {
    x + y + z
  }
}

f <- enclosing()

calling <- function() {
  w <- 5
  f(x = 2 * w)
}

calling()


defenv <- new.env()
evalenv <- new.env(parent = defenv)
callenv <- new.env()

defenv$z <- 2
defenv$f <- function(x, y = x) x + y + z

callenv$w <- 5

delayedAssign("x", 2 * w, callenv, evalenv)
delayedAssign("y", x, evalenv, evalenv)
evalenv$y

eval(body(defenv$f), evalenv)


nested <- function() {
  y <- 5
  function(x) x
}

f <- nested()
formals(f) <- list(x = quote(y))
f

f()

f <- as.function(alist(x =, y = 2, x + y))
f
f(2)

nested <- function(z) {
  as.function(alist(x =, y = z, x + y))
}
g <- nested(3)
g
h <- nested(4)
h

g(2)
h(2)

nested <- function(y) {
  as.function(alist(x =, y = y, x + y))
}

nested2 <- function(y) {
  z <- y
  function(x, y = z) x + y
}
nested2(2)(2)


f <- function(x) x + y
formals(f) <- list(x =, y =)
formals(f) <- alist(x =, y =)


f <- as.function(list(x = 2, y = 2, x + y))
f
f(2)


nested <- function(z) {
  as.function(alist(x =, y = z, x + y))
}
nested2 <- function(z) {
  as.function(alist(x =, y = z, x + y), envir = .GlobalEnv)
}
z <- -1
nested(3)(1)
nested2(3)(1)
