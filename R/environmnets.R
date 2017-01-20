

my_search <- function(env) {
  repeat {
    name <- environmentName(env)
    if (nchar(name) != 0) 
      name <- paste0(name, "\n")
    else
      name <- str(env, give.attr = FALSE)
    cat(name)
    env <- parent.env(env)
    if (identical(env, emptyenv())) break
  }
}

my_search(.GlobalEnv)
library(ggplot2)

environment(ls)
my_search(environment(ls))
baseenv()

environment(ls)
my_search(environment(ls))


f <- function() {
  g <- function() {
    h <- function() {
      function(x) x
    }
    h()
  }
  g()
}
my_search(environment(f()))

my_search(environment(sd))
environment(sd)
parent.env(environment(sd))
parent.env(parent.env(environment(sd)))



f <- function(x) {
  g <- function(y, z) x + y + z
  g
}
h <- function(a) {
  g <- f(x)
  i <- function(b) g(a + b, 5)
}
x <- 2
i <- h(1)
i(3)


f <- function() {
  my_env <- environment()
  parent.env(my_env) <- parent.frame()
  x
}
g <- function(x) f()
g(2)

f <- function() {
  my_env <- environment()
  call_env <- parent.frame()
  
  parent.env(my_env) <- call_env
  parent.env(call_env) <- parent.frame(2)

  x + y
}
g <- function(y) f() + a
h <- function(x) {
  a <- 1
  g(2)
}
h(3)

f <- function() {
  my_env <- environment()
  call_env <- parent.frame()
  parent.env(my_env) <- call_env
  y
}
g <- function(x) {
  closure <- function(y) {
    z <- f()
    z + x
  }
  closure
}
add1 <- g(1)
add2 <- g(2)

x <- 3
add1(3)
add2(4)

f <- function() {
  my_env <- environment()
  call_env <- parent.frame()
  parent.env(my_env) <- call_env
  parent.env(call_env) <- parent.frame(2)
  y
}

rm(x)
add1(3)
add2(4)
x <- 3
add1(3)
add2(4)





make_dynamic_scope <- function(f) {
  get_caller_chain <- function() {
    n <- sys.nframe() # at least 2
    print(n)
    start <- new.env()
    # start one extra frame down to search from caller...
    for (i in seq(2,n)) {
        
    }
    
  }
  wrapper <- function(...) {
    
  }
}
