

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

env <- new.env()
x <- 5
exists("x", env)
get("x", env)
env$x

assign("x", 3, envir = env)
env$x
x

#env <- new.env(parent = NULL)

env <- new.env(parent = emptyenv())
exists("x", env)

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

library(MASS)
environment(mvrnorm)
parent.env(environment(mvrnorm))
parent.env(parent.env(environment(mvrnorm)))

pryr::address(parent.env(parent.env(environment(sd))))
pryr::address(parent.env(parent.env(environment(mvrnorm))))
