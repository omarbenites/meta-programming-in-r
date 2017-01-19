

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

