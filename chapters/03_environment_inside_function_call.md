# Inside a function-call

When we execute the body of a function, as we have seen, we do this in the evaluation environment, that is linked through its parent to the environment where the function was defined, and with arguments stored as promises that will be evaluated either in the environment where the function was defined, for default parameters, or in the environment where the function was called, for parameters provided to the function there. In the last chapter, we saw how we could get hold of the formal parameters of a function, the body of the function, and the environment in which the function was defined. In this chapter we will examine how we can access these, and more, from inside a function while the function is being evaluated.

## Getting the components of the current function

In the last chapter, we could get the `formals`, `body`, and `environment` of a function we had a reference to. Inside a function body, we do not have such a reference. Functions do not have names as such; we give functions names when we assign them to variables, but that is a property of the environment where we have the name, not of the function itself, and functions we use as closures are often never assigned to any name at all. So how do we get hold of the current function to access its components?

To get hold of the current function, we can use the function `sys.function`. This function gives us the definition of the current function, which is what we really need, not its name. 

We can define this function to see how `sys.function` works:

```{r}
f <- function(x = 5) {
  y <- 2 * x
  sys.function()
}
```

If we just write the function name on the prompt we get the function definition:

```{r}
f
```

and since the function return the definition of itself, we get the same when we evaluate it:

```{r}
f()
```

When we call any of `formals`, `body`, or `environment`, we don't actually use a function name as the first parameter, we give them a reference to a function and they get the function definition from that.

We don't need to explicitly call `sys.function` for `formals` and `body`, though, because these two functions already use a call to `sys.function` for the default value for the function parameter, so if we want the components of the current function, we can simply leave out the function parameter.

Thus, to get the formal parameter of a function, inside the function body, we can just use `formals` without any parameters.

```{r}
f <- function(x, y = 2 * x) formals()
params <- f(1, 2)
class(params)
params
```

The same goes for the body of the current function:

```{r}
f <- function(x, y = 2 * x) { 
  z <- x - y
  body()
}
f(2)
```

The `environment` function works slightly different. If we call it without parameters we get the current (evaluating) environment.

```{r}
f <- function() {
  x <- 1
  y <- 2
  z <- 3
  environment()
}
env <- f()
as.list(env)
```

This is not what we would get with `environment(f)`:

```{r}
environment(f)
```

The `f` function is defined in the global environment, and `environment(f)` gives us the environment in which `f` is defined. If we call `environment()` inside `f` we get the evaluating environment. The local variables `x`, `y`, and `z` can be found in the evaluating environment but they are not part of `environment(f)` -- or if they are, they are different, global, parameters.

To get the equivalent of `environment(f)` from inside `f` we must get hold of the parent of the evaluating environment. We can get the parent of an environment using the function `parent.env`, so we can get the definition environment like this:

```{r}
f <- function() {
  x <- 1
  y <- 2
  z <- 3
  parent.env(environment())
}
f()
```

When we have hold of a function definition, as in the previous chapter, we do not have an evaluating environment. That environment only exists when the function is called. A function we have defined, but not invoked, has the three components we covered in the previous chapter, but there are more components to a function that is actively executed; there is a difference between a function definition, a description of what a function should do when it is called, and a function instantiation, the actual running code. One such difference is the evaluating environment. Another is that a function instantiation has actual parameters while a function definition only has formal parameters. The latter are part of the function definition; the former are provided by the caller of the function.

## Accessing actual function-parameters

We can see the difference between formal and actual parameters in the example below:

```{r}
f <- function(x = 1:3) {
  print(formals()$x)
  x
}
f(x = 4:6)
```

The `formals` give us the arguments as we gave them in the function definition, where `x` is set to the expression `1:3`. It is a *promise*, to be evaluated in the defining scope when we access `x` in the cases where no parameters were provided in the function call, so in the `formals` list it is not the values 1, 2, and 3, but the expression `1:3`. In the actual call, though, we *have* provided the `x` parameter, so what this function call returns is `4:6`, but because we return it as the result of an expression this promise is evaluated so `f` returns 4, 5, and 6.




```{r}
f <- function(x, y, z) {
  print(substitute(x))
  print(substitute(y))
  print(substitute(z))
}
f(2, 4, sin(2 + 4))

f <- function(x, y, z) {
  as.list(match.call())
}
f(2, 4, sin(2 + 4))
g <- f
g(2, 4, sin(2 + 4))
```

## Accessing the calling scope

