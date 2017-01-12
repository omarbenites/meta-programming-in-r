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

If we actually want the arguments parsed to the current function in the form of the promises they are really represented as, we need to get hold of them without evaluating them. If we take an argument and use it as an expression, the promise will be evaluated. This goes for both default parameters and parameters provided in the function call; they are all promises that will be evaluated in different environments, but they are all promises nonetheless.

One way to get the expression that the promises represent is to use the function `substitute`. This function, which we will get intimately familiar with in the chapter *[Manipulation of expressions]*, substitutes into an expression the values that variables refer to. This means that variables are replaced by the verbatim expressions, the expressions are not evaluated before they are substituted into an expression.

This small function illustrate how we can get the expression passed to a function:

```{r}
f <- function(x = 1:3) substitute(x)
f()
```

Here we see that calling `f` with default parameters gives us the expression `1:3` back. This is similar to the `formals` we saw earlier in the section. We substitute `x` with the expression it has in its formal arguments; we do not evaluate the expression. We can, of course, once we have the expression

```{r}
eval(f())
```

but it isn't done when we call `substitute`.

```{r}
f(5 * x)
f(foo + bar)
```

Because the substituted expression is not evaluated, we don't even need to call the function with an expression that *can* be evaluated.

```{r}
f(5 + "string")
```

The substitution is verbatim. If we set up default parameters that depend on others, we just get them substituted with variable names; we do not get the value assigned to other variables.

```{r}
f <- function(x = 1:3, y = x) substitute(x + y)
f()
f(x = 4:6)
f(y = 5 * x)
```

In this example, we we also see that we can call `substitute` with an expression instead of a single variable, we see that `x` gets replaced with the argument given to `x`, whether default or actual, and `y` gets replaced with `x` as the default parameter -- not the values we provide for `x` in the function call, and with the actual argument when we provide it.

If we try to evaluate the expression we get back from the call to `f` we will not be evaluating it in the evaluation environment of `f`. That environment is not preserved in the substitution.

```{r}
x <- 5
f(x = 5 * x)
eval(f(x = 5 * x))
```

The expression we evaluate is `5 * x + x`, not `5 * x + 5 * x` as it would be if we substituted the value of `x` into `y`, as we would if we evaluated the expression inside the function.

```{r}
g <- function(x = 1:3, y = x) x + y
g(x = 5 * x)
```

A common use for `substitute` is to get the expression provided to a function as a string. This is used in the `plot` function, for instance, to set the default labels of a plot to the expressions `plot` is called with. Here, `substitute` is used in combination with the `departs` function. This function takes an expression and translate it into its text representation.

```{r}
f <- function(x) {
  cat(deparse(substitute(x)), "==", x)
}
f(2 + x)
f(1:4)
```

Here, we use the `deparse(substitute(x))` pattern to get a textual representation of the argument `f` was called with, and the plain `x` to get it evaluated.

The actual type of object returned by `substitute` depends on the expression we give the function and the expressions variables refer to. If the expression, after variables have been substituted, is a simple type, that is what `substitute` returns.

```{r}
f <- function(x) substitute(x)
f(5)
class(f(5))
```

If you give `substitute` a local variable you have assigned to, you also get a value back. This is not because `substitute` does anything special here; local variables like these are not promises, we have evaluated an expression when we assigned to one.

```{r}
f <- function(x) {
  y <- 2 * x
  substitute(y)
}
f(5)
class(f(5))
```

This behaviour only works inside functions, though. If you call `substitute` in the global environment it considers variables as names and does not substitute them for their values.

```{r}
x <- 5
class(substitute(5))
class(substitute(x))
```

It will substitute variables for values if you give a function a simple type as argument.

```{r}
f <- function(x, y = x) substitute(y)
f(5)
class(f(5))
f(5, 5)
class(f(5, 5))
```

If the expression that `substitute` evaluates to is a single variable, the type it returns is `name`, as we just saw. For anything more complicated, `substitute` will return a `call` object. Even if it is an expression that could easily be evaluated to a simple value; `substitute` does not evaluate expressions, it just substitute variables.

```{r}
f <- function(x, y) substitute(x + y)
f(5, 5)
class(f(5, 5))
```

A `call` object refers to an unevaluated function call. In this case, we have the expression `5 + 5`, which is the function call `` `+`(5, 5) ``.

Such `call` objects can also be manipulated. We can translate a `call` into a list to get its components and we can evaluate it to invoke the actual function call.

```{r}
my_call <- f(5, 5)
as.list(my_call)
eval(my_call)
```

Since `substitute` doesn't actually evaluate a call, we can create function call objects with variables we can later evaluate in different environments.

```{r}
rm(x) ; rm(y)
my_call <- f(x, y)
as.list(my_call)
```

Here, we have created the call `x + y`, but removed the global variables `x` and `y`, so we cannot actually evaluate the call.

```{r}
eval(my_call)
```

We can, however, provide the variables when we evaluate the call

```
eval(my_call, list(x = 5, y = x))
```

or we can set global variables and evaluate the call in the global environment.

```{r}
x <- 5; y <- 2
eval(my_call)
```

We can treat a `call` as a list and modify it. The first element in a `call` is the function we will call, and we can replace it like this:

```{r}
(my_call <- f(5, 5))
my_call[[1]] <- `-`
eval(my_call)
```

The remaining elements in the `call` are the arguments to the function call, and we can modify these as well:

```{r}
my_call[[2]] <- 10
eval(my_call)
```

You can also create `call` objects manually using the `call` function. The first argument to `call` is the name of the function to call, and any additional arguments are parsed on this function when the `call` object is evaluated.

```{r}
(my_call <- call("+", 2, 2))
eval(my_call)
```

Unlike `substitute` inside a function, however, the arguments to `call` *are* evaluated when the call object is constructed. These are not lazy-evaluated.

```{r}
(my_call <- call("+", x, y))
(my_call <- call("+", x - y, x + y))
```

From inside a function, you can get the call used to invoke it using the `match.call` function.

```{r}
f <- function(x, y, z) {
  match.call()
}
(my_call <- f(2, 4, sin(2 + 4)))
as.list(my_call)
```

From the first element in this call you can get the name of the function as it was actually called. Remember that the function itself doesn't have a name, but in the call to the function we have a reference to it, and we can get hold of that reference through the `match.call` function.

```{r}
g <- f
(my_call <- g(2, 4, sin(2 + 4)))
my_call[[1]]
```

This function is often used to remember a function call in statistical models, where the call to the model constructor is saved together with the fitted model.

## Accessing the calling scope

