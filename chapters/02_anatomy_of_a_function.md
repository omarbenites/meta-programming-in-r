# Anatomy of a function

Everything you do in R involves defining functions or calling functions. You cannot do any action without evaluating some function or other. Even assigning values to variables or subscripting vectors or lists involves evaluating functions. But functions are more than just recipes for how to perform different actions, they are also data objects in themselves, and we have ways of probing and modifying them.

## Manipulating functions

If we define a very simple function like this

```{r}
f <- function(x) x
```

we can examine the components it consists of. There are three parts to a function: its formal parameters, its body, and the environment it is defined in. The functions `formals`, `body`, and `environment` gives us these:

```{r}
formals(f)
body(f)
environment(f)
```

### Formals

The formal parameters are given as a list where element names are the parameter names and values are default parameters.

```{r}
g <- function(x = 1, y = 2, z = 3) x + y + z
parameters <- formals(g)
for (param in names(parameters)) {
  cat(param, "=>", parameters[[param]], "\\n")
}
```

Strictly speaking it is a so-called `pairlist`, but that is an implementation detail that has no bearing on how you treat it. You can treat it as if it is a `list`.

```{r}
g <- function(x = 1, y = 2, z = 3) x + y + z
parameters <- formals(g)
for (param in names(parameters)) {
  cat(param, " => ", '"', parameters[[param]], '"', "\\n", sep = "")
}
```

For variables in this list that do not have default values, the list represents the values as the empty name. This is a special symbol that you cannot assign to, so it cannot be confused with a real value. You cannot use the `missing` function to check for a missing value in a `formals` -- that function is only useful inside a function call, and in any case there is a difference between a missing parameter and one that doesn't have a default value -- but you can always check if the value is the empty symbol.

```{r}
g <- function(x, y, z = 3) x + y + z
parameters <- formals(g)
for (param in names(parameters)) {
  cat(param, " => ", '"', parameters[[param]], '"',
      " (", class(parameters[[param]]), ")\\n", sep = "")
}
```

Primitive functions, those that call into the runtime system, such as `` `+` `` do not have formals. Only functions defined in R.

```{r}
formals(`+`)
```

### Function bodies

The function body is an expression. For `f` it is a very simple expression

```{r}
body(f)
```

but even multi-statement function bodies are expressions. They just evaluate to the result of the last expression in the sequence.

```{r}
g <- function(x) {
  y <- 2*x
  z <- x**2
  x + y + z
}
body(g)
```

When a function is called, R sets up an environment for it to evaluate this expression in; this environment is called the *evaluation environment* for the function call. The evaluation environment is first populated with values for the function's formal parameters, either provided in the function call or given as default parameters, and then the body executes inside this environment. Assignments will modify this local environment unless you use the `` `<<-` `` operator, and the result of the function is the last expression evaluated in the body. This is either the last expression in a sequence or an expression explicitly given to the `return` function.

When we just have the body of a function as an expression we don't get this function call semantics, but we can still try to evaluate the expression.

```{r}
eval(body(f))
```

It fails because we do not have a variable `x` defined anywhere. If we had a global `x` the evaluation would use that and not any function parameter, because the expression here doesn't know it is part of a function. It will be when it is evaluated as part of a call to `f` but not when we use it like this. We can give it a value for `x`, though, like this:

```{r}
eval(body(f), list(x = 2))
```

The `eval` function evaluates an expression and use the second argument to look up parameters. You can give it an environment, and the expression will then be evaluated in it, or you can use a list. We cover how to work with expressions and how to evaluate them in the next chapter; for now all you have to know is that we can evaluate an expression using `eval` if the variables in the expression are either found in the scope where we call `eval` or provided in the second argument to `eval`.

We can also set `x` as a default parameter and use that when we evaluate the expression:

```{r}
f <- function(x = 2) x
formals(f)
eval(body(f), formals(f))
```

Things get a little more complicated if default parameters refer to each other. This has to do with the evaluation environment is set up and not so much with how expressions are evaluated, but consider an example where one default parameter refers to another. 

```{r}
f <- function(x = y, y = 5) {
  x + y
}
```

Both parameters have default values so we can call `f` without any arguments.

```{r}
f()
```

We cannot, however, evaluate it just from the formal arguments without providing values:

```{r}
eval(body(f), formals(f))
```

In `formals(f)`, `x` points to the symbol `y` and `y` points to the numeric 5. But `y` is not used in the expression and if we simply look up `x` we just get the symbol `y`, we don't evaluate it further to figure out what `y` is. Therefore we get an error.

Formal arguments are not evaluated this way when we call a function. They are transformed into so-called promises: unevaluated expressions with an associated scope. This is how the [formal language definition](https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Argument-evaluation) puts it:

> When a function is called, each formal argument is assigned a promise in the local environment of the call with the expression slot containing the actual argument (if it exists) and the environment slot containing the environment of the caller. If no actual argument for a formal argument is given in the call and there is a default expression, it is similarly assigned to the expression slot of the formal argument, but with the environment set to the local environment.

What it means is that in the evaluating environment R first assign all variables to these "promises". The promises are place-holders for values but represented as expressions we haven't evaluated yet. As soon as you access them, though, they *will* be evaluated (and R will remember the value). For default parameters the promises will be evaluated in the evaluating environment and for parameters parsed to the function in the function call the promises will be evaluated in the calling scope.

Since all the promises are unevaluated expressions we don't have to worry about the order in which we assign the variables. As long as the variables exist when we evaluate a promise we are fine, and as long as there are no circular dependencies between the expressions we can figure out all the values when we need them.

Don't make circular dependencies. Don't do something like this:

```{r}
g <- function(x = 2*y, y = x/2) {
  x + y
}
```

We can try to make a similar setup for `f` where we build an environment of its formals as promises. We can use the function `delayedAssign` to assign values to promises like this:

```{r}
fenv <- new.env()
parameters <- formals(f)
for (param in names(parameters)) {
  delayedAssign(param, parameters[[param]], fenv, fenv)
}
eval(body(f), fenv)
```

Here we assign the expression `y` to variable `x` and the value 5 to variable `y`. Basic values like a numeric vector are not handled as unevaluated expressions. They could be, but there is no point. So before we evaluate the body of `f` the environment has `y` pointing to 5 and `x` pointing to the expression `y`, wrapped as a promise that says that the expression should be evaluated in `fend` when we need to know the value of `y`.

### Function environments

The environment of a function is the simplest of its components. It is just the environment where the function was defined. This environment is used to capture the enclosing scope and is what makes closures possible in R. The evaluating environment will be set up with the function's environment when it is created such that variables not found in the local environment, consisting of local variables and formal parameters, will be searched for in the enclosing scope.

## Calling a function

...what happens when you call a function...

```{r}
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
```


## Modifying functions

```{r}
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
```


## Constructing functions
