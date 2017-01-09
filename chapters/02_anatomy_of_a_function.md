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
f <- function(x = y, y = 5) x + y
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
g <- function(x = 2*y, y = x/2) x + y
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

Before we continue, it might be worthwhile to see how these components fit together when a function is called. I explained this in some detail in *Functional Programming in R* but it is essential to know in order to understand how expressions are evaluated. When we start to fiddle around with non-standard evaluation it becomes even more important. So it bears repeating.

When expressions are evaluated, they are evaluated in an environment. Environments are chained in a tree-structure. Each environment has a "parent," and when R needs to look up a variable, it first look in the current environment to see if that environment holds the variable. If it doesn't, R will look in the parent. If it doesn't find it there either, it will look in the grandparent, and it will continue going up the tree until it either finds the variable or hits the global environment and see that it isn't there, at which point it will raise an error. We call the variables an expression can find by searching this way its scope. Since the search always picks the first place it finds a given variable, local variables overshadow global variables, and while several environments on this parent-chain might contain the same variable name, only the inner-most environment, the first we find, will be used.

When a function, `f`, is created, it gets associated `environment(f)`. This environment is the environment where `f` is defined.  When `f` is invoked, R creates an evaluation environment for `f`, let's call it `evalenv`. The parent of `evalenv` is set to `environment(f)`. Since `environment(f)` is the environment where `f` is defined, having it as the parent of the evaluation environment means that the body of `f` can see its enclosing scope if `f` is a closure.

After the evaluation environment is created, the formals of `f` are added to it as promises. As we saw in the quote from the language definition earlier, there is a difference between default parameter and parameters given to the function where it is called in how these promises are set up. Default parameters will be promises that should be evaluated in the evaluation scope, `evalenv`. This means they can refer to other local variables or formal parameters, since these will be put in `evalenv`, and since `evalenv`'s parent is `environment(f)`, these promises can also refer to variables in the scope where `f` was defined. Expressions given to `f` where it is called, however, will be stored as promises that should be called in the calling environment. Let's call that `callenv`. If they were evaluated in the `evalenv` they would not be able to refer to variables in the scope were we call `f`, only local variables or variables in the scope were `f` was defined.

We can see it all in action in the example below:

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
```

We start out in the global environment where we define `enclosing` to be a function. When we call `enclosing` we create an evaluation environment in which we store the variable `z` and then return a function which we store in the global environment as `f`. Since this function was defined in the evaluation environment of `enclosing`, this environment is the `environment` of `f`.

Then we create `calling` and store that in the global environment and call it. This create, once again, an evaluation environment. In this we store the variable `w` and then call `f`. We don't have `f` in the evaluation environment, but because the parent of the evaluation environment is the global environment we can find it. When we call `f` we give it the expression `2 * w` as parameter `x`.

Inside the call to `f` we have another evaluation environment. Its parent is the closer we got from `enclosing`. Here we need to evaluate `f`'s body: `x + y + z`, but before that the evaluation environment needs to be set up. Since `x` and `y` are formal parameters, they will be stored in the evaluation environment as promises. We provided `x` as a parameter when we called `f`, so this promise must be evaluated in the calling environment -- the environment inside `calling` -- while `y` has the default value so it must be evaluated in the evaluation environment. In this environment it can see `x` and `y` and through the parent environment `z`. We evaluate `x`, which is the expression `2 * w` in the calling environment, where `w` is known and `y` in the local environment where `x` is know, so we can get the value of those two variables, and then from the enclosing environment `z`.

We can try to emulate all this using explicit environments and `delayedAssign` to store promises. We need three environments since we don't actually need to simulate the global environment for this. We need the environment where the `f` function was defined, we call it `defend`, then we need the evaluating environment for the call to `f`, and we need the environment in which `f` is called.

```{r}
defenv <- new.env()
evalenv <- new.env(parent = defenv)
callenv <- new.env()
```

Here, `defenv` and `calling` have the global environment as their parent, but we don't worry about that. The evaluating environment has `defend` as its parent.

In the definition environment we save the value of `z`:

```{r}
defenv$z <- 2
```

In the calling environment we save the value of `w`:

```{r}
callenv$w <- 5
```

In the evaluation environment we set up the promises. The `delayedAssign` function takes two environments as arguments. The first is the environment where the promise should be evaluated and the second where it should be stored. For `x` we want the expression to be evaluated in the calling environment and for `y` we want it to be evaluated in the evaluation environment. Both variables should be stored in the evaluation environment.

```{r}
delayedAssign("x", 2 * w, callenv, evalenv)
delayedAssign("y", x, evalenv, evalenv)
```

In the `evalenv` we can now evaluate `f`:

```{r}
f <- function(x, y = x) x + y + z
eval(body(f), evalenv)
```

There is surprisingly much going on behind a function call, but it all follows these rules for how arguments are passed along as promises.


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
