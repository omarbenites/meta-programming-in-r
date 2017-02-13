# Substitution and non-standard evaluation

We can take expressions and manipulate them by treating them as strings, but we can also modify them by substituting variables for expressions, we can build expressions by more advanced quoting, and we can move back and forth between strings and expressions.

## A little more on quotes

We start this chapter by quickly revisiting the quote mechanism. We have already seen how to quote expressions, but you can do more than just create verbatim expressions. We have discussed the `quote` function in some detail, but only seen the `bquote` function in passing. The `bquote` function allows you create expressions where you only partially quote---you can evaluate some values when you create the expression and leave other parts for later. While `quote` wraps an entire expression in quotes, the `bquote`  function does partial substitution in an expression. You call it  with an expression, as you would call `quote`, but any sub-expression you wrap in a call to "dot", that is, you write `.(...)`, will not be quoted but will instead be evaluated and the value will be put into the expression you are constructing. If you just use `bquote` without using `.(...)`, it works just as `quote`.

```{r}
quote(x + y)
bquote(x + y)
bquote(.(2+2) + y)
```

We used `bquote` when we constructed terms for the chain rule in our differentiation program. Here we used `bquote(.(dfdz) * .(dzdx))` to construct the product of the differentiated function and the differentiated argument; we constructed `dfdz` and `dzdx` by constructing a differentiated function, using `bquote` again, for `dfdz`---`bquote(d(.(function_name), .(var)))`---and by calling the `diff_expr` function for `dzdx`.

In many situations where you need to create a call object, using `bquote` can be much simpler than constructing the call object explicitly. For the simple example from above, there is not a huge difference

```{r}
bquote(.(2+2) + y)
call("+", 2+2, quote(y))
```

but once you start creating expressions with many nested calls---and you don't need particularly complex expressions to need multiple nested calls---then explicitly creating calls become much more cumbersome than using `bquote`.

```{r}
bquote((.(2+2) + x) * y)
call("*", call("(", call("+", 2+2, quote(x))), quote(y))
```


## Parsing and deparsing

When we use quoting to construct expressions we get objects we can manipulate via recursive functions, but we can also work with expressions as strings and translate between strings and expressions using the functions `parse` and `deparse`.

The `deparse` function translates an expression into the R source code that would be used to create the expression---represented as a string---and the `parse` function parses a string or a file and return the expressions in it.

```{r}
deparse(quote(x + y))
parse(text = "x + y")
```

For the call to `parse`, here, we need to specify that we are parsing a text (string). Otherwise, `parse` will assume that we are giving it a file name and try to parse the content of that file. The result of the call to `parse` is not, strictly speaking, and expression. Although the type it write is `expression`. That is an unfortunate choice for this type, because `expression` objects are actually lists of expressions. The `parse` function can parse more than one expression, and sequences of expressions are represented in the `expression` type. You can get the *actual* expressions by indexing into this object.

```{r}
(expr <- parse(text = "x + y; z * x"))
expr[[1]]
expr[[2]]
```

The `departs` function is often used when you need a string that represents an R object---for example for a label in a plot. Many functions extract expressions given as arguments and use those as default labels. There, you cannot actually just use `deparse`. That would evaluate the expression you are trying to departs before you turn it into a string. You need to get the actual argument, and for that you need the `substitute` function.

```{r}
f <- function(x) deparse(x)
g <- function(x) deparse(substitute(x))

x <- 1:4; y <- x**2
f(x + y)
g(x + y)
```

## Substitution

The `substitute` function replaces variables for values in an expression. The `deparse(substitute(x))` construction we just saw exploits this by using `substitute` to get the expression that the function parameter `x` refers to before translating it into a string. If we just refer to `x` will well force an evaluation of the argument and get the value it evaluates to; instead, because we use `substitute`, we get the expression that `x` refers to.

Getting the expression used as a function argument, rather than the value of the expression, is a common use of `substitute`. Together with `deparse`, it is used to create labels for plots. It is also use for so-called *non-standard evaluation*---functions that do not evaluate their arguments following the default rules for environments. Non-standard evaluation, which we return to in the next section, obtains the expressions in arguments using `substitute` and then evaluating them, using `eval` in environments different from the function's evaluation environment.

Before we consider evaluating expressions, however, we should get a handle of how `substitute` works. This depend a little bit on where it is called. In the global environment, `substitute` doesn't do anything. At least not unless you give it more arguments than the expression---we get to that shortly. In all other environments, if you just call `substitute` with an expression, the function will search through the expression and find variables. If it finds a variable that has a value in the current environment---whether it is a promise for a function call or a variable we have assigned values to---it will substitute the variable with the value. If the variable does not have a value in the environment it is left alone. In the global environment it leaves all variables alone.

In the example below we see that `substitute(x + y)` doesn't get modified in the global environment, even though the variables `x` and `y` are defined. Inside the function environment for `f`, however, we substitute the two variables with their values.

```{r}
x <- 2; y <- 3
substitute(x + y)
f <- function(x, y) substitute(x + y)
f(2, 3)
```

With `substitute`, variables are not found the same way as they are in `eval`. When `substitute` looks in an environment, it does not follow the parent pointer. If it doesn't find the variable to substitute in the exact environment in which it is called, it will not look further. So, if we write a functions like these:

```{r}
y - 3
f <- function(x) substitute(x + y)
f(2)

g <- function(x) function(y) substitute(x + y)
h <- g(2)
h(3)
```

the function `f`, when called, will have `x` in its evaluation environment and `y` in the parent environment---which is the global environment---but `substitute` will only substitute the local variable, `x`. For `h`, it will know `y` as a local variable and `x` from its closure, but only `y`, the local variable, will be substituted.

The actual environment that `substitute` use to find variables is given as its second argument. The default is just the current evaluating environment. We can change that by providing either an environment or a list with variable to value mappings.

```{r}
e <- new.env(parent = emptyenv())
e$x <- 2
e$y <- 3
substitute(x + y, e)

substitute(x + y, list(x = 2, y = 3))
```

Again, `substitute` will not follow parent pointers, whether these are set implicitly or explicitly in the environment we pass on to the function.

```{r}
x <- 2 ; y <- 3
e <- new.env(parent = globalenv())
substitute(x + y, e)

e <- new.env(parent = globalenv())
e$x <- 2
e$y <- 3
e2 <- new.env(parent = e)
substitute(x + y, e2)
```

If you want a variable substituted, you need to make sure it is in the exact environment you provide to `substitute`.

### Substituting expressions held in variables

A common case when you manipulate expressions is that you have a reference to an expression---for example from a function argument---and you want to modify it. In the global environment, you cannot do this directly with `substitute`. If you give `substitute` a variable, it will just return that variable.

```{r}
expr <- quote(x + y)
substitute(expr)
```

This is because `substitute` doesn't replace the variable in the global environment. You can get the expression substituted by explicitly giving `substitute` the expression in an environment or a list:

```{r}
substitute(expr, list(expr = expr))
```

Usually, though, you don't manipulate expressions in the global environment, and inside a function you *can* substitute an expression:

```{r}
f <- function() {
  expr <- quote(x + y)
  substitute(expr)
}
f()
```

But what if you want to replace, say, `y` with 2 in the expression here. The substitution, both in the global environment with an explicit list or inside a function, will replace `expr` with `quote(x + y)`, but you want to then take that and replace `y` with 2. You cannot just get `y` from the local environment and giving it to `substitute` explicitly won't work either.

```{r}
f <- function() {
  expr <- quote(x + y)
  y <- 2
  substitute(expr)
}
f()

f <- function() {
  expr <- quote(x + y)
  substitute(expr, list(y = 2))
}
f()
```

What you want to do is, first replace `expr` with the expression `quote(x + y)` and then replace `y` with 2. So the natural approach is to write this code, that will not work:

```{r}
substitute(substitute(expr, list(expr = expr)), list(y = 2))
```

The problem here is that `y` doesn't appear anywhere in the expression given to the outermost `substitute`, so it won't be substituted in anywhere. What you get is just the expression

```r
substitute(expr, list(expr = expr))
```

which you can evaluate to get `x + y`

```r
eval(substitute(substitute(expr, list(expr = expr)), list(y = 2)))
```

but the evaluation *first* substitutes `y` into the inner-most `substitute` expression---where there is no `y` variable---and *then* substitutes `expr` into the expression `expr`. The order is wrong.

To substitute variables in an expression you hold in another variable you have to write the expression in the opposite order of what comes naturally. You don't want to substitute `expr` at the inner-most level and then `y` at the outer-most level; you want to first substitute `expr` into an substitute expression that takes care of substituting `y`. The outer-most level substitutes `expr` into `substitute(expr, list(y = 2))` which you can evaluate to get `y` substituted into the expression.

So we create the expression we need to evaluate like this:

```{r}
substitute(substitute(expr, list(y = 2)), list(expr = expr))
```

and we complete the substitute like this:

```{r}
eval(substitute(substitute(expr, list(y = 2)), list(expr = expr)))
```

It might take a little getting used to, but you just have to remember that you need to do the substitutions in this order.

### Substituting function arguments

Function arguments are passed as unevaluated promises, but the second we access them they get evaluated. If you want to get hold of the promises without evaluating them, you can use the `substitute` function. This gives you the argument as an unevaluated, or quoted, expression.

This can be useful if you want to manipulate expressions or evaluate them in ways different from the norm---as we explore in the next section---but you do throw away information about which context the expression was supposed to be evaluated in. Consider the example below:

```{r}
f <- function(x) function(y = x) substitute(y)
g <- f(2)
g()
x <- 4
g(x)
```

In the first call to `g`, `y` has its default parameter, which is the one it gets from its closure, so it substitutes to the `x` that has the value 2. In the second call, however, we have the expression `x` from the global environment where `x` is 4. In both cases, however, we just have the expression `quote(x)`. From inside R, there is no mechanism for getting the environment out of a promise, so you cannot write code that modifies input expressions and then evaluate them in the enclosing scope for default parameters and the calling scope for function arguments.

You also have to be a little careful when you use `substitute` in functions that are called with other functions. The expression you get when you `substitute` is the exact expression a function gets called with. This expression doesn't propagate through other functions. In the example below, we call the function `g` with the expression `x + y`, but since `g` calls `f` with `expr`, that is what we get in the substitution.

```{r}
f <- function(expr) substitute(expr)
f(x + y)
g <- function(expr) f(expr)
g(x + y)

x <- 2; y <- 3
eval(f(x + y))
eval(g(x + y))
```

The `substitute` function is harder to use safely and correctly than is using `bquote` and explicitly modifying `call` objects, but it is the function you need to use to implement non-standard evaluation.

## Non-standard evaluation

Non-standard evaluation refers to any evaluation that doesn't follow the rules for how you evaluate expressions in the local evaluation environment.