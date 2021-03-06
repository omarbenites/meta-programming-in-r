# Working with substitutions

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

When we use quoting to construct expressions, we get objects we can manipulate via recursive functions, but we can also work with expressions as strings and translate between strings and expressions using the functions `parse` and `deparse`.

The `deparse` function translates an expression into the R source code that would be used to create the expression---represented as a string---and the `parse` function parses a string or a file and returns the expressions in it.

```{r}
deparse(quote(x + y))
parse(text = "x + y")
```

For the call to `parse`, here, we need to specify that we are parsing a text (string). Otherwise, `parse` will assume that we are giving it a file name and try to parse the content of that file. The result of the call to `parse` is not, strictly speaking, and expression. Although the type it writes is `expression`. That is an unfortunate choice for this type because `expression` objects are actually lists of expressions. The `parse` function can parse more than one expression, and sequences of expressions are represented in the `expression` type. You can get the *actual* expressions by indexing into this object.

```{r}
(expr <- parse(text = "x + y; z * x"))
expr[[1]]
expr[[2]]
```

The `departs` function is often used when you need a string that represents an R object---for example for a label in a plot. Many functions extract expressions given as arguments and use those as default labels. There, you cannot just use `deparse`. That would evaluate the expression you are trying to departs before you turn it into a string. You need to get the actual argument, and for that, you need the `substitute` function.

```{r}
f <- function(x) deparse(x)
g <- function(x) deparse(substitute(x))

x <- 1:4; y <- x**2
f(x + y)
g(x + y)
```

## Substitution

The `substitute` function replaces variables for values in an expression. The `deparse(substitute(x))` construction we just saw exploits this by using `substitute` to get the expression that the function parameter `x` refers to before translating it into a string. If we just refer to `x` will well force an evaluation of the argument and get the value it evaluates to; instead, because we use `substitute`, we get the expression that `x` refers to.

Getting the expression used as a function argument, rather than the value of the expression, is a common use of `substitute`. Together with `deparse`, it is used to create labels for plots. It is also used for so-called *non-standard evaluation*---functions that do not evaluate their arguments following the default rules for environments. Non-standard evaluation, which we return to in the next section, obtains the expressions in arguments using `substitute` and then evaluating them, using `eval` in environments different from the function's evaluation environment.

Before we consider evaluating expressions, however, we should get a handle on how `substitute` works. This depends a little bit on where it is called. In the global environment, `substitute` doesn't do anything. At least not unless you give it more arguments than the expression---we get to that shortly. In all other environments, if you just call `substitute` with an expression, the function will search through the expression and find variables. If it finds a variable that has a value in the current environment---whether it is a promise for a function call or a variable we have assigned values to---it will substitute the variable with the value. If the variable does not have a value in the environment, it is left alone. In the global environment, it leaves all variables alone.

In the example below, we see that `substitute(x + y)` doesn't get modified in the global environment, even though the variables `x` and `y` are defined. Inside the function environment for `f`, however, we substitute the two variables with their values.

```{r}
x <- 2; y <- 3
substitute(x + y)
f <- function(x, y) substitute(x + y)
f(2, 3)
```

With `substitute`, variables are not found the same way as they are in `eval`. When `substitute` looks in an environment, it does not follow the parent pointer. If it doesn't find the variable to substitute in the exact environment in which it is called, it will not look further. So, if we write functions like these:

```{r}
y - 3
f <- function(x) substitute(x + y)
f(2)

g <- function(x) function(y) substitute(x + y)
h <- g(2)
h(3)
```

the function `f`, when called, will have `x` in its evaluation environment and `y` in the parent environment---which is the global environment---but `substitute` will only substitute the local variable, `x`. For `h`, it will know `y` as a local variable and `x` from its closure, but only `y`, the local variable, will be substituted.

The actual environment that `substitute` use to find variables is given as its second argument. The default is just the current evaluating environment. We can change that by providing either an environment or a list with a variable to value mapping.

```{r}
e <- new.env(parent = emptyenv())
e$x <- 2
e$y <- 3
substitute(x + y, e)

substitute(x + y, list(x = 2, y = 3))
```

Again, `substitute` will not follow parent pointers, whether these are set implicitly or explicitly in the environment, we pass on to the function.

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

But what if you want to replace, say, `y` with 2 in the expression here. The substitution, both in the global environment with an explicit list or inside a function, will replace `expr` with `quote(x + y)`, but you want to take that then and replace `y` with 2. You cannot just get `y` from the local environment and give it to `substitute` explicitly won't work either.

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

To substitute variables in an expression, you hold in another variable you have to write the expression in the opposite order of what comes naturally. You don't want to substitute `expr` at the inner-most level and then `y` at the outer-most level; you want to first substitute `expr` into a substitute expression that takes care of substituting `y`. The outer-most level substitutes `expr` into `substitute(expr, list(y = 2))` which you can evaluate to get `y` substituted into the expression.

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

Function arguments are passed as unevaluated promises, but the second we access them, they get evaluated. If you want to get hold of the promises without evaluating them, you can use the `substitute` function. This gives you the argument as an unevaluated---or quoted---expression.

This can be useful if you want to manipulate expressions or evaluate them in ways different from the norm---as we explore in the next section---but you do throw away information about which context the expression was supposed to be evaluated in. Consider the example below:

```{r}
f <- function(x) function(y = x) substitute(y)
g <- f(2)
g()
x <- 4
g(x)
```

In the first call to `g`, `y` has its default parameter, which is the one it gets from its closure, so it substitutes to the `x` that has the value 2. In the second call, however, we have the expression `x` from the global environment where `x` is 4. In both cases, however, we just have the expression `quote(x)`. From inside R, there is no mechanism for getting the environment out of a promise, so you cannot write code that modifies input expressions and then evaluates them in the enclosing scope for default parameters and the calling scope for function arguments.^[In the package `pryr`, which we return to at the end of this chapter, there are functions, written in C, that does provide access to the internals of promises. Using `pryr`, you *can* get hold of both the expression and associated environment of a promise. In case you need it.]

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

Non-standard evaluation refers to any evaluation that doesn't follow the rules for how you evaluate expressions in the local evaluation environment. When we use `eval` to evaluate a function argument in an environment other than the default, which is what we get from a call to `environment()`, we are evaluating an expression in a non-standard way.

Typical uses of non-standard evaluation, or NSE, are evaluating expressions in the calling scope, which we have already seen examples of, and evaluating expressions in data frames. We have already seen that we can use a list to provide a variable to value mapping when using `substitute`, but we can also do the same when using `eval`.

```{r}
eval(quote(x + y), list(x = 2, y = 3))
```

Since a data frame is just a list of vector elements of the same length, we can also evaluate expressions in the context of these.

```{r}
d <- data.frame(x = 1:2, y = 3:3)
eval(quote(x + y), d)
```

When we use `eval` this way, where we explicitly quote the expression, we are not really doing NSE. The quoted expression would not be evaluated in any other, standard, way. After all, we explicitly quote it, and if we didn't quote it here, `x+y` would be evaluated in the calling scope, not inside the data frame.

```{r}
x <- 2; y <-  3
eval(x + y, d)
```

To do NSE, we have to explicitly substitute an argument, so we do not evaluate the argument-promise in the calling scope, and then evaluate it in an alternative scope. For example, we can implement our own version of the `with` function like this:

```{r}
my_with <- function(df, expr) {
  eval(substitute(expr), df)
}
d <- data.frame(x = rnorm(5), y = rnorm(5))
my_with(d, x + y)
```

Here, the expression `x + y` is *not* quoted in the function call, so normally we would expect `x + y` to be evaluated in the calling scope. Because we explicitly `substitute` the argument in `my_with` this does not happen. Instead, we evaluate the expression in the context of the data frame. This is non-standard evaluation.

The real `with` function works a little better than our version. If the expression we evaluate contains variables that are not found in the data frame, then it takes these variables from the calling scope. Our version can also handle variables that do not appear in the data frame, but it works slightly differently.

If we use the two functions in the global scope we don't see a difference:

```{r}
z <- 1
with(d, x + y + z)
my_with(d, x + y + z)
```

but if we use them inside functions, we do:

```{r}
f <- function(z) with(d, x + y + z)
f(2)
g <- function(z) my_with(d, x + y + z)
g(2)
```

What is going on here?

Well, `eval` takes a third argument that gives the enclosing scope for the evaluation. In `my_with` we haven't provided this, so we use the default value, which is the enclosing scope where we call `eval`, which is the evaluating environment of `my_with`. We haven't defined `z` in this environment, but the enclosing scope includes the global environment where we have. When we evaluate the expression in `my_with`, we find `z` in the global environment. In contrast, when we use `with`, the enclosing environment is the calling environment.

We can change `my_with` to have the same behaviour thus:

```{r}
my_with <- function(df, expr) {
  eval(substitute(expr), df, parent.frame())
}

f <- function(z) with(d, x + y + z)
f(2)
g <- function(z) my_with(d, x + y + z)
g(2)
```

Now, we have both typical uses of NSE: evaluating in a data frame and evaluating in the calling scope.

### Non-standard evaluation from inside functions

Non-standard evaluation is very hard to get right once you start using it from inside other functions. It is a convenient approach to simplify the syntax for many operations when you work with R interactively or when you write analysis pipelines in the global scope, but because substitutions tend to work verbatim on the function arguments you give functions, once arguments get passed from one function to another, NSE gets tricky.

Consider the example below:

```{r}
x <- 2; y <- 3
f <- function(d, expr) my_with(d, expr)
f(d, x + y)
g <- function(d, expr) my_with(d, substitute(expr))
g(d, x + y)
```

Here, we make two attempts at using `my_with` from inside a function, and neither work as intended. In `f`, the `expr` gets evaluated in the global scope. When we use the variable inside the function, the promise gets evaluated before it is passed along to `my_with`. In `g`, we do substitute, but it is `substitute(expr)` that `my_with` sees---remember, it does not see the expression as a promise but substitutes it to get an expression---so we don't actually get the argument substituted. The NSE in `my_with` prevents this.

If you want functions that do NSE, you really should write functions that work with expressions and do "normal" evaluation on those instead. We can make a version of `my_with` that expects the expression to be already quoted, which we can use in other functions, and then define `my_with` to do the NSE like this:

```{r}
my_with_q <- function(df, expr) {
  eval(expr, df, parent.frame())
}
my_with <- function(df, expr) my_with_q(d, substitute(expr))

g <- function(d, expr) my_with_q(d, substitute(expr))
g(d, x + y)
my_with(d, x + y)
```

### Writing macros with NSE

Since NSE allows you to evaluate expressions in the calling scope, you can use it write macros, that is, functions that work as shortcuts for statements where they are called. These differ from functions, that cannot generally modify data outside of their own scope, and I will not recommend using them unless you have very good reasons to---the immutability of data is a very important feature of R and violating it with macros goes against this---but because it is a good example of what you can do with NSE I will include the example here.

Consider the code below. This example is a modified implementation of the macro code presented in [*Programmer's Niche: Macros in R*(https://www.r-project.org/doc/Rnews/Rnews_2001-3.pdf) by Thomas Lumley, R Journal, 2001]. I have simplified the macro making function a bit; you can see the full version in the original article online.

```{r}
make_param_names <- function(params) {
  param_names <- names(params)
  if (is.null(param_names))
    param_names <- rep("", length(params))
  for (i in seq_along(param_names)) {
    if (param_names[i] == "") {
      param_names[i] <- paste(params[[i]])
    }
  }
  param_names
}

make_macro <- function(..., body) {
  params <- eval(substitute(alist(...)))
  body <- substitute(body)
  
  # Construct macro
  f <- eval(substitute(
    function() eval(substitute(body), parent.frame())
  ))

  # Set macro arguments
  param_names <- make_param_names(params)
  names(params) <- param_names
  params <- as.list(params)
  formals(f) <- params
  
  f
}
```

The first function simply extracts the names from a list of function parameters and make sure that all parameters have a name. This is necessary later when we construct the `formals` list of a function. In a `formals` list, all parameters must have a name, and we construct these names with this function. The second function is the interesting one. Here we construct a macro by constructing a function whose body will be evaluated in its calling scope. We first get hold of the parameters and body the macro should have. The parameters we get into a `list` by substituting `...` in and then evaluating the `alist(...)` expression. Without the substitution we would get the argument list that just contains `...` but with the substitution we get the arguments that are passed to the `make_macro` function, except for the named argument `body` that we just translate into the expression passed to the `make_macro` with another `substitute` call.

The function we construct is where the magic happens. We create the expression 

```r
substitute(function() eval(substitute(body), parent.frame()))
```

where `body` will be replaced with the expression that we pass to the macro. When we evaluate this, we get the function

```r
function() eval(substitute(<body>), parent.frame())
```

where `<body>` is not the symbol `body` but the `body` expression. This is a function that doesn't take any arguments (yet) but will substitute variables in `<body>` that are known in its calling scope---the `parent.frame()`---before evaluating the resulting expression.

We can see this in action with this small example:

```{r}
(m <- make_macro(body = x + y))
```

When we call `m` it will evaluate `x + y` in its calling scope, so we can set the variables `x` and `y` in the global scope and evaluate it thus:

```{r}
x <- 2; y <- 4
m()
```

The last part of the `make_macro` code sets the formal arguments of the macro. It simply takes the parameters we have specified, makes sure they all have a name, and then make them into a list and sets `formals(f)`. After that, `make_macro` returns the constructed function.

We can use this function to create a macro that replaces specific values in a column in a data frame with `NA` like this:

```{r}
set_NA_val <- make_macro(df, var, na_val, 
                         body = df$var[df$var == na_val] <- NA)
```

The macro we construct takes three parameters, the data frame, `df`, the variable (column) in the data frame, `var`, and the value that corresponds to `NA`, `na_val`. Its body then is the expression

```r
df$var[df$var == na_val] <- NA
```

with `df`, `var`, and `na_val` replaced by the arguments passed to the macro. We can use it like this:

```{r}
(d <- data.frame(x = c(1,-9,3,4), y = c(1,2,-9,-9)))
set_NA_val(d, x, -9); d
set_NA_val(d, y, -9); d
```

Here, we see that the constructed `set_NA_val` macro modifies a data frame in the calling scope. It saves some boilerplate code from being written, but at the cost of keeping parameter values immutable. The more traditional function solution where you return updated values is probably much more readable to most R programmers.

```{r}
set_NA_val_fun <- function(df, var, na_val) {
  df[df[,var] == na_val, var] <- NA
  df
}
(d <- data.frame(x = c(1,-9,3,4), y = c(1,2,-9,-9)))
(d <- set_NA_val_fun(d, "x", -9))
(d <- set_NA_val_fun(d, "y", -9))
```

Or even the `magrittr` pipeline version:

```{r}
library(magrittr)
d <- data.frame(x = c(1,-9,3,4), y = c(1,2,-9,-9)) 
d %<>% set_NA_val_fun("x", -9) %>% set_NA_val_fun("y", -9)
d
```


### Modifying environments in evaluations

The reason we can modify variables in macros is that we can modify environments. Actual values are immutable, but when we modify the data frame in the example above, we are replacing the reference in the environment to the modified data. If other variables refer to the same data frame, they will still be referring to the original version. Even with macros, we cannot *actually* modify data, but we can modify environments, and because we can evaluate expressions in environments different from the current evaluating environment, we can make it appear as if we are modifying data in the calling environment.

We can see this by examining the evaluation environment explicitly when we evaluate an assignment. If we explicitly make an environment and evaluate an assignment in it, we see that it gets modified.

```{r}
e <- list2env(list(x = 2, y = 3))
eval(quote(z <- x + y), e)
as.list(e)
```

This environment has the global environment as its parent. Don't try this with the empty environment as its parent. If you do, it won't know the `<-` function. What we see here, is that we modify `e` by setting the variable `z`.

We can also evaluate expressions in lists, so we can attempt the same here:

```{r}
l <- list(x = 2, y = 3)
eval(quote(z <- x + y), l)
l
```

Here we see that the list is *not* modified. It is only environments we can modify in lists, and while we can evaluate an assignment in a list using `eval`, we cannot actually modify the list.

## Accessing promises using the `pryr` package

As I mentioned before, there is no mechanism in pure R to get access to the internals of promises, so if you use `substitute` to translate a function argument into its corresponding expression then you loose information about which environment the expression should be evaluated in. You can, however, use the `pryr` package to examine promises. This package has the function `promise_info` that tells you both what the expression is and the environment it belongs to.

Consider this:

```{r}
library(pryr)
f <- function(x, y) function(z = x + y) promise_info(z)
g <- f(2, 3)
g()
x <- 4; y <- 5
g(x + y)
```

For a promise we get the expression it corresponds to in the `code` field, the environment it belongs to in the `env` field, whether it has been evaluated yet in the `evaled` field, and if it has been evaluated the corresponding value is in the `value` field. In the two different calls to `g`, we see that the `code` is the same, but the environment is different. In the first call, where we use the default values for parameter `z`, the environment is the `f` closure, and in the second, where we call `g` with an expression from the global environment, the promise environment is also the global environment.

We can see the difference between when a promise has been evaluated and before it is in the following example:

```{r}
g <- function(x) {
  cat("=== Before evaluation =====\\n")
  print(promise_info(x))
  force(x)
  cat("=== After evaluation ======\\n")
  promise_info(x)
}
g(x + y)
```

The code doesn't change when we evaluate a promise, but the environment is removed---we do not need to hold a reference to an environment we no longer need, and if we are the only one holding on to this environment we can free it for garbage collection by no longer holding on to it when we don't need it any longer---and the result of evaluating the promise is put in `value` and `evaled` is set to `TRUE`.

We can use the promise info to modify an environment and still evaluate it in the right scope. We just need to get hold of the code, which we can get either by the expression

```r
eval(substitute(
  substitute(code, list(y = quote(2 * y))), 
  list(code = pi$code)))
```

or the expression

```r
eval(substitute(
  substitute(expr, list(y = quote(2 * y)))))
```

where `expr` is the parameter that holds the promise, and `pi` is the result of calling `promise_info(expr)`. Neither is particularly pretty, and you have to remember to construct the expressions inside out, but that is the way you can get an expression substituted in for a variable that holds it and then modify it. Of the two, the traditional approach---the second of the two---is probably the simplest.

In both cases, we are creating the expression

```r
substitute(<expr>, list(y = quote(2 * y)))
```

where `<expr>` refers to the expression in the promise, and we then evaluate this expression, to substitute `y` for `quote(2 * y)`.

Evaluating the expression once we have modified it is almost trivial in comparison. We can just use `eval(expr, pi$env)`.

```{r}
f <- function(x, y) function(expr = x + y) {
  pi <- promise_info(expr)
  expr <- eval(substitute(
    substitute(expr, list(y = quote(2 * y)))))
  value <- eval(expr, pi$env)
  list(expr = expr, value = value)
}
g <- f(2, 2)
g()
x <- y <- 4
g(x + y)
z <- 4
g(z)
```

In the substitution when we create `expr` it is important that we replace `y` with `quote(2 * y)` and not simply `2 * y`. If we did the latter, then `y` would be evaluated in the standard way and would refer to the `y` in the enclosing scope, the parameter given to the call to `f` that creates `g`. Of course, that could be what we wanted: substitute whatever `y` is in the input expression with the `y` we have in the enclosing scope. In that case, the code would simply look like this:

```{r}
f <- function(x, y) function(expr = x + y) {
  pi <- promise_info(expr)
  expr <- eval(substitute(
    substitute(expr, list(y = 2 * y))))
  value <- eval(expr, pi$env)
  list(expr = expr, value = value)
}
g <- f(2, 2)
g()
x <- y <- 4
g(x + y)
z <- 4
g(z)
```

In both cases, we modify the expression in the promise---just in two different ways---and then we evaluate it in the promise scope. In the first case, `y` is taken from the promise scope if it appears in the modified expression; in the second case, `y` is replaced by the value we have in the enclosing scope.
