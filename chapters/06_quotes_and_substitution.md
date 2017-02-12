# Quotes and substitution

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

```
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

