# Manipulating expressions

Expression, the kind we create using the `quote` function, comes in four flavours: a primitive value, a name, a function call or a control structure, or a *pairlist*. Function calls includes operators such as the arithmetic or logical operators as these are function calls as well in R, and control structures can be considered just a special kind of function calls---they only really differ from function calls in the syntax you use to invoke them.

```{r}
class(quote(1))
class(quote("foo"))
class(quote(TRUE))
class(quote(x))
class(quote(f(x)))
class(quote(2+2))
class(quote(if (TRUE) "foo" else "bar"))
class(quote(for (x in 1:3) x))
```

Of these, the calls and control structures are of course the more interesting; values and symbols are pretty simple and we cannot do a lot with them. Pairlists are used for dealing with function parameters, so unless we are working with function arguments we won't see them in expressions. Calls and control structures, on the other hand, capture the action in an expression; we can treat these as lists and we can thus examine them and modify them.^[To the extend that we can modify data in R; we are of course creating new objects with replacement operators.] Working with expressions this way is, I believe, the simplest approach and is the topic of this chapter. Substituting values for variables is another, complementary, way that is the topic of the next chapter.

After we have learned the basics of expressions in the next section, the rest of the chapter will go through some potential real-life examples of how we would use meta-programming. You can find a full version of the examples in the `dfdr` package on [GitHub](https://github.com/mailund/dfdr).

## The basics of expressions

Both function calls and control structures can be manipulated as lists. Of those two, we will mostly focus on calls since those are, in my experience, more likely to be modified in a program. So let us get control structures out of the way first. I will only describe `if` and `for`; the rest are similar.

### Accessing and manipulating control structures

Statements involving control structures are expressions like any other expression in R, and we can create an unevaluated version of them using `quote`. As I explained above, we can then treat this expression object as a list. So we can get the length of the object and we can get access to the elements in the object. For a single `if`-statement we get expressions of length 3 while for `if-else`-statements we get expressions of length 4. The first element is the name `if`. For all control structures and function calls, the first element will always be the name of the function, so if you think of control structures as just functions with a slightly weird syntax, you don't have to consider them a special case at all.^[They basically *are* just special cases of calls. The `is.call` function will return `TRUE` for them, and there is no difference in how you can treat them. The only difference is in the syntax for how you write control-structure expressions compared to function calls.] The second element is the test-condition in the `if`-statement, and after that you get body of the statement. If it is an `if-else`-statement, the fourth element is the `else` part of the expression.

```{r}
x <- quote(if (foo) bar)
length(x)
x[[1]]
x[[2]]
x[[3]]

y <- quote(if (foo) bar else baz)
length(y)
y[[1]]
y[[2]]
y[[3]]
y[[4]]
```

With `for`-loops you get an expression of length 4 where the first element is, of course, the name `for`, the second is the iteration variable, the third the expression we iterate over and the fourth the loop body.

```{r}
z <- quote(for (x in 1:4) print(x))
length(z)
z[[1]]
z[[2]]
z[[3]]
z[[4]]
```

We can evaluate these control structures as any other expression:

```{r}
eval(z)
```

And we can modify them by assigning to their elements to change their behaviour before we evaluate them, e.g., changing what we loop over:

```{r}
z[[3]] <- 1:2
eval(z)
```

Changing what we do in the function body:

```{r}
z[[4]] <- quote(print(x + 2))
eval(z)
```

Or changing the index variable and the body:

```{r}
z[[2]] <- quote(y)
z[[4]] <- quote(print(y))
eval(z)
```

### Accessing and manipulating function calls

For function calls their class is `call` and when we treat them as lists the first element is the name of the function being called and the remaining elements are the function call arguments.

```{r}
x <- quote(f(x,y,z))
class(x)
length(x)
x[[1]]
x[[2]]
x[[3]]
x[[4]]
```

We can test if an expression is a function call using the `is.call` function:

```{r}
is.call(quote(x))
is.call(quote(f(x)))
```

You don't have access to any function body or environment or anything like that here. This is just the name of a function; it is not unless we evaluate the expression we will have to associate the name with an actual function. We can, of course, always evaluate the function call using `eval` and we can modify the expression if we want to change how it should be evaluated:

```{r}
x <- quote(sin(2))
eval(x)
x[[1]] <- quote(cos)
eval(x)
x[[2]] <- 0
eval(x)
```

I didn't quote the zero in the last assignment; I didn't have to since numeric values are already expressions and do not need to be quoted.

To explore an expression we usually need a recursive function. The two basic cases in a recursion are then `is.atomic`---for values---and `is.name`---for symbols---and the recursive cases are `is.call` for function calls and `is.pairlist` if we want to deal with those. In the function below, that just prints the structure of an expression, I do not bother.

```{r}
f <- function(expr, indent = "") {
  if (is.atomic(expr) || is.name(expr)) { # basic case
    print(paste0(indent, expr))
    
  } else if (is.call(expr)) { # a function call / sub-expression
    print(paste0(indent, expr[[1]]))
    n <- length(expr)
    if (n > 1) {
      new_indent <- paste0(indent, "  ")
      for (i in 2:n) {
        f(expr[[i]], new_indent)
      }
    }
    
  } else {
    print(paste0(indent, "Unexpected expression: ", expr[[1]]))
  }
}

f(quote(2 + 3*(x + y)))
```

You might find the output here a little odd, but it captures the structure of the expression `2+3*(x+y)`. The outmost function call is of the function `+` and it has two arguments, the number 2 and the call to `*`. The call to `*` also has two arguments---naturally---where one is 3 and the other is a call to the function `(`. If you find this odd, then welcome to the club, but parentheses are functions in R. The call to `(` only has a single argument which happens to be a function call to `+` with the arguments `x` and `y`.

This is all there is to the direct manipulation of function calls, but of course, the is much that can be done with these simple tools and the following sections will show how you can use them to achieve powerful effects.

## Expression simplification

## Automatic differentiation

