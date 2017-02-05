# Manipulating expressions

Expressions, the kind we create using the `quote` function, come in four flavours: a primitive value, a name, a function call or a control structure, or a *pairlist*. Function calls includes operators such as the arithmetic or logical operators as these are function calls as well in R, and control structures can be considered just a special kind of function calls---they only really differ from function calls in the syntax you use to invoke them.

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

To see manipulation of expressions in action we consider a scenario where we want to simplify expression. We want to evaluate sub-expressions that we can immediately evaluate, because they only consist of atomic values where we do not depend on variables, and we want to reduce multiplication by one or addition by zero. Something like this:

```r
simplify_expr(quote(2*(0 + ((4 + 5)*x)*1)))
## 2 * (9 * x)
```

It isn't quite perfect; if we really reduced the expression we would see that we could rearrange the parentheses and multiply 2 by 9, but we are going to simplify expressions locally and not attempt to rewrite them.

Since we are dealing with expressions, we need a recursive function that handles the basic cases---atomic values and names---and the recursive cases---calls and pair lists. I don't expect to see a pair list in an expression, so I simply give up if I see anything except atomic, name, or call objects. If I see any basic case I just return that; we can't simplify those further. For call objects, I call a function, `simplify_call` responsible for handling calls.

```{r}
simplify_expr <- function(expr) {
  if (is.atomic(expr) || is.name(expr)) {
    expr

  } else if (is.call(expr)) {
    simplify_call(expr)

  } else {
    stop(paste0("Unexpected expression ", 
                deparse(expr), 
                " in simplifying"))
  }
}
```

For call simplification, I don't actually attempt to simplify function calls. I don't know what any generic function is doing, so there is little I can do to simplify expressions that involve functions. I will assume, though, that if I am simplifying an expression, then functions in it behave as if they had call-by-value semantics and simplify their arguments. This is an assumption, it might be wrong, but for this exercises I will assume it. So for general function calls I will just simplify their arguments. For arithmetic expressions, I will try to simplify those further. I could also attempt to do that for other operations, but handling just the arithmetic operators show how we would handle operators in sufficient detail that I trust you, dear reader, to be able to handle other operators if you need to.

Call handling can then look like this:

```{r}
simplify_call <- function(expr) {
  if (expr[[1]] == as.name("+"))
    return(simplify_addition(expr[[2]], expr[[3]]))
  if (expr[[1]] == as.name("-")) {
    if (length(expr) == 2)
      return(simplify_unary_subtraction(expr[[2]]))
    else
      return(simplify_subtraction(expr[[2]], expr[[3]]))
  }

  if (expr[[1]] == as.name("*"))
    return(simplify_multiplication(expr[[2]], expr[[3]]))
  if (expr[[1]] == as.name("/"))
    return(simplify_division(expr[[2]], expr[[3]]))

  if (expr[[1]] == as.name("^"))
    return(simplify_exponentiation(expr[[2]], expr[[3]]))

  if (expr[[1]] == as.name("(")) {
    subexpr <- simplify_expr(expr[[2]])
    if (is.atomic(subexpr) || is.name(subexpr))
      return(subexpr)
    else if (is.call(subexpr) && subexpr[[1]] == as.name("("))
      return(subexpr)
    else
      return(call("(", subexpr))
  }

  simplify_function_call(expr)
}
```

It is mostly self-explanatory, but a few comments are in order: First, we need to compare the call names with name objects. They are not actually character strings but have type `name`; thus we need to use `as.name`. Second, minus come in two flavours, binary subtraction and a unary negation. We can tell the two apart by checking if the call has one or two arguments (i.e., whether it has length two or three; remember that the first element is the call name), and we just use two different functions to handle the two cases. Third, parentheses are also calls, so we need to handle them. We just get hold of the expression inside the parentheses. If this is something that doesn't need parentheses---single values, names, or an expression already surrounded by parentheses---we just return that sub-expression. Otherwise, we put parentheses around it. Finally, if we don't know what else to do, we just treat the expression as a function call.

Now we just handle each operator in turn. They are all handled very similarly, only differing in what we can simplify given each operator. For addition, we can get rid of addition by zero, and if both our arguments are numbers we can evaluate them right away, otherwise we need to return a call to `+` with simplified operands:

```{r}
simplify_addition <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (left == 0) return(right)
  if (right == 0) return(left)
  if (is.numeric(left) && is.numeric(right))
    return(left + right)
  call("+", left, right)
}
```

Unary minus we can just evaluate if its argument is numeric, otherwise we can get rid of an existing minus in the argument, since two minuses make a plus, and if all else fails we just have to return the simplified expression with a minus in front of it:

```{r}
simplify_unary_subtraction <- function(f) {
   simplified <- simplify_expr(f)
   if (is.numeric(simplified))
     -simplified
   else if (is.call(simplified) && simplified[[1]] == "-")
     simplified[[2]]
   else
     bquote(-.(simplified))
}
```

For the final case, here, we use the function `bquote`. It works similar to `quote` but substitutes a value in where we put `.(...)`. So we essentially write `quote(-simplified)` except that we actually put the simplified expression inside the expression. 

Binary subtraction is similar to addition but with a little more work when we subtract from zero. Here we need to use `bquote` again:

```{r}
simplify_subtraction <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (left == 0) {
    if (is.numeric(right))
      return(-right)
    else
      return(bquote(-.(right)))
  }
  if (right == 0)
    return(left)
  if (is.numeric(left) && is.numeric(right))
    return(left - right)
  call("-", left, right)
}
```

For multiplication we can simplify cases where the multiplication involves zero or one, but otherwise the function looks very similar to what we have seen before:

```{r}
simplify_multiplication <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (left == 0 || right == 0)
    return(0)
  if (left == 1)
    return(right)
  if (right == 1)
    return(left)
  if (is.numeric(left) && is.numeric(right))
    return(left * right)
  call("*", left, right)
}
```

Division and exportation is just more of the same, with different cases to handle:

```{r}
simplify_division <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (right == 1)
    return(left)
  if (is.numeric(left) && is.numeric(right))
    return(left / right)
  call("/", left, right)
}

simplify_exponentiation <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (right == 0) return(1)
  if (left == 0) return(0)
  if (left == 1) return(1)
  if (right == 1) return(left)
  if (is.numeric(left) && is.numeric(right))
    return(left ^ right)
  call("^", left, right)
}
````

The final function we need is function-call simplification. Here we just have to simplify all the function's arguments before returning a call. We can collect the arguments in a list and create a function call with an expression like:

```r
  do.call("call", c(list(function_name), arguments))
```

This would take the arguments, as a list, and make them into arguments in a call to `call`. This will work fine if the `function_name` is actually a function name, but expressions such as `f(x,y)(z)` are also function calls; here the function "name" is `f(x,y)` and the argument is `z`. We cannot wrap such an expression up in a call to `call`, but we can just take a list and make it into a call using `as.call`:

```r
simplify_function_call <- function(expr) {
  function_name <- expr[[1]]
  arguments <- vector("list", length(expr) - 1)
  for (i in seq_along(arguments)) {
    arguments[i] <- list(simplify_expr(expr[[i + 1]]))
  }
  as.call(c(list(function_name), arguments))
}
```

We could also get a little more ambitions and try to evaluate functions when all their arguments are values and when we know what the functions are---or at least have a reasonable expectation that we would know. We could always check if we can find the name in a relevant environment, and if it is a function, but since we are simplify expressions where we don't expect to know variables that are not functions, it is probably too much to demand that all function symbols are known. Still, we could say that functions such as `sin` and `cos`, `exp` and `log` are their usual selves and then do something like this:

```{r}
simplify_function_call <- function(expr) {
  function_name <- expr[[1]]
  arguments <- vector("list", length(expr) - 1)
  for (i in seq_along(arguments)) {
    arguments[i] <- list(simplify_expr(expr[[i + 1]]))
  }

  if (all(unlist(Map(is.numeric, arguments)))) {
    if (as.character(function_name) %in% c("sin", "cos", "exp", "log")) {
      result <- do.call(as.character(function_name), arguments)
      names(result) <- names(expr)
      return(result)
    }
  }
  as.call(c(list(function_name), arguments))
}
```

We now have a simple program that lets us simplify expressions to a certain extend:

```{r}
simplify_expr(quote(2*(0 + ((4 + 5)*x)*1)))
```

Neither function-call solution can handle named arguments. We simply work with positional arguments. We simply throw away the name information.

```{r}
f <- function(x, y) x
expr1 <- quote(f(x = 2, y = 1))
expr2 <- quote(f(y = 2, x = 1))
eval(expr1)
eval(expr2)

simplify_expr(expr1)
simplify_expr(expr2)
eval(simplify_expr(expr1))
eval(simplify_expr(expr2))
```

It isn't hard to remedy this, though. There is nothing special needed to work with named arguments when we deal with function calls; they are just accessed with the `named` function:

```{r}
names(expr1)
names(expr2)
```

If we make sure that the result of our simplification gets the same name as the original expression, we will be fine:

```{r}
simplify_function_call <- function(expr) {
  function_name <- expr[[1]]
  arguments <- vector("list", length(expr) - 1)
  for (i in seq_along(arguments)) {
    arguments[i] <- list(simplify_expr(expr[[i + 1]]))
  }
  result <- as.call(c(list(function_name), arguments))
  names(result) <- names(expr)
  result
}
```

```{r}
simplify_expr(expr1)
simplify_expr(expr2)
eval(simplify_expr(expr1))
eval(simplify_expr(expr2))
```




## Automatic differentiation

As a second, only slightly more involved, example, we consider *automatic differentiation:* automatically translating a function that computes an expression into a function that computes the derived expression. We will assume that we have a function whose body contains only a single expression---one that doesn't involve control structures or sequences of statements but just a single arithmetic expression---and recurse through this expression, applying the rules of differentiation. Although what we do with this meta-program is more complex than the expression simplification we just implemented, you will see that the form of the program is very similar.

We start with the main function, which we name `d` for differentiation. It takes two arguments: the function to be differentiated and the variable to take the derivative with respect to. If we want the function to be able to handle the built-in mathematical functions we need to handle these as special cases. These are implemented as so-called *primitive* functions and do not have a body. We need to handle them explicitly in the `d` function. For all other functions, we just need to compute the derivative of the expression in the function body. If we want to return a new function for the derivative we can just take the function we are modifying and replace its body. Since R doesn't actually let us modify arguments to a function, this will just create a copy we can return and leave the original function intact. Reusing the argument this way make sure that the new function has the same arguments, with the same names and same default values, as the original.

This function can look like this, where I've only handled three of the primitive functions---you can add the remaining as an exercises:

```r
d <- function(f, x) {

  if (is.null(body(f))) {
    if (identical(f, sin)) return(cos)
    if (identical(f, cos)) return(function(x) -sin(x))
    if (identical(f, exp)) return(exp)

    stop("unknown primitive")

  } else {
    # for other functions we have to parse the body
    # and differentiate it.
    df <- f
    body(df) <- simplify_expr(diff_expr(body(f), x))
    df
  }
}
```

For aesthetic reasons, we simplify the expression we get from differentiating the body of `f`, using the code we wrote in the previous section. We can use `d` like this:

```r
f <- function(x) x^2 + sin(x)
df <- d(f, "x")
df
## function (x) 
## 2 * x + cos(x)
```

For computing the derivative of the function body, we follow the pattern we used for the expression simplification: we write a recursive function for dealing with expressions, where we dispatch function calls to different cases for the different arithmetic operations.

The two basic cases for the recursive function are numbers and names---we assume that we do not get other atomic values such as logical vectors; we wouldn't know how to differentiate them anyway. For numbers, the derivative is always zero while for names it depends on whether we have the variable we are computing the derivative with respect to or another variable. The recursive case for the function is function calls, where we just call another function to handle that case.

```r
diff_expr <- function(expr, x) {
  if (is.numeric(expr)) {
    quote(0)

  } else if (is.name(expr)) {
    if (expr == x) quote(1)
    else quote(0)

  } else if (is.call(expr)) {
    diff_call(expr, x)

  } else {
    stop(paste0("Unexpected expression ", 
                deparse(expr), " in parsing."))
  }
}
```

For calls, we dispatch based on the type of call, so we deal with arithmetic expressions through a function for each operator, we deal with parentheses similar to how we handled them in the expression simplification, and we have a function for differentiating all other function calls.

```r
diff_call <- function(expr, x) {
  if (expr[[1]] == as.name("+"))
    return(diff_addition(expr[[2]], expr[[3]], x))

  if (expr[[1]] == as.name("-")) {
    if (length(expr) == 2)
      return(call("-", diff_expr(expr[[2]], x)))
    else
      return(diff_subtraction(expr[[2]], expr[[3]], x))
  }

  if (expr[[1]] == as.name("*"))
    return(diff_multiplication(expr[[2]], expr[[3]], x))
  if (expr[[1]] == as.name("/"))
    return(diff_division(expr[[2]], expr[[3]], x))

  if (expr[[1]] == as.name("^"))
    return(diff_exponentiation(expr[[2]], expr[[3]], x))

  if (expr[[1]] == as.name("(")) {
    subexpr <- diff_expr(expr[[2]], x)
    if (is.atomic(subexpr) || is.name(subexpr))
      return(subexpr)
    else if (is.call(subexpr) && subexpr[[1]] == as.name("("))
      return(subexpr)
    else
      return(call("(", subexpr))
  }

  return(diff_general_function_call(expr, x))
}
```

We handle the arithmetic operations just by following the rules we learned in calculus class:

```r
diff_addition <- function(f, g, x) {
  call("+", diff_expr(f, x), diff_expr(g, x))
}

diff_subtraction <- function(f, g, x) {
  call("-", diff_expr(f, x), diff_expr(g, x))
}

diff_multiplication <- function(f, g, x) {
  # f' g + f g'
  call("+",
       call("*", diff_expr(f, x), g),
       call("*", f, diff_expr(g, x)))
}

diff_division <- function(f, g, x) {
  # (f' g − f g' )/g**2
  call("/",
       call("-",
        call("*", diff_expr(f, x), g),
        call("*", f, diff_expr(g, x))),
       call("^", g, 2))
}

diff_exponentiation <- function(f, g, x) {
  # Using the chain rule to handle this generally.
  # if y = f**g then dy/dx = dy/df df/dx = g * f**(g-1) * df/dx
  dydf <- call("*", g, call("^", f, substitute(n - 1, list(n = g))))
  dfdx <- diff_expr(f, x)
  call("*", dydf, dfdx)
}
```



We handle function calls using the chain rule. FIXME