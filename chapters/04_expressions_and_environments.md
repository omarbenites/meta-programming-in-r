# Expressions and environments

In this chapter, we dig deeper into how environments work and how we can evaluate expressions in different environments. Understanding how environments are chained together helps you understand how the language finds variables and being able to create, manipulate, and chain together environments when evaluating expressions is a key trick for meta-programming.

## Expressions

You can consider everything that is evaluated in R to be an expression. Every statement you have in your programs is also an expression that evaluates to some value (which, of course, might be `NULL`). This includes control structures and function bodies. You can consider it all expressions; just some expressions involves evaluating several contained expressions for their side-effect before returning the result of the last expression they evaluate. From a meta-programming perspective, though, we are most interested in expressions we can get our hands on and examine, modify, or evaluate within a program.

Believe it or not, we have already seen most of the ways to get expression objects. We can get function bodies using the `body` function, or we can construct expressions using `quote` or `call`. Using any of these methods, we get an object we can manipulate and evaluate from within a program. Usually, expressions are just automatically evaluated when R gets to them during a program's execution, but if we have an expression as the kind of object we can manipulate, we have to evaluate it explicitly. If we don't evaluate it, its value is the actual expression; if we evaluate it, we get the value it corresponds to in a given scope.

In this chapter, we will not concern ourselves with manipulation of expressions. That is the topic for the next chapter. Instead, we will focus on how expressions are evaluated and how we can chance the scope I which we evaluate an expression. If we just write an expression as R source code, it will be evaluated in the scope where it is written. This is what you are used to. To evaluate it in a different scope, you need to use the `eval` function. If we don't give `eval` an environment, it will just evaluate an expression in the scope where `eval` is called, similar to if we had just written the expression there. If we give it an environment, however, that environment determines the scope in which the expression is evaluated.

To understand how we can exploit scopes, though, we first need to understand how environments define scopes in detail.

## Chains of linked environments

We have seen how functions have associated environments that capture where they were defined, and that when we evaluate a function call, we have an evaluation environment that is linked to this definition environment. We have also see how this works through a linked list of parent environments. So far, though, we have claimed that this chain ends in the global environment, where we look for variables if we don't find them in any nested scope. For all the examples we have seen so far, this might as well be true, but in any real use of R, you have packages loaded. The reason that you can find functions from packages is that these packages are also found in the chain of environments. The global environment also has a parent, and when you load packages, the last package you loaded will be the parent of the global environment and the previous package will be the parent of the new package. This explains both how you can find variables in loaded packages and why loading new packages can overshadow variables defined in other packages.

It has to stop at some point, of course, and there is a special environment that terminates the sequence. This is known as the empty environment, and it is the only environment that doesn't have a parent. When you start up R, the empty environment is there, then R puts the base environment, where all the base language functionality lives, then it puts an `Autoload` environment, responsible for loading data on demand, and on top of that, it puts the global environment. The base environment and the empty environment are sufficiently important that we have functions to get hold of them. These are `baseenv` and `emptying`, respectively.

When you import packages, or generally `attach` a namespace, it gets put of the this list just below the global environment. The function `search` will give you the sequence of environments from the global environment and down. We can see how loading a library affects this list in this example:

```{r}
search()
library(MASS)
search()
```

The `search` function is an internal function, but we can write our own version to get a feeling for how it could work. While `search` search from the global environment, though, we will make our function more general and give it an environment to start from. We simply need it to print out environment names and then move from the current environment to the parent until we hit the empty environment. 

To get the name of an environment, we can use the function `environmentName`. Not all environments have names---those we create when we nest or call functions, or those we create with `new.env` have not---but environments created when we are loading packages do.^[If you want to name your environments, you can set the attribute "name". It is generally not something you need, though.] If an environment doesn't have a name, though, `environmentName`, will give us empty strings when environments do not have names. If so, we will instead just use `str` to get a representation of environments we can `cat` and make sure we only get a simple representation by not getting any potential attributes printed as well.

To check if we have reached the end of the environment chain, we check `identical(env, emptyenv())`. You cannot compare two environments with `==`, but you can use `identical`. Our function could look like this:

```{r}
my_search <- function(env) {
  repeat {
    name <- environmentName(env)
    if (nchar(name) != 0) 
      name <- paste0(name, "\\n")
    else
      name <- str(env, give.attr = FALSE)
    cat(name)
    env <- parent.env(env)
    if (identical(env, emptyenv())) break
  }
}
```

Calling it with the global environment as the argument should give us a result similar to `search`, except we are printing the environments instead of returning a list of names.

```{r}
my_search(globalenv())
```

Since we can give it any environment, we can try to get hold of the environment of a function. If we write a function nested into other function scopes, we can see that we get (nameless) environments for the functions.

```{r}
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
```

We can also get hold of the environment of imported functions. For example, we can get the chain of environments starting at the `ls` function like this:

```{r}
my_search(environment(ls))
```

Here we see something that is a little weird. It looks like `"base"` is found both at the top and at the bottom of the list. Clearly, this can't be the same environment; it would have to have, as its parent, both the global environment and the parent, and environments only have one parent. The only reason it looks like it is the same environment is that `environmentName` gives us the same name for two different environments. They are different.

```{r}
environment(ls)
baseenv()
```

The environment of `ls` is a namespace defined by the `base` package. The `baseenv()` environment is how this package is exported into the environment below the global environment, making base functions available to you, outside of the `base` package. 

Having such extra environments is how packages manage to have private functions within a package and have other functions that are exported to users of a package. The base package is special in only defining two environments, the namespace for the package and the package environment. All other packages have three packages set up in their environment chain before the global environment: the package namespace, a namespace containing imported symbols, and then the base environment, which, as we just saw, connects to the global environment. A graph of environments when three packages are loaded, `MASS`, `stats` and `graphics`, is shown in +@fig:environment-graph (here `graphics` was loaded first, then `stats` and then `MASS`, so `MASS` appears first, followed by `stats` and then `graphics` on the path from the global environment to the base environment). The solid arrows indicate parent pointers for the environments and the dashed arrows indicate from which package symbols are exported into package environments.

![Environment graph with three loaded packages: `MASS`, `stats`, and `graphics`.](figures/environment-graph){#fig:environment-graph}

If you try to access a symbol from a package, starting in the global environment, then you only get access to the exported functions, and some of these might be overshadowed by other packages you have loaded. For functions defined inside the package, their parent environment contains all the functions (and other variables) defined in the package, and because this package namespace environment sits before the global environment in the chain, variables from other packages do not overshadow variables inside the package when we execute functions from the package.

If a package imports other packages, these goes into the import environment, below the package namespace and before the base environment. So functions inside a package can see imported symbols, but only if they aren't overshadowed inside the package. If a user of the package imports other packages, these cannot overshadow any symbols the package functions can see, since such imports come later in the environment chain, as seen from inside the package. After the imports environment comes the base namespace. This gives all packages access to the basic R functionality, even if some of it should be overshadowed as seen from the global environment.[^depends-vs-imports] 

[^depends-vs-imports]: 

 Strictly speaking, there is a lot more to importing other packages than what I just explained here. Since this book is not about R packages, I will only give a very short explanation in this footnote.

 There are three ways of specifying that a package depends on anther in the `DESCRIPTION` file: Using `Suggests:`, `Depends:`, and `Imports:`. The first doesn't actually set up any dependencies; it is just a suggestion of other packages that might enhance the functionality of the one being defined. 
 
 The packages specified in the `Depends:` directive will be loaded into the `search` path when you load the package. For packages specified here, you will clutter up the global namespace---not the global environment, but the search path below it---and you risk that functions you depend on will be overshadowed by packages that are loaded into the global namespace later. You should avoid using `Depends:` when you can, for these reasons.
 
 Using `Imports:` you just require that a set of other packages are installed before your own package can be installed. Those packages, however, are not put on the search path, nor are they imported in the `imports` environment. Using `Imports:` just enable you to access functions and data in another package using the package namespace prefix, so if you `Imports:` the `stats` package you know you can access `stats::sd` because that function is guaranteed to exist on the installation when your package is used.
 
 Actually importing variables into the `imports` namespace, you need to modify the `NAMESPACE` file, using the directives `imports()`, `importFrom()`, `importClassesFrom()`, or `importMethodsFrom()`. The easiest way to handle the `NAMESPACE` file, though, is using `Roxygen`, and here you can import names using `@importFrom <package> <name>` for a single function, `@import <package>` for the entire package, and `@importClassesFrom <package> <classes>` and `@importMethodsFrom <package> <methods>` for S4 classes.
 
 To ensure that packages you write play well with other namespaces you should use `Imports:` for dependencies you absolutely need (and `Suggests:` for other dependencies) and either use the package prefixes for dependencies in other packages or import the dependencies in the `NAMESPACE`.

The function `sd` sits in the package `stats`. Its parent is `namespace:stats` and its grandparent is `imports:stats`, and its great-grandparent is `namespace:base`. If we access `sd` from the global environment, though, we find it in `package:stats`. 

```{r}
my_search(environment(sd))
environment(sd)
parent.env(environment(sd))
parent.env(parent.env(environment(sd)))
parent.env(parent.env(parent.env(environment(sd))))
``` 

Figure @fig:environment-graph-functions-in-package shows how both `ls` and `sd` sits in package and namespace environments and how their parents are the namespace rather than the package environment.

![Environment graph showing the positions of `stats::sd` and `base::ls`.](figures/environment-graph-functions-in-package){#fig:environment-graph-functions-in-package}

As we now see, this simple way of chaining environments give us not only lexical scope in functions; it also explains how namespaces in packages work.

## Environments and function calls

How environments and package namespaces work together is interesting to understand, and might give you some inspiration for how namespaces can be implemented for other uses, but in day-to-day programming, we are more interested in how environments and functions work together. So from now on, we are going to pretend that the scope graph ends at the global environment and focus on how environments are chained together when we define and call functions.

We already know the rules for this: when we call a function we get an evaluation environment, its parent points to the environment in which the function was defined, and if we want the environment of the caller of the function we can get it using the `parent.frame` function. Just to test our knowledge, though, we should consider a more complex example of nested functions than we have done so far. Consider the code below at the point where we evaluate the `i(3)` call. Figure @fig:environment-graph-function-calls shows how environments are chained together; here solid lines indicate parent pointers and dashed lines are pointers from variables to their values. Figure @fig:environment-graph-function-calls-parent-frame adds the call stack as parent frame environments. Follow along on the figures while we go through the code.

```r
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
```

![Environment graph for a complex example of nested functions.](figures/environment-graph-function-calls){#fig:environment-graph-function-calls}

![Environment graph for a complex example of nested functions highlighting the call stack as we can get it with `parent.frame`.](figures/environment-graph-function-calls-parent-frame){#fig:environment-graph-function-calls-parent-frame}

Ok, the first two statements in the code defines functions `f` and `h`. We don't call them, we just define them, so we are not creating any new environments. The environment associated with both functions is the global environment. Then we set `x` to two. Nothing interesting happens here either. When we call `h`, however, we start creating new evaluation environments. We first create one to evaluate `h` inside. This environment sets `a` to 1 since we called `h` with 1. It then calls `f` with `x`.

Already here, it gets a little tricky. Since we call `f` with a variable, which will be lazy-evaluated inside `f`, we are not calling `f` with the value of the global parameter `x`. So `f` gets a promise it can later use to get a value for `x`, and this promise knows that `x` should be found in the scope of the `h` call we are currently evaluating. That `x` happens to be the global variable in this example, but you could assign to a local variable `x` inside `h` after you called `f` and then `f` would be using this local `x` instead. When we create a promise in a function call, the promise knows it should be evaluated in the calling scope, but it doesn't find any variables just yet; that only happens when the promise is evaluated.

Inside the call to `f`, we define the function `g` and then return it. Since `g` is defined inside the `f` call, its environment is set to the evaluation scope of the call. This will later let `g` know how to get a value for `x`. It doesn't store `x` itself, but it can find it in the scope of the `f` call, where it will find it to be a promise that should be evaluated in the scope of the `h` call, where it will be found to be the global variable `x`. It already looks very complicated, but whenever you need a value, you should just follow the parent environment chain to see where you will eventually find it.

The `h(1)` call then defines a function, `i`, returns it, and we save that in the global variable `i`. The environment of this function is the evaluation scope of the `h(1)` call, so this is where the function will be looking for the names `a` and `g`.

Now, finally, we call `i(3)`. This first creates an evaluation environment where we set the variable `b` to `3` since that is what the argument for `b` is. Then we find the `g` function, which is the closer we created earlier, and we call `g` with parameters `a+b` and 5. The 5 is just passed along as a number, we don't translate constants into promises, but the `a+b` will be lazily evaluated, so it is a promise in the call to `g`. Calling `g`, we create a new evaluation environment for it. Inside this environment, we store the variables `y` and `z`. The former is set to the promise `a+b` and the latter to `5`. Since promises should be evaluated where they are defined, here in the calling scope, this is stored together with the promise.

We evaluate `g(a + b, 5)` as such: We first need to figure out what `x` is, so we look for it in the local evaluation environment, where we don't find it. Then we look in the parent environment where we do find it, but see that it is a promise from the `h(1)` environment, so we have to look there for it now. It isn't there either, so we continue down the parent chain and find it in the global environment where it is 2. Then we need to figure out what `y` is. We can find `y` in the local environment where we see that it is a promise, `a+b`, that should be evaluated in the `i(3)` environment. Here we need to find `a` and `b`. We can find `b` directly in the environment, so that is easy, but `a` we need to search for in the parent environment. Here we find it so, we can evaluate `a+b` as `1+3`. This value now replaces the promise in `y`. Finally, we need to find `z`, but at least this is easy. That is just the number 5 stored in the local environment. We now have all the values we need to compute `x + y + z`, they are `2 + (1+3) + 5` so when we return from the `i(3)` call, we get the return value 11.

The environment graphs can get rather complicated, but the rules for finding values are quite simple. You just follow environment chains. The only pitfall that tends to confuse programmers is the lazy evaluation. Here, the rules are also simple; they are just not as familiar. Promises are evaluated in the scope where they are defined. So a default parameter will be evaluated in the environment where the function is defined, and actual parameters will be evaluated in the calling scope. They will always be evaluated when you access a parameter, so if you don't want side-effects of modifying closure environments by changing variables in other scopes, you should use `force` before you create closures.

Take some time to work through this example. Once you understand it, you understand how environments work. It doesn't get more complicated than this. Well, unless we start messing with the environments as we are wont to do...

## Manipulating environments

So how can we make working with environments even more complicated? We can, of course, start modifying them and chaining them up in all kinds of new ways. You see, we can not only access environments and put variables in them, but we can also modify the chain of parent environments. We can, for example, change the environment we execute a function in by changing the parent of the evaluation environment like this:

```{r}
f <- function() {
  my_env <- environment()
  parent.env(my_env) <- parent.frame()
  x
}
g <- function(x) f()
g(2)
```

We are not changing the local environment---that would be hard to do, you don't have anywhere to put values if you don't have that---but we are making its parent point to the function call rather than the environment where the function was defined. If we didn't mess with the parent environment, `x` would be searched for in the global environment, but because we set the parent environment to the parent frame, we will instead start the search in the caller where we find `x`.

The power to modify scopes in this way can be used for good but certainly also for evil. There is nothing complicated in how it works; if you understood how environment graphs work from the previous section, you will also understand how they work if we start changing parent pointers. The main problem is just that environments are mutable, so if you start modifying them one place, it has consequences elsewhere.

Consider this example of a closure:

```{r}
f <- function() {
  my_env <- environment()
  call_env <- parent.frame()
  parent.env(my_env) <- call_env
  y
}
g <- function(x) {
  closure <- function(y) {
    z <- f()
    z + x
  }
  closure
}
add1 <- g(1)
add2 <- g(2)
```

It is just a complicated way of writing a closure for adding numbers, but we are not going for elegance here---we aim to see how modifying environments can affect us. Here we have a function `f` that sets up its calling environment as its scope and then returns `y`. Since `y` is not a local variable it must be found in the enclosing scope with a search that starts in the parent environment; this is the environment we just changed to the calling scope. In the function `g` we then define a closure that takes one argument, `y`, and then calls `f` and adds `x` to the result of the call. Since `y` is a parameter of the closure, `f` will be able to see it when it searches from its (modified) parent scope. Since `x` is not local to the closure, it will be searched for in the enclosing scope, where it was a parameter of the enclosing function.

It works as we would expect it to. Even though it is a complicated way of achieving this effect, there are no traps in the code.

```{r}
add1(3)
add2(4)
```

For reasons that will soon be apparent I just want to show you that setting a global variable `x` does not change the behaviour:

```{r}
x <- 3
add1(3)
add2(4)
```

It also shouldn't. When we read the definition of the closure, we can see that the `x` it refers to is the parameter of `g`, not a global variable.

But now I am going to break the closure without even touching it. I just do one simple extra thing in `f`: I don't just change the enclosing scope of `f`, I do the same for the caller of `f`. I set the parent of the caller of `f` to be its caller instead of its enclosing environment:

```{r}
f <- function() {
  my_env <- environment()
  call_env <- parent.frame()
  parent.env(my_env) <- call_env
  parent.env(call_env) <- parent.frame(2)
  y
}
```

I haven't touched the closure, `g`, of the `add1` and `add2` functions. I have just made a small change to `f`. Now, however, if I don't have a global variable for `x` the addition functions do not work. This would give me an error:

```r
rm(x)
add1(3)
```

Even worse, if I *do* have a global variable `x` I don't get an error, but I don't get the expected results either.

```{r}
x <- 3
add1(3)
add2(4)
```

What happens here, of course, is that we change the enclosing scope of the closure from the `g` call to the global environment (which is the calling scope of `g` as well as its parent environment), so this is where we now start the search for `x`. The evaluation environment for the `g` call is not on the search path any longer.

While you *can* modify environments in this way, you should need an excellent reason to do so. Changing the behaviour of completely unrelated functions is the worst kind of side effects. It is one thing to mess up your own function, but don't mess up other people's functions. Of course, we are only modifying active environments here. We have not permanently damaged any function; we have just messed up the behaviour of function calls on the stack---if we wanted to mess up functions permanently we can do so using the `environment` function as well, though, but doing that tends to be a more deliberate and thought-through choice.

Don't go modifying the scope of calling functions. If you want to change the scope of expressions you evaluate, you are better off creating new environment chains for this, rather than modifying existing ones; the latter solution can easily have unforeseen consequences while the former at least has consequences restricted to the function you are writing.

## Explicitly creating environments

You create a new environment with the `new.env` function. By default, this environment will have the current environment as its parent[^truth_of_the_env] and you can use functions such as `exists` and `get` to check what it contains.

[^truth_of_the_env]: If you check the documentation for `new.env` you will see that the default argument is actually `parent.frame()`. If you think about it, this is how it becomes the current environment: when you call `new.env` the current environment will be its parent frame.

```{r}
env <- new.env()
x <- 5
exists("x", env)
get("x", env)

f <- function() {
  x <- 7
  new.env()
}
env2 <- f()
get("x", env2)
```

You can also use the `$` subscript operator to access it, but in this case, R will not search up the parent list to find a variable; only if a variable is in the actual environment can you get to it.

```{r}
env$x
```

You can assign variables to environments using `assign` or through the `$<-` function.

```{r}
assign("x", 3, envir = env)
env$x
env$x <- 7
env$x
```

Depending on what you want to do with the environment, you might not want it to have a parent environment. There is no way to achieve that.

```r
env <- new.env(parent = NULL) # This won't work!
```

All environments have a parent except the empty environment, but you can get the next best thing by making this environment the parent of your new one.

```{r}
global_x <- "foo"
env <- new.env(parent = emptyenv())
exists("global_x", env)
```

We can try to do something a little more interesting with manually created environments: build a parallel call stack we can use to implement dynamic scoping rather than lexical scoping. Lexical scoping is the scoping we already have in R, where a function call's parent is the definition scope of the function. Dynamic scope instead has the calling environment. It is terribly hard to reason about programs in languages with dynamic scope, so I would advise that you avoid them, but for the purpose of education, we can try implementing it.

Since we don't want to mess around with the actual call stack and modify parent pointers, we need to make a parallel sequence of environments, and we need to copy the content of each call stack frame into these. We can copy an environment like this:

```{r}
copy_env <- function(from, to) {
  for (name in ls(from, all.names = TRUE)) {
    assign(name, get(name, from), to)
  }
}
```

Just for illustration purposes, we need a function that will show us what names we see in each environment when moving down towards the global environment---we don't want to go all the way down to the empty environment, here, so we stop a little early. This function lets us do that:

```{r}
show_env <- function(env) {
  if (!identical(env, globalenv())) {
    print(env)
    print(names(env))
    show_env(parent.env(env))
  }
}
```

Now comes the function for creating the parallel sequence of environments. It is not that difficult; we can use `parent.frame` to get the frames on the call stack arbitrarily deep---well, down to the first function call---and we can get the depth of the call stack using the function `sys.nframe`. The only thing we have to be careful about is adjusting the depth of the stack by one since we want to create the call stack chain for the *caller* of the function; not for the function itself. The rest is just a loop.

```{r}
build_caller_chain <- function() {
  n <- sys.nframe() - 1
  env <- globalenv()
  for (i in seq(1,n)) {
    env <- new.env(parent = env)
    frame <- parent.frame(n - i + 1)
    copy_env(frame, env)
  }
  env
}
```

To see it in action we need to set up a rather convoluted example with both nested scopes and a call stack. It doesn't look pretty, but try to work through it and consider what the function environments must be and what the call stack must look like.

[comment]: # I have to fake this one because of knitr...


```r
f <- function() {
  x <- 1
  function() {
    y <- 2
    function() {
      z <- 3
      
      print("---Enclosing environments---")
      show_env(environment())

      call_env <- build_caller_chain()
      print("---Calling environments---")
      show_env(call_env)
    }
  }
}
g <- f()()
h <- function() {
  x <- 4
  y <- 5
  g()
}
h()
## [1] "---Enclosing environments---"
## <environment: 0x1035937b8>
## [1] "z"
## <environment: 0x103596780>
## [1] "y"
## <environment: 0x1035964e0>
## [1] "x"
## [1] "---Calling environments---"
## <environment: 0x10354b748>
## [1] "z"
## <environment: 0x103553b20>
## [1] "x" "y"
```

When we call `h` it calls `g`, and we get the list of environments starting from the innermost level where we have a `z` and out till the outermost level, just before the global environment, where we have the `x`. On the (parallel) call-stack we also see a `z` first (it is in a copy o the function's environment, but it is there), but this chain is only two steps long and the second environment contains both `x` and `y`.

We can use the stack chain we constructed here to implement dynamic scoping. We simply need to evaluate expressions in the scope defined by this chain rather than the current evaluating environment. The `<<-` assignment operator won't work---it would require us to write a similar function to get that behaviour, and it would be a design choice to which degree changes made to this chain should be moved back into the actual call stack frames---but as long as it comes to evaluating expressions, we can use it for dynamic scoping.

## Environments and expression evaluation

Now, finally, we come to what this chapter is all about: how we combine expressions and environments to compute values. The good news is that we are past all the hard stuff and it gets pretty simple after all of the stuff above. All you have to do to evaluate an expression in any selected environment chain is to provide it to the `eval` function. We can see it in the example below that evaluates the same expression in the lexical scope and the dynamic scope:

```{r}
f <- function() {
  x <- 1
  function() {
    y <- 2
    function() {
      z <- 3
      
      cat("Lexical scope: ", x + y + z, "\\n")
      
      call_env <- build_caller_chain()
      cat("Dynamic scope: ", eval(quote(x + y + z), call_env), "\\n")
    }
  }
}
g <- f()()

h <- function() {
  x <- 4
  y <- 5
  g()
}
h()
```

The hard part of working with environments really isn't evaluating them. It is manipulating them. 