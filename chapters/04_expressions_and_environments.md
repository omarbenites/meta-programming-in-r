# Expressions and environments

In this chapter we dig deeper into how environments work and how we can evaluate expressions in different environments. Understanding how environments are chained together helps you understand how the language finds variables, and being able to create, manipulate, and chain together environments when evaluating expressions is a key trick for meta-programming.

## Chains of linked environments

We have seen how functions have associated environments that capture where they were defined, and that when we evaluate a function call we have an evaluation environment that is linked to this definition environment. We have also see how this works through a linked list of parent environments. So far, though, we have claimed that this chain ends in the global environment, where we look for variables if we don't find them in any nested scope. For all the examples we have seen so far, this might as well be true, but in any real use of R you have packages loaded. The reason that you can find functions from packages is because these packages are also found in the chain of environments. The global environment also has a parent, and when you load packages, the last package you loaded will be the parent of the global environment and the previous package will be the parent of the new package. This explains both how you can find variables in loaded packages and why loading new packages can overshadow variables defined in other packages.

It has to stop at some point, of course, and there is a special environment that terminates the sequence. This is know as the empty environment and it is the only environment that doesn't have a parent. When you start up R, the empty environment is there, then R puts the base environment, where all the base language functionality lives, then it puts an `Autoload` environment, responsible for loading data on demand, and on top of that it puts the global environment. The base environment and the empty environment are sufficiently important that we have functions to get hold of them. These are `baseenv` and `emptying`, respectively.

When you import packages, or generally `attach` a namespace, it gets put of the this list just below the global environment. The function `search` will give you the sequence of environments from the global environment and down. We can see how loading a library affects this list in this example:

```{r}
search()
library(MASS)
search()
```

The `search` function is an internal function, but we can write our own version to get a feeling for how it could work. While `search` search from the global environment, though, we will make our function more general and give it an environment to start from. We simply need it to print out environment names and then move from the current environment to the parent until we hit the empty environment. 

To get the name of an environment we can use the function `environmentName`. Not all environments have names---those we create when we nest or call functions or those we create with `new.env` have not---but environments created when we are loading packages do.^[If you want to name your environments, you can set the attribute "name". It is generally not something you need, though.] If an environment doesn't have a name, though, `environmentName`, will give us empty strings when environments do not have names. If so, we will instead just use `str` to get a representation of environments we can `cat` and make sure we only get a simple representation by not getting any potential attributes printed as well.

To check if we have reached the end of the environment chain we check `identical(env, emptyenv())`. You cannot compare two environments with `==`, but you can use `identical`. Our function could look like this:

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

Calling it with the global environment as argument should give us a result similar to `search`, except we are printing the environments instead of returning a list of names.

```{r}
my_search(globalenv())
```

Since we can give it any environment, we can try to get hold of the environment of a function. If we write a function nested into other function scopes we can see that we get (nameless) environments for the functions.

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

Having such extra environments is how packages manage to have private functions within a package, and have other functions that are exported to users of a package. The base package is special in only defining two environments, the namespace for the package and the package environment. All other packages actually have three packages set up in their environment chain before the global environment: the package namespace, a namespace containing imported symbols, and then the base environment, which, as we just saw, connects to the global environment. A graph of environments when three packages are loaded, `MASS`, `stats` and `graphics`, is shown in +@fig:environment-graph (here `graphics` was loaded first, then `stats` and then `MASS`, so `MASS` appears first, followed by `stats` and then `graphics` on the path from the global environment to the base environment). The solid arrows indicate parent pointers for the environments and the dashed arrows indicate from which package symbols are exported into package environments.

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

The function `sd` sits in the package `stats`. Its parent is `namespace:stats` and its grandparent is `imports:stats`, and its great grandparent is `namespace:base`. If we access `sd` from the global environment, though, we find it in `package:stats`. 

```{r}
my_search(environment(sd))
environment(sd)
parent.env(environment(sd))
parent.env(parent.env(environment(sd)))
parent.env(parent.env(parent.env(environment(sd))))
``` 

Figure @fig:environment-graph-functions-in-package shows how both `ls` and `sd` sits in package and namespace environments and how their parents are the namespace rather than the package environment.

![Environment graph showing the positions of `stats::sd` and `base::ls`.](figures/environment-graph-functions-in-package){#fig:environment-graph-functions-in-package}

As we now see, this simple way of chaining environments give us not only lexical scope in functions, it also explains how namespaces in packages work.

## Environments and function calls



## Environments and expression evaluation

