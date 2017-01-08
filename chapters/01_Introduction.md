
# Introduction

Welcome to the *Advanced Statistical Programming in R* series and this book, *Meta-programming in R*. I am writing this series, and this book, to have teaching material beyond the typical introductory level most text books on R have. It covers some of the more advanced techniques used in R programming such as fully exploiting functional programming, writing meta-programs (programming the actual language structures), and writing domain specific languages to embed in R.

## About the series

The *Advanced Statistical Programming in R* series consists of short, single-topic books, where each book can be used alone for teaching or learning R. That said, there will, of course, be some dependencies in topics, if not in content, among the books. For instance, functional programming is essential to understand for any serious R programming and the first book in the series covers that. Reading the other books without understanding functional programming will not be fruitful. However, if you are already familiar with functional programming, then you can safely skip the first book.

For each book, I will make clear what I think the prerequisites for reading the book are, but for the entire series I will expect you to already be familiar with programming and the basic R you will see in any introductory book. None of the books will give you a tutorial introduction to the R programming language.

If you have used R before for data analysis and are familiar with writing expressions and functions, and want to take it further and write more advanced R code, then the series is for you.


## About this book

This book gives an introduction to meta-programming. Meta-programming is when you write programs manipulating other programs: you treat code as data that you can generate, analyse, or modify. R is a very high-level language where all operations are functions, and all functions are data that can be manipulated. Functions are objects and you can, within the language, extract their components, modify them, or create new functions from their constituent components.

There is great flexibility in how function calls and expressions are evaluated. The lazy evaluation semantics of R means that arguments to functions are passed as unevaluated expressions and these expressions can be modified before they are evaluated or they can be evaluated in other environments than the context where a function is called. This can be exploited to create small domain-specific languages and is a key component in the "tidy verse" in packages such as `dplyr` or `ggplot2` where expressions are evaluated in contexts defined by data frames.

There is some danger in modifying how the language evaluates function calls and expressions, of course. It makes it harder to reason about code. On the other hand, adding small embedded languages for dealing with common programming tasks adds expressiveness to the language that far outweighs the risks of programming confusion, as long as such meta-programming is used sparingly and in well-understood (and well documented) frameworks.

In this book, you will learn how to manipulate functions and expressions, and how to evaluate expressions in non-standard ways. Prerequisites for reading this book are familiarity with functional programming, at least familiarity with higher-order functions, that is, functions that take other functions as input or that return functions, and some familiarity with classes and generic functions.

