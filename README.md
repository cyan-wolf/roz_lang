# Roz Programming Language

## About
**Roz** is a dynamically-typed programming language. It is a [Rust](https://www.rust-lang.org/) implementation of 
the [Lox](https://craftinginterpreters.com/the-lox-language.html) language. 

## What is Lox?
Lox is a dynamically-typed interpreted programming language implemented in the [Crafting Interpreters](https://craftinginterpreters.com/) book by Robert Nystrom. 
In the book, the author discusses two implementations of Lox: one in Java called **jlox** and another one in C called **clox**. 
* In the **jlox** version, scripts are executed by parsing using a 
recursive descent parser and then by traversing the generated abstract syntax tree (AST).
* In the **clox** version, scripts are executed by generating bytecode which is then executed on a 
bytecode virtual machine (VM).

## What is Roz?
Roz is based on jlox, which means that it executes scripts by generating and then traversing an AST, not by implementing a bytecode-VM.
Roz mostly implements all the features of jlox, such as variables, functions, `if`/`else` statements, `while`/`for` loops, and classes.
In addition, it implements a few extra features such as anonymous functions, standard library functions (for file IO and math), 
growable lists, hash maps, exceptions, `try`/`catch`/`finally` blocks, and more. There are many example Roz source files in the 
`demos` directory, which show off the various features, standard libary functions, and syntax.

## How to Use
If you want to try out the language, you need to:
* [Install Rust](https://www.rust-lang.org/tools/install) on your system.
* Download the source code.
* Navigate to the root directory of the project and run `cargo run`, which will start an interactive session where you can define variables, call functions, and more.
* To run a source file, specify the name of the file after `cargo run`, i.e. `cargo run your_file_name.roz`.
  


