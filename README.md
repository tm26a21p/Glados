# GLaDOS: Generic Language and Data Operand Syntax

## Technical Overview

## Project Objective

GLaDOS is a programming language project implemented in Haskell. It aims to develop a minimalist clone of the Erlang interpreter, as its foundational component.

### General Information

Binary Name: 
Languages: Haskell
Compilation: Via Makefile, including re, clean, and fclean OR stack build
Repository Structure:
Source files included, excluding unnecessary files like binaries, temp files, and obj files.

### Build System and Dependencies
GLaDOS utilizes a Makefile for compilation, providing rules for building, cleaning, and removing temporary files.   
While you are free to choose any build system, I recommand Stack for its robustness and reliability.

### Testing

Comprehensive testing is crucial for the development of GLaDOS. Unit and integration tests are mandatory, and you must demonstrate how much of your code is covered by these tests. Additionally, automation of testing through Continuous Integration (CI) and Continuous Delivery (CD) is encouraged to maintain code quality and prevent the introduction of errors.

## Algorithm

### Syntax and Parsing

GLaDOS supports Symbolic-Expressions (S-expressions) as its primary representation. It must handle atoms such as signed integers and symbols, as well as lists with nested sub-expressions. The parser must be able to interpret these expressions correctly to ensure the proper functioning of the interpreter.

### Core Concepts
GLaDOS supports almost all basic of Erlang, and procedures. It also includes features for defining bindings, user-defined functions (both anonymous lambdas and named functions), and conditional expressions using "if" notation.

### Evaluation and Execution
The interpreter evaluates expressions using an Abstract Syntax Tree (AST) and an environment for variable bindings. It supports both anonymous and named functions, including recursion, and handles conditional expressions and built-in functions as specified in the project requirements.

### Tests and Run
This project uses GitHub Actions as defined in the .github/workflows/ci.yml file for continuous integration. The workflow is triggered on every pull request and push to the main branch.

The workflow has 2 jobs:

- test: This job sets up GHC and Erlang, clones the project, caches dependencies, builds the project, and runs tests. The tests are run with the command stack test --fast --no-terminal --system-ghc. After the tests, it runs functional tests with the command stack --local-bin-path . install --system-ghc && mv glados-exe glados && ./testFunctional.sh.

- release: This job is also dependent on the test job and only runs if the test job passes. It sets up GHC, clones the project, caches dependencies, builds the project, bumps the version, pushes a tag, and creates a release.

Example of launch launch this project locally:
```sh
make; ./glados ErlangCodeBase/official_basic_op.erl
```

## Conclusion

GLaDOS is a challenging yet rewarding project that offers valuable insights into functional programming, language design, and compiler construction. By implementing an Erlang interpreter in Haskell, I gain practical experience in parsing, evaluation, and testing, while also exploring advanced topics like type inference, optimization, and metaprogramming.