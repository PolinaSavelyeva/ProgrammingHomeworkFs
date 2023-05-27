# ProgrammingHomeworkFs
SPBU's F# 1—2 semester homeworks

## Tasks

### Homework №1 
Implement things below.
1. A function that takes two numbers (base and exponent) and calculates the `power in a naive way`. 
2. A function that takes two numbers (base and exponent) and calculates the `power using fast exponentiation`.
3. A function that takes an array and returns the `difference between the largest and smallest elements` in that array.
4. A function that takes two integers and returns an `array of odd numbers` strictly between them.
5.  Add `tests` for all of this functions.

### Homework №2
For two types of lists, invented during the class, implement things below.
1. `Bubble Sort` — a function that takes a list and returns the sorted list.
2. `Quick Sort` — a function that takes a list and returns the sorted list using the Hoare partition scheme.
3. `Concatenation` of two lists — a function that takes two lists and returns a third list, which is the concatenation of the two input lists
4. Add `tests` for all of this functions.

### Homework №3
Use recursion and unmutable variables.
1. Describe an algebraic type that defines a `tree` in which each node can have an arbitrary number of children. The leaves and nodes store values of some arbitrary (same for all) type.
2. Implement a function that  finds the number of `different elements` stored in the nodes. 
3. Implement a function that constructs a list containing `all the values from all the nodes`.
5.  Add `tests` for all of this functions.

### Homework №4
1. Using algebraic data types, implement the `trees` necessary for representing sparse matrices and sparse vectors. Provide the ability to store values of different types in vectors and matrices (matrices can have rows of strings, rows of integers, etc.).
2. Implement the `vector type` and the `matrix type` using the previously implemented trees.
3. Implement a function for `multiplying a vector by a matrix`.
4. Add `tests` for all of this functions.

### Homework №5

1. Implement the `necessary operations` for BFS. The mask can be any vector. The element-wise operation can be any operation.
2. Implement the `BFS` algorithm using matrix-vector operations.
3. Add `tests` for both individual operations and the BFS.

### Homework №6
0. Configure [FSharpLint](https://fsprojects.github.io/FSharpLint/) to run on the server during project build.
1. Learn how to read files in the [matrix market format](https://math.nist.gov/MatrixMarket/).
2. Learn how to construct a `graph` based on the read adjacency matrix.
3. Implement `parallel versions` of matrix and vector operations.
4. Conduct `experimental performance` research on BFS and answer the questions below.
   a. Under what graph parameter settings is it more beneficial to use the parallel version of the algorithm?
   b. What is the optimal number of threads that yields the highest performance improvement? Why?
5. Prepare a [report](https://github.com/PolinaSavelyeva/Articles/tree/main/2023/BFS_report) on the conducted research.


## Running
### Requirements

Make sure the following requirements are installed on your system:

- [dotnet SDK](https://www.microsoft.com/net/download/core) 3.0 or higher (recommended 6.0+)
- [Mono](http://www.mono-project.com/) if you're on Linux or macOS.
or
- [VSCode Dev Container](https://code.visualstudio.com/docs/remote/containers)

### Building

```sh
> build.cmd <optional buildtarget> // on windows
$ ./build.sh  <optional buildtarget> // on unix
```
### Template
To find more options take a look at the [MiniScaffold](https://github.com/TheAngryByrd/MiniScaffold) template.