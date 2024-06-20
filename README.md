# Function Parameter Prefix Lint

### What it does

This lint checks if all function parameters start with `p_`.
This convention comes from large C/C++ codebases where it is common to use prefixes to identify the origin of a variable.

### Why is this bad?

It can be difficult for a reviewer to understand the origin of a variable if the prefix is not used.

### Known problems
Machine fix isn't implemented.
This requires finding all identifiers in the function body implying some more complex AST walk.

### Example
```rust
// example code where a warning is issued
fn foo(a: i32, b: i32) {
    println!("{} {}", a, b);
}
```
Use instead:
```rust
// example code that does not raise a warning
fn foo(p_a: i32, p_b: i32) {
    println!("{} {}", p_a, p_b);
}
```
