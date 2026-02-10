# lam
A small functional programming language with partial application and operator sections.
Written in Zig.

# Example
```
(+ 1) 5
```

```
λ (+ 1) 5
6
```

This is an application, an application consists of a section `(+ 1)` and a literal `5`. Section `(+ 1)` is a partially applied addition operator - its a function which takes an argument and adds one onto that value. Since `5` is applied to this section, the result will be `6`.

## Syntax

### Sections
Partially applied functions which fix one operand.
- `(+ x)` - add `x` to the argument.
- `(- x)` - subtract `x` from the argument.
- `(* x)` - multiply `x` by the argument.
- `(/ x)` - divide `x` by the argument.

### Applications
Apply a section (function) to a value.
- `(+ 1) 5` - will take 5 and add 1 onto it.
- `(* 2) 6` - will take 6 and multiply it by 2.

### Instrincts
Atomics of lam, functions which cannot be broken down into simple expressions are built-in.
- `(list 10) 5` - creates a list of `5` integers with the value `10`.