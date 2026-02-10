# lam
A small functional programming language with partial application, operator sections, and function composite.
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

```
λ (list 50) 10                                      
[50, 50, 50, 50, 50, 50, 50, 50, 50, 50]
```

Uses the `list` instrinct to creates a list of ten integers.

```
λ (compose (+ 1) (* 2)) 5
11
```

This is point free function composition, it allows us to compose the two partial functions into a single execution pipeline. Here is the breakdown:

```
= (+ 1)((* 2) 5)
= (+ 1)(10)
= 11
```

The `compose` special form takes two functions and creates a new function that applies then right-to-left.

# Syntax

## Sections
Partially applied functions which fix one operand.
- `(+ x)` - add `x` to the argument.
- `(- x)` - subtract `x` from the argument.
- `(* x)` - multiply `x` by the argument.
- `(/ x)` - divide `x` by the argument.
- `(get x)` - retrieve an element from the list, `x` being its index.

## Applications
Apply a section (function) to a value.
- `(+ 1) 5` - will take 5 and add 1 onto it.
- `(* 2) 6` - will take 6 and multiply it by 2.
- `(get 1) [10, 20, 30]` - will retrieve the item at index `1` in the list.

## Instrincts
Atomics of lam, functions which cannot be broken down into simple expressions. Built-in.
- `(list 10) 5` - creates a list of `5` integers with the value `10`.

## Composition
Compose two functions into a single function that applies in the right-to-left sequence.
- `(compose (+ 1) (* 2)) 5` - multiply `5` by `2` first, then add `1` onto the produced result.
- `(compose (+ 10) (get 1)) [5, 15, 25]` - retrieve the element at index `1` and add `10` onto it.

### Chaining
Compositions can also be chained for pipelining:
```
λ (compose (+ 100) (compose (* 2) (get 0))) [7, 8, 9]
114
```

This will retrieve the first element in the list `7`, multiply it by `2`, then add `100`.
Resulting in `114`.