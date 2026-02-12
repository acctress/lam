# lam
A minimal, point-free functional Lisp written in Zig. Auto-currying, operator sections, and right-to-left function composition.

<img width="582" height="190" alt="image" src="https://github.com/user-attachments/assets/1471782e-f6c6-444f-8caa-f30b337b7d2b" />

# Recent Changes

* Updated to version identifier to correct standard.
* Updated to version `1.1.2`
    - Refactored source code into seperate files.

# Example
```
λ map (compose (+ 10) (* 2)) [1, 2, 3]
[12, 14, 16]
```

This demonstrates `map` applying a `composed` function to each element of the list.
It multiplies each element by `2`, and adds `10` to each produced result. The order of composition is righ-to-left.


```
λ (list 50) 10                                      
[50, 50, 50, 50, 50, 50, 50, 50, 50, 50]
```

Uses the `list` intrinsic to creates a list of ten integers.

```
λ compose (+ 1) (* 2) 5
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

## Console
- `putln "hello, world!"` - will print "hello world!" onto the console.

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

## intrinsics
Atomics of lam, functions which cannot be broken down into simple expressions. Built-in curried functions.

- `map fn list` - apply function to each element
- `compose f g` - create function that applies `g` then `f`
- `list value count` - create list of `count` elements with `value`
- `get index list` - retrieve element at `index`

## Complex Examples

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

### Pipeline
```
λ get 0 (map (compose (+ 10) (* 2)) [1, 2, 3])
12
```

### Lists
Lists in lam allow both explicit notation and range syntax with optional steps.
- `[1, 2, 3, 4, 5]` - is an explicit list.
- `[1..10]` - is a range notated list.
- `[1..10,2]` - is a range notated list with an explicit step.

### Strings and Chars
Under the hood, strings are just an array of chars, which is why we can do some cool things with them using the `str` intrinsic.
```
λ str (list 97 5)
→ aaaaa
```
This will create a list of five integers, all with the value of `97` (a in ASCII), the `str` intrinsic take's a list as it's argument, and converts that list into a string.

We can get creative with it:
```
λ str (map (+ 32) [72, 69, 76, 76, 79])
→ hello
```

Or:
```
λ putln (str (list 97 5))
aaaaa
```

And:
```
λ str (map (+ 65) [0..25])
→ ABCDEFGHIJKLMNOPQRSTUVWXYZ
```
