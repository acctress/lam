# lam
A minimal, point-free functional Lisp. Auto-currying, operator sections, and right-to-left function composition.

```
(fn (qsort xs)
    (if (== (len xs) 0) []
        (let (pivot (head xs))
        (let (rest  (tail xs))
        (let (lo (filter (\x -> (< x pivot)) rest))
        (let (hi (filter (\x -> (>= x pivot)) rest))
            (append (append (qsort lo) [pivot])
                    (qsort hi))))))))

(putln (qsort [5, 3, 8, 1, 9, 2, 7, 4, 6]))
```

```
λ (fn (fib n) (if (== n 0) 0 (if (== n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))
λ fib 10
→ 55
```

```
λ map (\x -> (if (> x 5) "big" "small")) [1..11]
→ ["small", "small", "small", "small", "small", "big", "big", "big", "big", "big"]
```

```
λ map (\f -> (f 10)) [(+ 1), (* 2), (- 3)]
→ [11, 20, -7]
```

# Recent Changes

* Version 0.3.22
* `==` and `!=` now accept both numbers and strings as arguments.
* panics in runtime.rs are replaced with LamError

To import modules in the REPL or script files, simply run:
```
(use "module")
```

For example:
```
λ (use "std/list")
λ take 2 [1..11]
→ [1, 2]
```

Pattern matching is trivial:
```
λ (fn (how_big x) (match x (0 "zero") (1 "one") (n if (> n 100) "big") (_ "idk")))

λ how_big 999
→ "big"

λ how_big 42
→ "idk"

λ how_big 0
→ "zero"

λ how_big 1
→ "one"
```

An arm in a pattern follows: `(<pattern> <body>)` or `(<pattern> <guard> <body>)`
* `(0 "zero")` is a literal pattern - matches if the value equals `0`
* `("hello" "greeting")` is a string literal pattern - matches string
* `(_ "idk")` `_` is a wildcard, will match anything but will not be bound
* `(x (+ x 1))` variable pattern, matches anything, binds the value to `x` for use in the `<body>`
* `([h, ...t] h)` list destructuring, `h` binds to the first element, `t` is bound to the rest of the list.
If nothing matches, lam panics with a `non exhaustive match`.

```
λ "hello world" |> split " " |> len
→ 2

λ [1..101] |> filter (\x -> (== 0 (- x (* 2 (/ x 2))))) |> len
→ 100

λ fold 0 (+) ([1..11] |> map (\x -> (* x x)))
→ 385

λ append [1..5] [6..11]
→ [1, 2, 3, 4, 6, 7, 8, 9, 10]

# we use a lambda to be explicit with the order of the comparison, x IS GREATER THAN 5
λ filter (\x -> (> x 5)) [1..11]
→ [6, 7, 8, 9, 10]

# a quirk: this reads as x IS GREATER THAN 5 but really it means "is 5 greater than x"
# if this predicate results in true, it'll filter the numbers where 5 is greater than x
# if you want it to be explicit, just write a lambda like above
λ filter (> 5) [1..11]
→ [1, 2, 3, 4]

λ filter (== 5) [1..100]
→ [5]

λ str (reverse (chars "hello"))
→ "olleh"

λ concat (head (split " " "hello world")) "!"
→ "hello!"

λ map (\x -> (+ x 1)) [1, 2, 3]
→ [2, 3, 4]

λ fold 0 (\a b -> (+ a b)) [1..11]
→ 55

λ map (\x -> (str [x])) [104, 101, 108, 108, 111]
→ ["h", "e", "l", "l", "o"]

λ map (\s -> (len s)) (split " " "the quick brown fox")
→ [3, 5, 5, 3]

λ (fn (fact n) (if (== n 0) 1 (* n (fact (- n 1)))))
λ map fact [1..11]
→ [1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800]

λ (fn (apply_all fns x) (map (\f -> (f x)) fns))
λ apply_all [(+ 1), (* 2), (- 3)] 10
→ [11, 20, -7]

λ (fn (fib n) (if (== n 0) 0 (if (== n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))
λ map fib [0..15]
→ [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]

λ fold "" (\acc x -> (concat acc (concat x " "))) (split "" "hello")
→ " h e l l o  "

λ map (\pair -> (fold 0 (+) pair)) [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
→ [6, 15, 24]

λ len (map (\x -> (* x x)) [1..1001])
→ 1000

λ fold 0 (+) (map (\x -> (* x x)) [1..101])
→ 338350

λ (fn (add x y) ((+ x) y))
λ add 3 4
→ 7

λ (fn (quad a b c x) ((+ ((+ ((* a) ((* x) x))) ((* b) x)))) c))
λ (let (f (quad 2 3 1)) (f 5))
→ 66

λ (== 5) 5
→ 1

λ zip [1,2,3,4] [5,6,7,8]
→ [[1, 5], [2, 6], [3, 7], [4, 8]]

λ [1..10]
→ [1, 2, 3, 4, 5, 6, 7, 8, 9]

λ [1..100;10]
→ [1, 11, 21, 31, 41, 51, 61, 71, 81, 91]

λ [(+ 1 5), (+ 2 6), (+ 3 7)]
→ [6, 8, 10]

λ [(+ 1), (* 2), (- 3)]
→ [<fn>, <fn>, <fn>]

λ (fn (mul a b) (* a b))
λ fold 1 mul [1..6]
→ 120

λ (if (== 1 2) 2 3)
→ 3

λ (+ 1 2)
→ 3

λ (* 3 4)
→ 12

λ (== 5 5)
→ 1

λ (== 5 6)
→ 0

λ (!= 5 6)
→ 1

λ (+ 1) 5
→ 6

λ map (*) [1, 2, 3]
→ [<fn>, <fn>, <fn>]

λ fold 0 (+) [1..11]
→ 55

λ fold 1 (*) [1..6]
→ 120

λ (fn (fact n) (if (== n 0) 1 (* n (fact (- n 1)))))
λ fact 10
→ 3628800

λ compose (+ 10) (* 2) 5
→ 20

λ map (compose (+ 10) (* 2)) [1, 2, 3]
→ [12, 14, 16]

λ str [97..123]
→ "abcdefghijklmnopqrstuvwxyz"

λ chars (str [97..123])
→ [97,  98,  99,  100, 101, 
   102, 103, 104, 105, 106, 
   107, 108, 109, 110, 111, 
   112, 113, 114, 115, 116, 
   117, 118, 119, 120, 121, 
   122
]

λ split " " "hello world!"
→ ["hello", "world!"]
```

`(if (== 1 2) 10 20)` looks funky, but it's really simple. It essentially says: `if 1 is equal to 2, use the value 10, else use 20`

As of version `0.2.21`, the `fold` function is unable to take bare operators as functions e.g. `fold 0 (+) list`, a wrapper will have to be defined like `mul`. This is because in version `0.2.21`, any zero argument sections cause an error - a fix will most likely be added next version.
> This issue has been resolved in version `0.2.22`

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
