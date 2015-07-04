let multiplies x = x * 10
let nonzero a b = a <> 0 && b <> 0
let rec sum n = if n <= 1 then n else n + sum (n - 1)
let rec power x n = if n <= 1 then x else n * power x (n - 1)
(*let x = 1 in let x = 2 x + x*)
let rec factorial n = if n <= 1 then n else n * factorial (n - 1)
