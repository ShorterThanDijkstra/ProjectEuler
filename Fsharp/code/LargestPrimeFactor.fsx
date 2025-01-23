let isPrime (n: int64) : bool =
    let ends = double n |> sqrt |> int64 |> (+) (1: int64)

    let rec go i =
        match i with
        | i when i = ends -> true
        | i when n % i = 0 -> false
        | _ -> i + (1: int64) |> go in

    go 2

let largestFactor (n: int64) : int64 =
    let rec go i =
        if n % i = 0L then n / i else i + 1L |> go in

    go 2L

let rec largestPrimeFactor (n: int64) : int64 =

    let lf = largestFactor n

    let rec go i =
        match i with
        | i when i = 1L -> 1L
        | i when n % i = 0 ->
            if isPrime i then
                i
            else
                let l1 = largestPrimeFactor i
                let l2 = largestPrimeFactor (n / i)
                max l1 l2
        | _ -> i - 1L |> go in

    if isPrime n then 1L else lf |> go

let NUM: int64 = 600851475143L
