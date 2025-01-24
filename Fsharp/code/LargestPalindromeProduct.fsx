let isPalindrome (n: int) : bool =
    let s = string n
    let sRev = System.String(s.ToCharArray() |> Array.rev)
    s = sRev

let largestPalindromeProduct: int * int * int =
    let rec go a b (larA, larB, lar) =
        let mul = a * b

        let next =
            if mul > lar && isPalindrome mul then
                (a, b, mul)
            else
                (larA, larB, lar)

        if a < 100 && b < 100 then next
        elif b <= 100 then go (a - 1) (a - 1) next
        else go a (b - 1) next in

    go 999 999 (0, 0, 0)
// (993, 913, 906609)