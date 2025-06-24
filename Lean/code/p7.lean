partial def isPrime (n : Nat) : Bool :=
  if n < 2 then false
  else if n = 2 then true
  else if n % 2 = 0 then false
  else
    let rec checkOdd (d : Nat) : Bool :=
      if d * d > n then true
      else if n % d = 0 then false
      else checkOdd (d + 2)
    checkOdd 3

partial def nthPrime (n : Nat) : Nat :=
  let rec loop i k :=
    if !isPrime k then loop i (k + 1)
    else if i == n then k 
         else loop (i + 1) (k + 1)
  loop 1 1
#eval nthPrime 10001
