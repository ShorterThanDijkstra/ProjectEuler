def sumSquare (n : Nat) : Nat :=
  List.range' 1 n |> List.sum |> (. ^ 2)

def squareSum (n : Nat) : Nat :=
  List.range' 1 n |> List.map (. ^ 2) |> List.sum

#eval sumSquare 10 - squareSum 10
#eval sumSquare 100 - squareSum 100

