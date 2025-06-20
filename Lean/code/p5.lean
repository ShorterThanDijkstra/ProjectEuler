import Std.Data.HashMap

def myMap : Std.HashMap String Nat := 
  Std.HashMap.emptyWithCapacity 10
  |>.insert "apple" 5
  |>.insert "banana" 3
  |>.insert "cherry" 8

#eval myMap

def nextPrime : Nat -> Nat := sorry

def insertMap (m : Std.HashMap Nat Nat) (i : Nat) : Std.HashMap Nat Nat := sorry

def factors (n : Nat) : Std.HashMap Nat Nat :=
  let res := Std.HashMap.emptyWithCapacity (n / 2)
  let rec go i m :=
    if i >= n then m
    else let i' := nextPrime i
         let m' := if (n % i) == 0 then insertMap m i else m 
         go i' m
  go 2 n 
def main : IO Unit :=
  IO.println s!"Hello!"
