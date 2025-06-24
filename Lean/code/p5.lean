import Std.Data.HashMap
import Init.Data.Float

def nOverLogLog_n (n : Nat) : Float :=
  if n < 3 then 0.0 
  else
    let n_float := n.toFloat
    let log_n := n_float.log
    let log_log_n := log_n.log
    n_float / log_log_n

partial def isPrime (n : Nat) : Bool :=
  if n < 2 then false
  else if n = 2 then true
  else if n % 2 = 0 then false
  else
    let rec checkOddDivisors (d : Nat) : Bool :=
      if d * d > n then true
      else if n % d = 0 then false
      else checkOddDivisors (d + 2)
    checkOddDivisors 3

partial def nextPrimeFrom (n : Nat) : Nat :=
  if isPrime n then n
  else
    let rec findNext (m : Nat) : Nat :=
      if isPrime m then m
      else findNext (m + 1)
    findNext (n + 1)

def insertMap (m : Std.HashMap Nat Nat) (i : Nat) : Std.HashMap Nat Nat :=
    let count := m.getD i 0
    m.insert i (count + 1)

def unionMap (m1 : Std.HashMap Nat Nat) (m2 : Std.HashMap Nat Nat) : (Std.HashMap Nat Nat) :=
  let keys := m1.keys.append m2.keys
  let res := Std.HashMap.emptyWithCapacity keys.length
  List.foldr (fun k acc => let v1 := m1.getD k 0
                           let v2 := m2.getD k 0
                           acc.insert k (max v1 v2))
             res
             keys

def multiMap (m : Std.HashMap Nat Nat) : Nat :=
  m.fold (fun acc k v => acc * (k ^ v)) 1

partial def factors (n : Nat) : Std.HashMap Nat Nat :=
  let res := Std.HashMap.emptyWithCapacity (n / 2) -- Hardy-Ramanujan
  let rec loop i m n' :=
    if i > n' then m
    else if (n' % i) == 0 then loop i (insertMap m i) (n' / i)
         else loop (nextPrimeFrom (i + 1)) m n'
  loop 2 res n

def smallestMultiple (n : Nat) : Nat :=
  let map := List.foldl (fun acc i => unionMap acc (factors i))
             (Std.HashMap.emptyWithCapacity n)
             (List.range' 2 (n - 1))
  --dbg_trace s!"{map.toList}"
  multiMap map

#eval smallestMultiple 10
#eval smallestMultiple 20
