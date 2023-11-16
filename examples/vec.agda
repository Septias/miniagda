data T : Set where
  tt : T

data N : Set where
  zero : N 
  suc  : (n : N) → N

data Vec (A : Set) : (m : N) → Set where
  nil  : Vec A zero
  cons : (n : N) (vec : Vec A n) (a : A) → Vec A (suc n)

_ : Vec T (suc zero)
_ = cons {A = T} zero (nil {A = T}) tt 