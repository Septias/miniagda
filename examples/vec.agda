data T : Set where
  tt : T

data Eq (A : Set) (x : A) : (_ : A) → Set1 where
  refl : Eq A x x

data N : Set where
  zero : N 
  suc  : (n : N) → N

data Leq : (_ : N) (_ : N) → Set where
  lz : (n : N) → Leq zero n
  ls : (m : N) (n : N) (_ : Leq m n) → Leq (suc m) (suc n)

data Vec (A : Set) : (_ : N) → Set where
  nil  : Vec A zero
  cons : (n : N) (_ : Vec A n) (_ : A) → Vec A (suc n)

data Sum (A : Set) (B : Set) : Set where
  left  : (_ : A) → Sum A B
  right : (_ : B) → Sum A B 

_ : Eq N (suc zero) (suc zero) 
_ = refl {A = N} {x = (suc zero)}
