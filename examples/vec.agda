data T : Set where
  tt : T

data N : Set where
  zero : N 
  suc  : (_ : N) → N

data Vec (A : Set) : (n : N) → Set where
  nil  : Vec A zero
  cons : (n : N) (_ : Vec A n) (_ : A) → Vec A (suc n)

_ : ∀ (_ : N) → Set
_ = λ (n : N) → Vec T n 