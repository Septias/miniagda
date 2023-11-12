data T : Set where
  tt : T

data N : Set where
  zero : N 
  suc  : (_ : N) → N

data Vec (A : Set) : (n : N) → Set where
  nil  : Vec A zero
  cons : (n : N) (_ : Vec A n) (_ : A) → Vec A (suc n)

_ : Vec T (suc zero)
_ = (λ (t1 : T) → λ (t2 : T) → cons {A = T} zero nil t1) tt tt