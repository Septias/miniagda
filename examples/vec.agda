data N : Set where
  zero : N 
  suc  : N → N

data Vec (A : Set) : (n : N) → Set where
  nil  : Vec A zero
  cons : (n : N) (_v : Vec A n) (_a : A) → Vec A (suc n)

------------------------------------------------------------

-- Γ = {N : Set, zero : N, cons : N → N}

-- Δ = {A :0 Set}                           
-- -- Ξ = {n :1 N} wf.                          
-- -- Δ₁ = () wf.                         
-- -- Δ₂ = (n :1 N) (_v :2 Vec A n) (_a :3 A) wf.
-- lvls(Δᵢ) <= 0
-- A = vars(A)                                  
-- ΔΔ₂ ⊢ (suc n) : Ξ 

