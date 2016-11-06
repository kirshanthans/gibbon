#lang s-exp "../treelang.rkt"

;; Data type of unknown size.
(data Nat [Zero] [Suc Nat])

;; A field stored after the unknown-sized packed object.
(data Foo [MkFoo Nat Int])

(case (MkFoo (Suc (Zero)) 71)
  ;; This requires copy-insertion.
  [(MkFoo x y) y])
