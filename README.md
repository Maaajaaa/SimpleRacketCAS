# SimpleRacketCAS
A simple "CAS" in the functional language Racket, made as an assignment for Computer Science class.

It is only designed for and __can only derive, simplify and evaluate symbolic expressions__ (in Racket syntax, but with a maximum of two terms per operation e.g. (* 8 x), but not more like (+ 40 1 1)).

The code is based on Code from the [Programming Wiki (German)](http://programmingwiki.de/Grundlagen_der_funktionsorientierten_Programmierung_mit_SCHEME/Projekt_CAS), where the assignment is also described (in German).

**Example**
```
(derivative '(* 5 (sin x)) 'x)
```
