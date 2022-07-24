- `hspec`      => Example Based Testing
- `QuickCheck` => Property Based Testing

---

```bash
$ stack ghci <project-name>:<test-suite-name>
```

---

```
λ> :i Gen
λ> :i Arbitrary
```

---

```haskell
trivialInt :: Gen Int
trivialInt = return 1
```

`return` (and `pure`) => "puts" a value inside a monad

---

`Gen Int` (`Gen Float`) always starts with `0` (`0.0`)

---

Left at "14) Morse code"
