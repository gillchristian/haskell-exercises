y monad de `(->) r` es componer funciones binarias donde todas comparten un argumento:

```hs
((>=>)  @((->) Int)) :: (a -> Int -> b) -> (b -> Int -> c) -> a -> Int -> c
```
