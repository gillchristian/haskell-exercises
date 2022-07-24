{-# LANGUAGE LambdaCase #-}

module ParensDepth where

maxDepth :: String -> Int
maxDepth =
  maximum
    . scanl1 (+)
    . map (\case '(' -> 1; _ -> -1)
    . filter (`elem` "()")
