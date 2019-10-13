module Safetail where

safetail1 x = if null x then [] else tail x

safetail2 x | null x = []
            | otherwise = tail x

safetail3 [] = []
safetail3 x = tail x

