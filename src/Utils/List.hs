module Utils.List ((!!?)) where

infixl 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(_:_) !!? n | n < 0 = Nothing
[] !!? _ = Nothing
(x:_) !!? 0 = Just x
(_:t) !!? n = t !!? (n - 1)
