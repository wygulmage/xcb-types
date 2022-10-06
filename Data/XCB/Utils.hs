module Data.XCB.Utils where

-- random utility functions

import Data.Char
import Control.Applicative

ensureUpper :: String -> String
ensureUpper [] = []
ensureUpper (x:xs) = toUpper x : xs

-- |Like mapMaybe, but for any Alternative.
-- Never returns 'empty', instead returns 'pure []'
mapAlt :: Alternative f => (a -> f b) -> [a] -> f [b]
mapAlt f = go
 where go [] = pure []
       go (y:ys) = liftA2 (:) (f y) (go ys) <|> go ys
