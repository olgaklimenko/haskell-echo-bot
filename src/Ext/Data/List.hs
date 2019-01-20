module Ext.Data.List where


list :: b -> ([a] -> b) -> [a] -> b
list b _ [] = b
list _ f xs = f xs
