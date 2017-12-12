module Grammar
  ( Digit
  , Press
  , DaChar (DaChar)
  , DaPhone (DaPhone)
  , symbol
  , button
  , presses
  , DaPhone
  , numbers
  , letters
  , symbols
  , daPhone
  ) where

type Digit  = Char
type Press  = Int
data DaChar = DaChar {
    symbol  :: Char
  , button  :: Digit
  , presses :: Press
} deriving (Show)
newtype DaPhone = DaPhone [DaChar]

-- Nubers
one   = DaChar '1' '1' 1
two   = DaChar '2' '2' 4
three = DaChar '3' '3' 4
four  = DaChar '4' '4' 4
five  = DaChar '5' '5' 4
six   = DaChar '6' '6' 4
seven = DaChar '7' '7' 5
eight = DaChar '8' '8' 4
nine  = DaChar '9' '9' 5
zero  = DaChar '0' '0' 3

numbers = [ one,   two, three, four, five
          , six, seven, eight, nine, zero]

-- Letters
-- 2
a = DaChar 'a' '2' 1
b = DaChar 'b' '2' 2
c = DaChar 'c' '2' 3
-- 3
d = DaChar 'd' '3' 1
e = DaChar 'e' '3' 2
f = DaChar 'f' '3' 3
-- 4
g = DaChar 'g' '4' 1
h = DaChar 'h' '4' 2
i = DaChar 'i' '4' 3
-- 5
j = DaChar 'j' '5' 1
k = DaChar 'k' '5' 2
l = DaChar 'l' '5' 3
-- 6
m = DaChar 'm' '6' 1
n = DaChar 'n' '6' 2
o = DaChar 'o' '6' 3
-- 7
p = DaChar 'p' '7' 1
q = DaChar 'q' '7' 2
r = DaChar 'r' '7' 3
s = DaChar 's' '7' 4
-- 8
t = DaChar 't' '8' 1
u = DaChar 'u' '8' 2
v = DaChar 'v' '8' 3
-- 9
w = DaChar 'w' '9' 1
x = DaChar 'x' '9' 2
y = DaChar 'y' '9' 3
z = DaChar 'z' '9' 4

letters = [ a, b, c, d, e, f , g, h, i, j, k, l, m
          , n, o, p, q, r , s, t, u, v, w, x, y, z]

-- Symbols
star  = DaChar '*' '*' 2
upper = DaChar '^' '*' 1
plus  = DaChar '+' '0' 1
space = DaChar '_' '0' 2
dot   = DaChar '.' '#' 1
comma = DaChar ',' '#' 2
hash  = DaChar '#' '#' 3

symbols = [ star, upper, plus, space
          ,  dot, comma, hash]

daPhone = DaPhone $ numbers ++ letters ++ symbols

