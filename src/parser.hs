module Parser where

import Prelude hiding (Maybe (..))

data S = S1 A S
       | Nothing
       deriving (Eq, Show)

data A = A1 Char A Char
       | A2 Char
       deriving (Eq, Show)

parseS "" = (Nothing , "")

parseS ('(':xs) =
  let (a, r1) = parseA ('(':xs)
      (s, r2) = parseS r1
  in (S1 a s, r2)

parseS ('{':xs) =
  let (a, r1) = parseA ('{':xs)
      (s, r2) = parseS r1
  in (S1 a s, r2)

parseS ('[':xs) =
  let (a, r1) = parseA ('[':xs)
      (s, r2) = parseS r1
  in (S1 a s, r2)

parseA ('(':xs) =
  let (a, r1) = parseA xs
      (')':r2) = r1
  in (A1 '(' a ')', r2)

parseA (')':xs) = (A2 ' ', (')':xs))

parseA ('{':xs) =
  let (a, r1) = parseA xs
      ('}':r2) = r1
  in (A1 '{' a '}', r2)

parseA ('}':xs) = (A2 ' ', ('}':xs))

parseA ('[':xs) =
  let (a, r1) = parseA xs
      (']':r2) = r1
  in (A1 '[' a ']', r2)

parseA (']':xs) = (A2 ' ', (']':xs))

