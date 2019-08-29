{-# LANGUAGE TemplateHaskell #-}

module Password where

import Prelude hiding ((!!))
import Control.Lens hiding (Profunctor, dimap)
import Data.List ((\\),genericIndex,permutations,sortBy)
import Data.Semigroup

import Distribution
import PrettyPrint ()
import Semantics
import Syntax

($=) :: (a -> b) -> a -> b
($=) = ($)

(.^) :: s -> ASetter s t a b -> b -> t
(.^) s x y = set x y s

(!!) :: [a] -> Int -> a
(!!) = Data.List.genericIndex
infixl 7 !!  -- lower precedence than in Prelude

data SP = SP {
  _pw :: [Char],
  _gs :: [Char],
  _l :: [Int],
  _i :: Int,
  _ans :: Bool
  } deriving (Show, Eq, Ord)

makeLenses ''SP

makeState :: [Char] -> [Char] -> SP
makeState pw  gs = SP {  _pw = pw, _gs = gs,  _l = [], _i = 0, _ans = True }

projectPw :: Dist (Dist SP) -> Dist (Dist [Char])
projectPw = fmap (fmap (\s -> s^.pw))

initialDist :: [Char] -> [Char] -> Dist SP
initialDist pw gs = uniform [ makeState pw' gs | pw' <- permutations pw]

basicI :: Int -> Kuifje SP
basicI n =
  update (\s -> return (s.^i $= 0)) <>                          -- |i := 0;|
  update (\s -> return (s.^ans $= True)) <>                     -- |ans := true|
  while (\s -> return (s^.ans && s^.i < n))                     -- |while (ans && i<N) do|
    (                                                           -- |begin|
    cond (\s -> return ((s^.pw !! s^.i) /= (s^.gs !! s^.i)))    -- |if (pw[i] /= gs[i])|
          (update (\s -> return (s.^ans $= False)))             -- |then ans := false|
          skip <>                                               -- |else skip|
    (update (\s -> return (s.^i $= (s^.i+1))))                  -- |i++|
    )                                                           -- |end|

hyperI :: [Char] -> [Char] -> Dist (Dist [Char])
hyperI pw gs = projectPw (hysem (basicI (length pw)) (initialDist pw gs))

basicL :: Int -> Kuifje SP
basicL n =
  update (\s -> return (s.^i $= 0)) <>                          -- |i := 0;|
  update (\s -> return (s.^ans $= True)) <>                     -- |ans := true|
  while (\s -> return (s^.i < n))                               -- |while i<N do|
    (                                                           -- |begin|
    cond (\s -> return ((s^.pw !! s^.i) /= (s^.gs !! s^.i)))    -- |if (pw[i] /= gs[i])|
          (update (\s -> return (s.^ans $= False)))             -- |then ans := false|
          skip <>                                               -- |else skip|
    (update (\s -> return (s.^i $= (s^.i+1))))                  -- |i++|
    )                                                           -- |end|

hyperL :: [Char] -> [Char] -> Dist (Dist [Char])
hyperL pw gs = projectPw (hysem (basicL (length pw)) (initialDist pw gs))

basicM :: Int -> Kuifje SP
basicM n =
  update (\s -> return (s.^i $= 0)) <>                          -- |i := 0;|
  update (\s -> return (s.^ans $= True)) <>                     -- |ans := true|
  while (\s -> return (s^.i < n))                               -- |while i<N do|
    (                                                           -- |begin|
    (update (\s -> return (s.^ans $=                            -- |ans :=|
      (s^.ans && (s^.pw !! s^.i) == (s^.gs !! s^.i))))) <>      -- |ans && (pw[i] = gs[i]);|
    (update (\s -> return (s.^i $= (s^.i+1))))                  -- |i++|
    )                                                           -- |end|

hyperM :: [Char] -> [Char] -> Dist (Dist [Char])
hyperM pw gs = projectPw (hysem (basicM (length pw)) (initialDist pw gs))

basicN :: Int -> Kuifje SP
basicN n =
  update (\s -> return (s.^i $= 0)) <>                          -- |i := 0;|
  update (\s -> return (s.^ans $= True)) <>                     -- |ans := true|
  while (\s -> return (s^.i < n))                               -- |while i<N do|
    (                                                           -- |begin|
    (update (\s -> return (s.^ans $=                            -- |ans :=|
      (s^.ans && (s^.pw !! s^.i) == (s^.gs !! s^.i))))) <>      -- |ans && (pw[i] = gs[i]);|
    (update (\s -> return (s.^i $= (s^.i+1))))                  -- |i++|
    ) <>                                                        -- |end;|
  observe (\s -> return (s^.ans))                               -- |observe ans|

hyperN :: [Char] -> [Char] -> Dist (Dist [Char])
hyperN pw gs = projectPw (hysem (basicN (length pw)) (initialDist pw gs))

basicR :: Int -> Kuifje SP
basicR n =
  update (\s -> return (s.^l $= [0..n-1])) <>                   -- |l := [0,...,n-1];|
  update (\s -> return (s.^ans $= True)) <>                     -- |ans := true;|
  while (\s -> return (s^.ans && not (null (s^.l))))            -- |while (ans && l/=[]) do|
    (                                                           -- |begin|
    update (\s -> uniform [s.^i $= j | j <- s^.l]) <>           -- |i := uniform(l);|
    (update (\s -> return (s.^ans $=                            -- |ans :=|
      (s^.ans && (s^.pw !! s^.i) == (s^.gs !! s^.i))))) <>      -- |ans && (pw[i] = gs[i]);|
    (update (\s -> return (s.^l $= (s^.l \\ [s^.i]))))          -- |l := l - {i}|
    ) <>                                                        -- |end;|
  observe (\s -> return (s^.ans))                               -- |observe ans|

hyperR :: [Char] -> [Char] -> Dist (Dist [Char])
hyperR pw gs = projectPw (hysem (basicR (length pw)) (initialDist pw gs))

basicS :: Int -> Kuifje SP
basicS n =
  update (\s -> return (s.^l $= [0..n-1])) <>                   -- |l := [0,...,n-1];|
  update (\s -> return (s.^ans $= True)) <>                     -- |ans := true;|
  while (\s -> return (s^.ans && not (null (s^.l))))            -- |while (ans && l/=[]) do|
    (                                                           -- |begin|
    update (\s -> uniform [s.^i $= j | j <- s^.l]) <>           -- |i := uniform(l);|
    cond (\s -> return ((s^.pw !! s^.i) /= (s^.gs !! s^.i)))    -- |if (pw[i] /= gs[i])|
          (update (\s -> return (s.^ans $= False)))             -- |then ans := false|
          skip <>                                               -- |else skip|
    (update (\s -> return (s.^l $= (s^.l \\ [s^.i]))))          -- |l := l - {i}|
    ) <>                                                        -- |end;|
  observe (\s -> return (s^.ans))                               -- |observe ans|

hyperS :: [Char] -> [Char] -> Dist (Dist [Char])
hyperS pw gs = projectPw (hysem (basicS (length pw)) (initialDist pw gs))

ge :: Ord a => Dist a -> Prob
ge = sum . zipWith (*) [1..] . (sortBy (flip compare) . map snd . runD . reduction)

run :: IO ()
run = do
  putStrLn "hyperI \"abc\" \"abc\""
  print $ hyperI "abc" "abc"
  putStrLn "hyperI \"abc\" \"axc\""
  print $ hyperI "abc" "axc"
  putStrLn "hyperL \"abc\" \"abc\""
  print $ hyperL "abc" "abc"
  putStrLn "hyperM \"abc\" \"abc\""
  print $ hyperM "abc" "abc"
  putStrLn "hyperN \"abc\" \"abc\""
  print $ hyperN "abc" "abc"
  putStrLn "hyperR \"abc\" \"abc\""
  print $ hyperR "abc" "abc"
  putStrLn "condEntropy bayesVuln (hyperR \"abc\" \"abc\")"
  print $ condEntropy bayesVuln (hyperR "abc" "abc")
  putStrLn "condEntropy bayesVuln (hyperI \"abc\" \"abc\")"
  print $ condEntropy bayesVuln (hyperI "abc" "abc")
  putStrLn "condEntropy bayesVuln (hyperI \"abcde\" \"abcde\")"
  print $ condEntropy bayesVuln (hyperI "abcde" "abcde")
  putStrLn "condEntropy bayesVuln (hyperR \"abcde\" \"abcde\")"
  print $ condEntropy bayesVuln (hyperR "abcde" "abcde")
  putStrLn "condEntropy ge (hyperR \"abc\" \"abc\")"
  print $ condEntropy ge (hyperR "abc" "abc")
  putStrLn "condEntropy ge (hyperI \"abc\" \"abc\")"
  print $ condEntropy ge (hyperI "abc" "abc")
  putStrLn "condEntropy ge (hyperR \"abcde\" \"abcde\")"
  print $ condEntropy ge (hyperR "abcde" "abcde")
  putStrLn "condEntropy ge (hyperI \"abcde\" \"abcde\")"
  print $ condEntropy ge (hyperI "abcde" "abcde")
  putStrLn "condEntropy bayesVuln (hyperI \"abcdef\" \"abcdef\")"
  print $ condEntropy bayesVuln (hyperI "abcdef" "abcdef")
  putStrLn "condEntropy bayesVuln (hyperR \"abcdef\" \"abcdef\")"
  print $ condEntropy bayesVuln (hyperR "abcdef" "abcdef")
  putStrLn "condEntropy ge (hyperI \"abcdef\" \"abcdef\")"
  print $ condEntropy ge (hyperI "abcdef" "abcdef")
  putStrLn "condEntropy ge (hyperR \"abcdef\" \"abcdef\")"
  print $ condEntropy ge (hyperR "abcdef" "abcdef")
  putStrLn "hyperS \"abc\" \"abc\""
  print $ hyperS "abc" "abc"
