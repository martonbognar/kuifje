{-# LANGUAGE TemplateHaskell #-}

module Password where

import Prelude hiding ((!!))
import Control.Lens hiding (Profunctor, dimap)
import Data.List ((\\),genericIndex,permutations,sortBy)
import Data.Semigroup

import Distribution
import PrettyPrint
import Semantics
import Syntax

($=) :: (a -> b) -> a -> b
($=) = ($)

(.^) s x y = set x y s

(!!) = Data.List.genericIndex
infixl 7 !!  -- lower precedence than in Prelude

data SP = SP { _pw :: [Char], _gs :: [Char], _l :: [Int], _i :: Int, _ans :: Bool }
  deriving (Show,Eq,Ord)

makeLenses ''SP

makeState :: [Char] -> [Char] -> SP
makeState pw  gs = SP {  _pw = pw, _gs = gs,  _l = [], _i = 0, _ans = True }

projectPw :: Dist (Dist SP) -> Dist (Dist [Char])
projectPw = fmap (fmap (\s -> s^.pw))

initialDist pw gs = uniform [ makeState pw' gs | pw' <- permutations pw]

basicI :: Int -> Kuifje SP
basicI n =
  update (\s -> return (s.^i $= 0)) <>                          -- |i := 0;|
  update (\s -> return (s.^ans $= True)) <>                     -- |ans := true|
  while (\s -> return (s^.ans && s^.i < n))                     -- |while (ans && i<N) do|
    (                                                           -- |begin|
    cond (\s -> return ((s^.pw !! s^.i) /= (s^.gs !! s^.i)))    -- \quad |if (pw[i] /= gs[i])|
          (update (\s -> return (s.^ans $= False)))             -- \qquad |then ans := false|
          skip <>                                               -- \qquad |else skip|
    (update (\s -> return (s.^i $= (s^.i+1))))                  -- \quad |i++|
    )                                                           -- |end|

hyperI pw gs = projectPw (hysem (basicI (length pw)) (initialDist pw gs))

basicL :: Int -> Kuifje SP
basicL n =
  update (\s -> return (s.^i $= 0)) <>                          -- |i := 0;|
  update (\s -> return (s.^ans $= True)) <>                     -- |ans := true|
  while (\s -> return (s^.i < n))                               -- |while i<N do|
    (                                                           -- |begin|
    cond (\s -> return ((s^.pw !! s^.i) /= (s^.gs !! s^.i)))    -- \quad |if (pw[i] /= gs[i])|
          (update (\s -> return (s.^ans $= False)))             -- \qquad |then ans := false|
          skip <>                                               -- \qquad |else skip|
    (update (\s -> return (s.^i $= (s^.i+1))))                  -- \quad |i++|
    )                                                           -- |end|

hyperL pw gs = projectPw (hysem (basicL (length pw)) (initialDist pw gs))

hyperM pw gs = projectPw (hysem (basicM (length pw)) (initialDist pw gs))

basicM :: Int -> Kuifje SP
basicM n =
  update (\s -> return (s.^i $= 0)) <>                          -- |i := 0;|
  update (\s -> return (s.^ans $= True)) <>                     -- |ans := true|
  while (\s -> return (s^.i < n))                               -- |while i<N do|
    (                                                           -- |begin|
    (update (\s -> return (s.^ans $=                            -- \quad |ans :=|
      (s^.ans && (s^.pw !! s^.i) == (s^.gs !! s^.i))))) <>      -- \qquad |ans && (pw[i] = gs[i]);|
    (update (\s -> return (s.^i $= (s^.i+1))))                  -- \quad |i++|
    )                                                           -- |end|

hyperN pw gs = projectPw (hysem (basicN (length pw)) (initialDist pw gs))

basicN :: Int -> Kuifje SP
basicN n =
  update (\s -> return (s.^i $= 0)) <>                          -- |i := 0;|
  update (\s -> return (s.^ans $= True)) <>                     -- |ans := true|
  while (\s -> return (s^.i < n))                               -- |while i<N do|
    (                                                           -- |begin|
    (update (\s -> return (s.^ans $=                            -- \quad |ans :=|
      (s^.ans && (s^.pw !! s^.i) == (s^.gs !! s^.i))))) <>      -- \qquad |ans && (pw[i] = gs[i]);|
    (update (\s -> return (s.^i $= (s^.i+1))))                  -- \quad |i++|
    ) <>                                                        -- |end;|
  observe (\s -> return (s^.ans))                               -- |observe ans|

basicR :: Int -> Kuifje SP
basicR n =
  update (\s -> return (s.^l $= [0..n-1])) <>                   -- |l := [0,...,n-1];|
  update (\s -> return (s.^ans $= True)) <>                     -- |ans := true;|
  while (\s -> return (s^.ans && not (null (s^.l))))            -- |while (ans && l/=[]) do|
    (                                                           -- |begin|
    update (\s -> uniform [s.^i $= j | j <- s^.l]) <>           -- \quad |i := uniform(l);|
    (update (\s -> return (s.^ans $=                            -- \quad |ans :=|
      (s^.ans && (s^.pw !! s^.i) == (s^.gs !! s^.i))))) <>      -- \qquad |ans && (pw[i] = gs[i]);|
    (update (\s -> return (s.^l $= (s^.l \\ [s^.i]))))          -- \qquad |l := l - {i}|
    ) <>                                                        -- |end;|
  observe (\s -> return (s^.ans))                               -- |observe ans|

hyperR pw gs = projectPw (hysem (basicR (length pw)) (initialDist pw gs))

ge:: Ord a => Dist a -> Prob
ge = sum . zipWith (*) [1..] . (sortBy (flip compare) . map snd . runD . reduction)

basicS :: Int -> Kuifje SP
basicS n =
  update (\s -> return (s.^l $= [0..n-1])) <>                   -- |l := [0,...,n-1];|
  update (\s -> return (s.^ans $= True)) <>                     -- |ans := true;|
  while (\s -> return (s^.ans && not (null (s^.l))))            -- |while (ans && l/=[]) do|
    (                                                           -- |begin|
    update (\s -> uniform [s.^i $= j | j <- s^.l]) <>           -- \quad |i := uniform(l);|
    cond (\s -> return ((s^.pw !! s^.i) /= (s^.gs !! s^.i)))    -- \quad |if (pw[i] /= gs[i])|
          (update (\s -> return (s.^ans $= False)))             -- \qquad |then ans := false|
          skip <>                                               -- \qquad |else skip|
    (update (\s -> return (s.^l $= (s^.l \\ [s^.i]))))          -- \qquad |l := l - {i}|
    ) <>                                                        -- |end;|
  observe (\s -> return (s^.ans))                               -- |observe ans|

hyperS pw gs = projectPw (hysem (basicS (length pw)) (initialDist pw gs))

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
  putStrLn "condEntropy bv (hyperR \"abc\" \"abc\")"
  print $ condEntropy bv (hyperR "abc" "abc")
  putStrLn "condEntropy bv (hyperI \"abc\" \"abc\")"
  print $ condEntropy bv (hyperI "abc" "abc")
  putStrLn "condEntropy bv (hyperI \"abcde\" \"abcde\")"
  print $ condEntropy bv (hyperI "abcde" "abcde")
  putStrLn "condEntropy bv (hyperR \"abcde\" \"abcde\")"
  print $ condEntropy bv (hyperR "abcde" "abcde")
  putStrLn "condEntropy ge (hyperR \"abc\" \"abc\")"
  print $ condEntropy ge (hyperR "abc" "abc")
  putStrLn "condEntropy ge (hyperI \"abc\" \"abc\")"
  print $ condEntropy ge (hyperI "abc" "abc")
  putStrLn "condEntropy ge (hyperR \"abcde\" \"abcde\")"
  print $ condEntropy ge (hyperR "abcde" "abcde")
  putStrLn "condEntropy ge (hyperI \"abcde\" \"abcde\")"
  print $ condEntropy ge (hyperI "abcde" "abcde")
  putStrLn "condEntropy bv (hyperI \"abcdef\" \"abcdef\")"
  print $ condEntropy bv (hyperI "abcdef" "abcdef")
  putStrLn "condEntropy bv (hyperR \"abcdef\" \"abcdef\")"
  print $ condEntropy bv (hyperR "abcdef" "abcdef")
  putStrLn "condEntropy ge (hyperI \"abcdef\" \"abcdef\")"
  print $ condEntropy ge (hyperI "abcdef" "abcdef")
  putStrLn "condEntropy ge (hyperR \"abcdef\" \"abcdef\")"
  print $ condEntropy ge (hyperR "abcdef" "abcdef")
  putStrLn "hyperS \"abc\" \"abc\""
  print $ hyperS "abc" "abc"
