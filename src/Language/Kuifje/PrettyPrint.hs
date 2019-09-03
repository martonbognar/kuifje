module Language.Kuifje.PrettyPrint where

import Data.List (transpose)
import Text.PrettyPrint.Boxes
import qualified Data.Map.Strict as HM (mapWithKey, elems)

import Language.Kuifje.Distribution

class Boxable a where
  toBox :: a -> Box

instance Boxable Bool where
  toBox b = text (show b)

instance Boxable Integer where
  toBox i = text (show i)

instance Boxable Int where
  toBox i = text (show i)

instance Show a => Boxable [a] where
  toBox = text . show

instance (Show a, Show b) => Boxable (a,b) where
  toBox p  =  text (show p)

instance (Show a, Show b, Show c) => Boxable (a,b,c) where
  toBox p  =  text (show p)

instance (Show a, Show b, Show c, Show d) => Boxable (a,b,c,d) where
  toBox p  =  text (show p)

distToBox :: (Ord a, Boxable a) => Dist a -> Box
distToBox d = tabulate $ HM.elems (HM.mapWithKey lambdaPrint (unpackD d))
                where lambdaPrint e p = [text (show p), toBox e]

instance (Boxable a, Ord a) => Boxable (Dist a) where
  toBox = distToBox

instance (Ord a, Boxable a) => Show (Dist a) where
  show = render . distToBox

tabulate :: [[Box]] -> Box
tabulate rs = table
  where
   heights  = map (maximum . map rows) rs
   rs''     = zipWith (\r h -> map (alignVert top h) r) rs heights
   columns  = transpose rs''
   widths   = map (maximum . map cols) columns
   rs'      = transpose (zipWith (\c w -> map (alignHoriz left w) c) columns widths)
   columns' = map (hsep 3 top) rs'
   table    = vsep 0 left columns'
