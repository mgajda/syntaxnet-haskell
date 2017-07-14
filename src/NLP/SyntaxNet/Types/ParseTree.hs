{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}

module NLP.SyntaxNet.Types.ParseTree where

import Data.Text
import Control.Lens
import Data.Tree
import Data.Tree.Lens
import Data.Default

import NLP.SyntaxNet.Types.CoNLL

--------------------------------------------------------------------------------

-- | Convert list of nodes with defined level
--   into Tree structure
fromList :: [Token] -> Maybe (Tree Token)
fromList nodes =
  Just $ fromListAux nodes 0
    where fromListAux :: [Token] -> Int -> (Tree Token)
          fromListAux = undefined
          
-- | Convert Tree structure to a sequantial list structure
-- 
toList :: Tree Token -> [Token]
toList t =
  toListAux t []
    where
      toListAux (Node x ts) xs = x : Prelude.foldr toListAux xs ts
