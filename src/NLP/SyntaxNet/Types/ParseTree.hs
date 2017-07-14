{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}

module NLP.SyntaxNet.Types.ParseTree where

import           Control.Lens
import           Data.Default
import           Data.Text
import           Data.Tree
import           Data.Tree.Lens
import qualified Data.Map as M

import NLP.SyntaxNet.Types.CoNLL

--------------------------------------------------------------------------------

-- |A 'Tree' of 'Token's
type TokenTree = Tree Token

-- |A 'Forest' of 'Token's
type TokenForest = Forest Token

-- |A Map of text values the appropriate tree
type TokenMap = M.Map Text TokenTree

--------------------------------------------------------------------------------

-- |Given a list of 'TokenTree's, return the top-level 'token's.
forestTokens :: [TokenTree] -> [Text]
forestTokens = Prelude.map forestToken

-- |Given a 'TokenTree', return the top-level 'token'.
forestToken :: TokenTree -> Text
forestToken (Node tkn subf) = tnWord tkn

-- |Given a list of 'TokenTree's, return a map of each token with
-- the appropriate tree.
mkMap :: TokenForest -> TokenMap
mkMap =
  M.fromList . tokenTreeAlist
    where
      tokenTreeAlist frs = Prelude.zip (forestTokens frs) frs

-- | Convert list of nodes with defined level
--   into Tree structure
fromList :: [Token] -> Maybe TokenTree
fromList nodes =
  Just $ fromListAux nodes 0
    where fromListAux :: [Token] -> Int -> TokenTree
          fromListAux = undefined
          
-- | Convert Tree structure to a sequantial list structure
-- 
toList :: Tree Token -> [Token]
toList t =
  toListAux t []
    where
      toListAux (Node x ts) xs = x : Prelude.foldr toListAux xs ts
