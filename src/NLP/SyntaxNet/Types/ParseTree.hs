{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE ViewPatterns      #-}

module NLP.SyntaxNet.Types.ParseTree where

import           Control.Lens
import           Data.Default
import           Data.Text
import           Data.Tree
import           Data.Tree.Lens
import qualified Data.Map as M
import           Prelude  as P

import           Data.ConllToken
--import           Data.SyntaxTree (SyntaxtTree(..), createSyntaxTree)
import           Model.PennTreebank
import           Model.UniversalTreebank
import           Data.TagLabel

import           NLP.SyntaxNet.Types.CoNLL

--------------------------------------------------------------------------------

-- |A 'Tree' of 'SnConllToken Text's
type TokenTree = Tree (SnConllToken Text)

-- |A 'Forest' of 'SnConllToken Text's
type TokenForest = [Tree (SnConllToken Text)]

-- |A Map of text values the appropriate tree
type TokenMap = M.Map Text TokenTree

drawTree' :: TokenTree -> String
drawTree'  = P.unlines . draw

-- | Neat 2-dimensional drawing of a forest.
drawForest' :: TokenForest -> String
drawForest'  = P.unlines . P.map drawTree'

draw :: TokenTree -> [String]
draw (Node tkn ts0) =
  P.lines ((unpack $ _tnWord tkn)
           ++ " "
           ++ (show $ _tnPosFG tkn)
           ++ " "
           ++ (unpack $ toLabelText $ _tnRel tkn)) ++ drawSubTrees ts0
    where
      drawSubTrees []     = []
      drawSubTrees [t]    = shift " +-- " "   " (draw t)
      drawSubTrees (t:ts) = shift " +-- " " |  " (draw t) ++ drawSubTrees ts

      shift first other = P.zipWith (++) (first : repeat other)

--------------------------------------------------------------------------------

-- |Given a list of 'TokenTree's, return the top-level 'token's.
forestTokens :: [TokenTree] -> [Text]
forestTokens = P.map forestToken

-- |Given a 'TokenTree', return the top-level 'token'.
forestToken :: TokenTree -> Text
forestToken (Node tkn subf) = _tnWord tkn

-- |Given a list of 'TokenTree's, return a map of each token with
-- the appropriate tree.
mkMap :: TokenForest -> TokenMap
mkMap =
  M.fromList . tokenTreeAlist
    where
      tokenTreeAlist frs = P.zip (forestTokens frs) frs

-- | Return the elements at level i from a forest.  0-based indexing.
-- 
getLevel :: Forest a -> Int -> [a]
getLevel fs 0 = P.map rootLabel fs
getLevel fs n = P.concatMap (\fs' -> getLevel (subForest fs') (n-1)) fs

-- | Convert list of nodes with defined level
--   into Tree structure
fromList :: [(SnConllToken Text)] -> Maybe TokenTree
fromList (n:nodes) =
  Just $ Node n (fromListAux nodes []) 
    where fromListAux :: [(SnConllToken Text)]      -- ^ List of parsed Tokens
                      -> [TokenTree] -- ^ Building Forest
                      -> [TokenTree] -- ^ Final Forest
          fromListAux []         f = f
          fromListAux (t:ts:tss) f
            -- check current and next level
            | _tnId t == _tnId ts      = do
              -- next element on the same level, attach only              
              fromListAux (ts:tss) (f ++ [Node t []])
            | _tnId t <  _tnId ts      = do
              -- attach and move recursevly deep
              fromListAux (ts:tss) (f ++ [(Node t (fromListAux (ts:tss) [] ))])
            | _tnId t >  _tnId ts      = do
              -- next level is higher, attach only and move forest up              
              f ++ [Node t []]

-- | Debug version of fromList inside IO monad
-- 
fromList' :: [(SnConllToken Text)] -> IO (Maybe TokenTree)
fromList' (n:nodes) = do
  forest <- fromListAux nodes []
  return $ Just $ Node n forest 
    where fromListAux :: [(SnConllToken Text)] -- ^ List of parsed Tokens
                      -> [TokenTree]           -- ^ Building Forest
                      -> IO [TokenTree]        -- ^ Final Forest
          fromListAux []         f = return $ f
          fromListAux (t:ts:tss) f
            -- check current and next level
            | _tnId t == _tnId ts      = do
              -- next element on the same level, attach only
              let lvl = P.replicate (_tnId t) '-'
                  lvl'= P.replicate (_tnId t) ' '
              putStrLn $ lvl  
              putStrLn $ lvl' ++ "ch: 1; " ++ "lv: " ++ (show $ _tnId t) ++ " ; wr: " ++ (unpack $ _tnWord $ t)
              
              fromListAux (ts:tss) (f ++ [Node t []])
            | _tnId t <  _tnId ts      = do
              -- attach and move recursevly deep
              let lvl = P.replicate (_tnId t) '-'
                  lvl'= P.replicate (_tnId t) ' '
              putStrLn $ lvl  
              putStrLn $ lvl' ++ "ch: 2; " ++ "lv: " ++ (show $ _tnId t) ++ " ; wr: " ++ (unpack $ _tnWord $ t)

              sforest <- fromListAux (ts:tss) [] -- <
              fromListAux (ts:tss) (f ++ [(Node t sforest)])
            | _tnId t >  _tnId ts      = do
              -- next level is higher, attach only and move forest up
              let lvl = P.replicate (_tnId t) '-'
                  lvl'= P.replicate (_tnId t) ' '
              putStrLn $ lvl  
              putStrLn $ lvl' ++ "ch: 2; " ++ "lv: " ++ (show $ _tnId t) ++ " ; wr: " ++ (unpack $ _tnWord t)
              
              return $ f ++ [Node t []]

          
-- | Convert Tree structure to a sequantial list structure
-- 
toList :: TokenTree -> [(SnConllToken Text)]
toList t =
  toListAux t []
    where
      toListAux (Node x ts) xs = x : P.foldr toListAux xs ts
