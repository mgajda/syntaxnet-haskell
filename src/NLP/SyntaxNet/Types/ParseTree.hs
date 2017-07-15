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

import NLP.SyntaxNet.Types.CoNLL

--------------------------------------------------------------------------------

-- |A 'Tree' of 'Token's
type TokenTree = Tree Token

-- |A 'Forest' of 'Token's
type TokenForest = [Tree Token]

-- |A Map of text values the appropriate tree
type TokenMap = M.Map Text TokenTree

drawTree' :: TokenTree -> String
drawTree'  = P.unlines . draw

-- | Neat 2-dimensional drawing of a forest.
drawForest' :: TokenForest -> String
drawForest'  = P.unlines . P.map drawTree'

draw :: TokenTree -> [String]
draw (Node tkn ts0) =
  P.lines ((unpack $ tnWord tkn) ++ " " ++ (show $ tnPosFG tkn) ++ " " ++ (showPP $ tnRel tkn)) ++ drawSubTrees ts0
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
forestToken (Node tkn subf) = tnWord tkn

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
fromList :: [Token] -> Maybe TokenTree
fromList (n:nodes) =
  Just $ Node n (fromListAux nodes []) 
    where fromListAux :: [Token]      -- ^ List of parsed Tokens
                      -> [Tree Token] -- ^ Building Forest
                      -> [Tree Token] -- ^ Final Forest
          fromListAux []         f = f
          fromListAux (t:ts:tss) f
            -- check current and next level
            | tnId t == tnId ts      = do
              -- next element on the same level, attach only              
              fromListAux (ts:tss) (f ++ [Node t []])
            | tnId t <  tnId ts      = do
              -- attach and move recursevly deep
              fromListAux (ts:tss) (f ++ [(Node t (fromListAux (ts:tss) [] ))])
            | tnId t >  tnId ts      = do
              -- next level is higher, attach only and move forest up              
              f ++ [Node t []]

-- | Debug version of fromList inside IO monad
-- 
fromList' :: [Token] -> IO (Maybe TokenTree)
fromList' (n:nodes) = do
  forest <- fromListAux nodes []
  return $ Just $ Node n forest 
    where fromListAux :: [Token]         -- ^ List of parsed Tokens
                      -> [Tree Token]    -- ^ Building Forest
                      -> IO [Tree Token] -- ^ Final Forest
          fromListAux []         f = return $ f
          fromListAux (t:ts:tss) f
            -- check current and next level
            | tnId t == tnId ts      = do
              -- next element on the same level, attach only
              let lvl = P.replicate (tnId t) '-'
                  lvl'= P.replicate (tnId t) ' '
              putStrLn $ lvl  
              putStrLn $ lvl' ++ "ch: 1; " ++ "lv: " ++ (show $ tnId t) ++ " ; wr: " ++ (unpack $ tnWord $ t)
              
              fromListAux (ts:tss) (f ++ [Node t []])
            | tnId t <  tnId ts      = do
              -- attach and move recursevly deep
              let lvl = P.replicate (tnId t) '-'
                  lvl'= P.replicate (tnId t) ' '
              putStrLn $ lvl  
              putStrLn $ lvl' ++ "ch: 2; " ++ "lv: " ++ (show $ tnId t) ++ " ; wr: " ++ (unpack $ tnWord $ t)

              sforest <- fromListAux (ts:tss) [] -- <
              fromListAux (ts:tss) (f ++ [(Node t sforest)])
            | tnId t >  tnId ts      = do
              -- next level is higher, attach only and move forest up
              let lvl = P.replicate (tnId t) '-'
                  lvl'= P.replicate (tnId t) ' '
              putStrLn $ lvl  
              putStrLn $ lvl' ++ "ch: 2; " ++ "lv: " ++ (show $ tnId t) ++ " ; wr: " ++ (unpack $ tnWord $ t)
              
              return $ f ++ [Node t []]

          
-- | Convert Tree structure to a sequantial list structure
-- 
toList :: Tree Token -> [Token]
toList t =
  toListAux t []
    where
      toListAux (Node x ts) xs = x : P.foldr toListAux xs ts
