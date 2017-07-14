{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}

module NLP.SyntaxNet.Types.ParseTree where

import Data.Text
import Data.Tree
import Data.Default

import NLP.SyntaxNet.Types.CoNLL

--------------------------------------------------------------------------------

-- | Single Node, containing parse word, its tag and label
-- 
data TreeNode =
  TreeNode
    { tnLevel  :: Int    -- ^ parsed word level
    , tnWord   :: Text   -- ^ parsed word
    , tnPosCp  :: PosCg  -- ^ Part-of-Speech (POS) coarse-grained (PRON, VERB, DET, NOUN, etc)
    , tnPosTag :: PosFg  -- ^ Part-of-Speech (POS) fine-grained   (PRP, VBD, DT, NN etc.) 
    } deriving (Eq, Show)

