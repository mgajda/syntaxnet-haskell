{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}

module NLP.SyntaxNet.Types.ParseTree where

import Data.Text
import Data.Tree
import Data.Default

--------------------------------------------------------------------------------

-- | Single Node, containing parse word, its tag and label
data TreeNode =
  TreeNode
    { tnWord   :: Text -- ^ parsed word
    , tnTag    :: Text -- ^ parsed word assigned Tag
    , tnLabel  :: Text -- ^ parsed word assigned Label
    , tnLevel  :: Int  -- ^ parsed word level
    } deriving (Eq, Show)

