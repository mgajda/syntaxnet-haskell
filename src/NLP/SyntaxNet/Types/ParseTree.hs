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
    } deriving (Eq, Show)

-- -- | Milti-way tree
-- data ParseTree =
--   ParseTree
--     { ptRoot   :: TreeNode    -- ^ labeled value
--     , ptForest :: [ParseTree] -- ^ zero or more child trees
--     } deriving (Eq, Show)

  
-- -- TODO: 1) Add Read instance, to read from SyntaxNet output
-- --       2) Add Show instance, for nice output 
