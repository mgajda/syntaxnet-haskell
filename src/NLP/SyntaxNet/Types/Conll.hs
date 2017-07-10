module NLP.SyntaxNet.Types.ParseTree where

import Data.Text
import Data.Default

--------------------------------------------------------------------------------

-- | Row entry containing all relevant information
data CnllEntry =
  CnllEntry
    { cnId        :: Text -- ^ Index number
    , cnWord      :: Text -- ^ Parsed word
    , cnParam     :: Text -- ^ TODO: identify
    , cnPosCr     :: Text -- ^ Part-of-Speech (POS) coarse-grained (PRON, VERB, DET, NOUN, etc) 
    , cnPosFn     :: Text -- ^ Part-of-Speech (POS) fine-grained   (PRP, VBD, DT, NN etc.) 
    , cnParm2     :: Text -- ^ TODO: identify
    , cnShift     :: Int  -- ^
    , cnRel       :: Text -- ^ grammatical relationships between different words in the sentence
    , cnParam3    :: Text -- ^ TODO: identify
    , cnParam4    :: Text -- ^ TODO: identify     
    } deriving (Show, Eq)


-- TODO: 1. Add Cassava based reader 
--  
